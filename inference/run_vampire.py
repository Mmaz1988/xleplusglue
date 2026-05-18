import os
import subprocess
from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
import re
import traceback
import time
import shutil
import logging
import uuid


from vampire_call import generate_tptp_files, massacer, generate_svg_glyph, discourse_checks
from vampire_models import VampireRequest, VampireResponse, Context, Item, Check, VampireMultipleRequest
from vampire_redis_calls import merge_and_save_last_session, load_vampire_progress, save_vampire_progress

logging.basicConfig(level=logging.DEBUG, format="%(asctime)s - %(levelname)s - %(message)s")
logger = logging.getLogger(__name__)

vampire_command = 'vampire'

# Retrieve the helper file path from the environment variable
BOXER = os.getenv("BOXER_PATH", "boxer")


class VampireCancelled(Exception):
    pass


def _make_vampire_tmp_root(session_key: str) -> str:
    safe_session_key = re.sub(r"[^A-Za-z0-9_.-]+", "_", session_key or "session")
    return os.path.join("tmp", f"{safe_session_key}-{uuid.uuid4().hex}")


def _cleanup_tmp_root(tmp_root=None):
    if tmp_root:
        shutil.rmtree(tmp_root, ignore_errors=True)


def _vampire_session_key(request):
    return getattr(request, "session_key", "last_session")


def _is_vampire_cancel_requested(session_key):
    try:
        progress = load_vampire_progress(session_key)
    except Exception:
        return False

    return bool(progress.get("cancelRequested")) or progress.get("state") == "cancel_requested"


def _update_vampire_progress(session_key, **updates):
    try:
        progress = load_vampire_progress(session_key)
    except Exception:
        progress = {"sessionKey": session_key}

    progress.update(updates)
    progress.setdefault("completedItemIds", [])
    progress.setdefault("changedItemIds", [])
    progress.setdefault("itemResults", {})
    progress.setdefault("itemCount", 0)
    progress.setdefault("proofCount", 0)
    progress.setdefault("totalItemCount", 0)
    progress["sessionKey"] = session_key
    save_vampire_progress(session_key, progress)


def _ensure_not_cancelled(session_key):
    if _is_vampire_cancel_requested(session_key):
        raise VampireCancelled()

# function for calling predicate with a specific knowledgebase and input
def useProlog(knowledgeBase, inputString):
    """
    Runs a Prolog knowledge base with a given input query.

    Improvements:
    - Uses `communicate()` instead of multiple `write()` calls.
    - Handles errors properly.
    - Ensures the Prolog process terminates correctly.
    - Uses a timeout to prevent hanging.
    """
    try:
        # Start Prolog
        with subprocess.Popen(['swipl'],
                              stdin=subprocess.PIPE,
                              stdout=subprocess.PIPE,
                              stderr=subprocess.PIPE,
                              text=True) as prolog:

            # Construct the full Prolog input as a single string
            prolog_input = f"{knowledgeBase}\n{inputString}\nhalt.\n"

            # Send the input and get the output
            stdout, stderr = prolog.communicate(prolog_input, timeout=10)

            # Handle output
            if stderr:
                print("Prolog Errors:", stderr.strip())
            return stdout.strip() if stdout else None

    except subprocess.TimeoutExpired:
        print("Error: Prolog execution timed out.")
        return None
    except Exception as e:
        print("Error:", str(e))
        return None

#merge two DRSs
def mergeDrs(firstOne, secondOne, tmp_root="tmp"):
    logger.info("Merging DRSs: %s, %s", firstOne, secondOne)
    os.makedirs(tmp_root, exist_ok=True)
    merged_file = os.path.join(tmp_root, "mergedRes.txt")
    callToMerge = "presupDRT:printMerged(" + firstOne + "," + secondOne + ",'" + merged_file + "')."
    useProlog(f"[{os.path.join(BOXER,'presupDRT')}].",callToMerge)

    filepath = merged_file
    mergedRes = open(filepath, 'r').read()

    pattern = r"\d+? ((?:drs|merge)\(.*?\))\n"
    matches = re.findall(pattern, mergedRes, re.DOTALL)  # Use DOTALL to match across multiple lines
    logger.info("Extracted merged Drs: %s", matches)

    if os.path.exists(merged_file):
        os.remove(merged_file)
    return matches

#print boxer output
def printDRS(Drs, tmp_root="tmp"):
    if not os.path.exists(tmp_root):
        os.makedirs(tmp_root, exist_ok=True)

    # logger.info("Trying to print DRS: %s", Drs)

    Drs = wrap_hyphenated_words(Drs)

    boxing_file = os.path.join(tmp_root, "boxing.txt")
    inputDrs = "printDrs:saveToFile(" + Drs + ",'" + boxing_file + "')."
    useProlog(f"[{os.path.join(BOXER,'printDrs')}].",inputDrs)

    filepath = boxing_file
    boxed = open(filepath, 'r').read()

    # logger.info("Generated following DRS: %s", boxed)

    if os.path.exists(boxing_file):
        os.remove(boxing_file)
    return boxed


def extract_drs_blocks(text):
    pattern = r"((?:drs|merge|alfa)\(.*?\))\n"
    logger.info("pattern=%r", pattern)
    matches = re.findall(pattern, text, re.DOTALL)  # Use DOTALL to match across multiple lines
    logger.info("Extracted DRS blocks: %s", matches)
    return matches


# manipulate a string to be readable by vampire
def inputToFof(inputstring):
    fofstring = inputstring.replace("input_formula","fof")
    return fofstring


#convert drs to fol to tptp and get the vampire output from that
def conversion(formula, tptp_type="fof", tmp_root="tmp"):
    logger.info("Converting formula to TPTP: %s", formula)
    _cleanup_tmp_root(tmp_root)
    os.makedirs(tmp_root, exist_ok=True)
    #if formula contains app or merge, resolve first.

    formulas = []

    resolve_file = os.path.join(tmp_root, "unpure.txt")
    if "app(" in formula or "merge(" in formula:
        logger.info("Resolving application or merge in formula: %s", formula)
        resolve_input = "presupDRT:resolve2file(" + formula + ",'" + resolve_file + "')."
        useProlog(f"[{os.path.join(BOXER,'presupDRT')}].",resolve_input)
        #stip digits and whitespaces in the beginning (.e.g 1 drs(...))
        formula = open(resolve_file, 'r').read()
        pattern = r"\d+? (.*?)\n"
        formulas = re.findall(pattern, formula, re.DOTALL)  # Use DOTALL to match across multiple lines
        logger.info("Resolved formula: %s", formulas)
    else:
        formulas = [formula]



    new_fols = []
    prologs = []
    #get prolog output of drs to fol
    for formula in formulas:
        drs2fol_file = os.path.join(tmp_root, "folly.txt")
        betterformula = "drs2fol:printfol(" + formula + ",'"+ drs2fol_file +"')."
        logger.info("Calling Prolog to convert DRS to FOL: %s", betterformula)
        useProlog(f"[{os.path.join(BOXER,'drs2fol')}].",betterformula)
        logger.info(f"Loading knowledge base [{os.path.join(BOXER,'drs2fol')}].")

        newfol = open(drs2fol_file, 'r').read()
        logger.info("Function conversion generated following formula: " + newfol)
        #now get TPTP string from Prolog
        fof_file = os.path.join(tmp_root, "fof.txt")

        # tptp conversion file
        tptp_prolog = ""
        betterfol = ""
        if tptp_type == "fof":
            betterfol = "fol2fof(" + newfol + ",'" + fof_file + "')."
            tptp_prolog = "fol2fof"
        else:
            betterfol = "fol2tff(" + newfol + ",'" + fof_file + "')."
            tptp_prolog = "fol2tff"
        # betterfol = "fol2tptp(" + newfol + ",'" +fof_file+"')."

        logger.info("Calling Prolog to convert FOL to TPTP: %s", betterfol)
        useProlog(f"[{os.path.join(BOXER,tptp_prolog)}].",betterfol)

        data = open(fof_file, 'r').read()
        data = inputToFof(data)
        data = wrap_hyphenated_words(data)

        if newfol not in new_fols:
            new_fols.append(newfol)

        if data not in prologs:
            prologs.append(data)

        # delete tmp folders and content after use
        os.remove(drs2fol_file)
        os.remove(fof_file)

    os.remove(resolve_file) if os.path.exists(resolve_file) else None

    _cleanup_tmp_root(tmp_root)
    print(f'Generated TPTP formulas: %s', new_fols)

    return new_fols, prologs


def run_vampire_batch(ctx_tptp, hypothesis_tptp, axioms, logic_type, vampire_mode, max_duration, output_folder):
    proof_files = generate_tptp_files(ctx_tptp, hypothesis_tptp, axioms=axioms, logic=logic_type,
                                      output_folder=output_folder)
    results = massacer(output_folder, mode=vampire_mode, timeout=max_duration, vampire_path="bin")

    timeout_count = sum(1 for result in results if result.get("Termination Reason") == "Timeout")
    if vampire_mode == ["-sa", "fmb"] and results and timeout_count > len(results) / 2:
        logger.info("Timeout majority (%d/%d); retrying with casc", timeout_count, len(results))
        proof_files = generate_tptp_files(ctx_tptp, hypothesis_tptp, axioms=axioms, logic=logic_type,
                                          output_folder=output_folder)
        results = massacer(output_folder, mode=["--mode", "casc"], timeout=max_duration, vampire_path="bin")

    return proof_files, results


#what a DRT input should look like
#class Formula(BaseModel):
#    formula: str
#   newformula: str

def single_vampire_request(request):
    tmp_root = _make_vampire_tmp_root(_vampire_session_key(request))
    new_context = []
    new_active_indices = []
    current_checks = []

    # Delete tmp folder and all contents with shutil
    _cleanup_tmp_root(tmp_root)

    logger.info("Received Vampire Request: %s", request)
    readings = extract_drs_blocks(request.hypothesis)
    logger.debug("Readings extracted: %s", readings)

    # if logic_type is zero then use fof, otherwise use tff
    logic_type = "fof" if str(request.vampire_preferences['logic_type']) == '0' else "tff"
    model_building = True if request.vampire_preferences['model_building'] == True  else False
    logger.info("Logic type=%s", logic_type)

    # use proof search based on model building in fof and mixed search in tff
    vampire_mode = []
    if logic_type == "fof" and model_building:
        vampire_mode = ["-sa", "fmb"]
    else:
        vampire_mode = ["--mode", "casc"]

    # CHeck if vampire preferences have max_duration with default 45 seconds
    max_duration = int(request.vampire_preferences.get('max_duration', 45))
    logger.info("Using Vampire mode: %s with max duration: %d seconds", vampire_mode, max_duration)

    hypotheses = []
    for reading in readings:
        prolog_hypotheses, fof_hypotheses = conversion(reading, tptp_type=logic_type, tmp_root=tmp_root)
        for prolog_hypothesis, fof_hypothesis in zip(prolog_hypotheses, fof_hypotheses):
            # fof_hypothesis = extract_fof(fof_hypothesis)
            context = Context(original=request.text, prolog_drs=reading, prolog_fol=prolog_hypothesis,
                              tptp=fof_hypothesis, box=printDRS(reading, tmp_root=tmp_root))
            hypotheses.append(context)

    if not request.context:
        new_context = hypotheses
        new_active_indices = [i for i in range(len(hypotheses))]
        logger.info("No context provided; returning hypotheses")

    else:
        logger.info("Context provided. Processing hypotheses.")
        logger.debug("First context: %s", request.context[0].tptp)

        active_contexts = request.context

        if request.active_indices:
            active_contexts = [ctx for i, ctx in enumerate(request.context) if i in request.active_indices]

        for ctx in active_contexts:
            for hypothesis in hypotheses:
                output_folder = os.path.join(tmp_root, "current")
                proof_files, results = run_vampire_batch(
                    ctx.tptp,
                    hypothesis.tptp,
                    request.axioms,
                    logic_type,
                    vampire_mode,
                    max_duration,
                    output_folder,
                )
                logger.debug("Vampire Results: %s", results)

                consistent, informative, maxim_of_relevance = discourse_checks(data=results)
                logger.debug("Consistent: %s, Informative: %s, Relevant: %s",  consistent, informative, maxim_of_relevance)

                #Placeholder code
                if consistent and informative:
                    # Create new context
                    new_prolog = mergeDrs(ctx.prolog_drs,hypothesis.prolog_drs, tmp_root=tmp_root)
                    for prolog in new_prolog:
                        # Should be singleton lists because mergeDrs above already resolves ambiguities
                        prolog_hypotheses, fof_hypotheses = conversion(prolog,tptp_type=logic_type, tmp_root=tmp_root)
                        prolog_hypothesis = prolog_hypotheses[0]
                        fof_hypothesis = fof_hypotheses[0]
                        # fof_hypothesis = extract_fof(fof_hypothesis)
                        context = Context(original=ctx.original + " " + hypothesis.original,
                                          prolog_drs=prolog, prolog_fol=prolog_hypothesis,
                                          tptp=fof_hypothesis, box=printDRS(prolog, tmp_root=tmp_root))
                        if context not in new_context:
                            new_context.append(context)
                            svg_output = generate_svg_glyph(results)
                            check = Check(glyph=svg_output, informative=informative, consistent=consistent, relevant= maxim_of_relevance, proof_files=proof_files)
                            current_checks.append(check)
                elif ctx not in new_context:
                    # Keep old context
                    new_context.append(ctx)
                    svg_output = generate_svg_glyph(results)
                    check = Check(glyph=svg_output, informative=informative, consistent=consistent, relevant=maxim_of_relevance, proof_files=proof_files)
                    current_checks.append(check)

        new_active_indices = [i for i in range(len(new_context))]


    # Create singleton list consisting of first hypothesis
    if request.pruning:
        if len(new_context) > 0:
            new_context = [new_context[0]]
        else:
            new_context = [hypotheses[0]]
        new_active_indices = [0]

    context_checks_mapping = {}
    # Create context_checks_mapping
    for i, check in enumerate(current_checks):
        context_checks_mapping[i] = check

    logger.info(f"Returning Vampire Response: {new_context}, {new_active_indices}, {context_checks_mapping}")

    result = VampireResponse(context=new_context,
                                 active_indices=new_active_indices,
                                 context_checks_mapping=context_checks_mapping)
    _cleanup_tmp_root(tmp_root)
    return result


# Define the Pydantic model for request validation
def multiple_vampire_request(request):
    session_key = _vampire_session_key(request)
    tmp_root = _make_vampire_tmp_root(session_key)

    # if logic_type is zero then use fof, otherwise use tff
    logic_type = "fof" if str(request.vampire_preferences['logic_type']) == '0' else "tff"
    model_building = True if request.vampire_preferences['model_building'] == True  else False
    logger.info("Logic type=%s", logic_type)

    # use proof search based on model building in fof and mixed search in tff
    vampire_mode = []
    if logic_type == "fof" and model_building:
        vampire_mode = ["-sa", "fmb"]
    else:
        vampire_mode = ["--mode", "casc"]

    # CHeck if vampire preferences have max_duration with default 45 seconds
    max_duration = int(request.vampire_preferences.get('max_duration', 45))
    logger.info("Using Vampire mode: %s with max duration: %d seconds", vampire_mode, max_duration)

    # Inference id to Check
    inference_results = {}

    def snapshot_progress(state: str, active_item_id=None):
        _update_vampire_progress(
            session_key,
            state=state,
            cancelRequested=(state == "cancelled"),
            activeItemId=active_item_id,
            itemCount=len(inference_results),
            proofCount=sum(len(check_list) for check_list in inference_results.values()),
            completedItemIds=list(inference_results.keys()),
            itemResults={key: [item.dict() for item in checks] for key, checks in inference_results.items()},
            totalItemCount=len(request.nli_items),
        )

    snapshot_progress("running")

    try:
        for id, nli_item in request.nli_items.items():
            _ensure_not_cancelled(session_key)
            output_folder = os.path.join(tmp_root, "current")
            # merge premises into one drs

            if len(nli_item['premises']) > 1:
                while len(nli_item['premises']) > 1:
                    _ensure_not_cancelled(session_key)
                    logger.info("Current premises to merge: %s and %s1 ", nli_item['premises'][0], nli_item['premises'][1])
                    first = extract_drs_blocks(nli_item['premises'][0]) if isinstance(nli_item['premises'][0], str) else nli_item['premises'][0]
                    second = extract_drs_blocks(nli_item['premises'][1]) if isinstance(nli_item['premises'][1], str) else nli_item['premises'][1]

                    merged_list = []

                    if not request.pruning:
                        for reading1 in first:
                            for reading2 in second:
                                _ensure_not_cancelled(session_key)
                                merged = mergeDrs(reading1, reading2, tmp_root=tmp_root)
                                for drs in merged:
                                    if drs not in merged_list:
                                        merged_list.append(drs)
                                        logger.info("Updated merged list: %s", merged_list)
                    else:
                        merged = mergeDrs(first[0], second[0], tmp_root=tmp_root)
                        if merged:
                            merged_list.append(merged[0])

                    nli_item['premises'] = [merged_list] + nli_item['premises'][2:]

            else:
                readings = extract_drs_blocks(nli_item['premises'][0])
                nli_item['premises'] = [[readings[0]]] if readings else [[]]

            premise_semantics = nli_item['premises'][0]
            logger.info("Premise semantics: %s", premise_semantics)

            # This might require fixing if there are multiple hyptheses
            hypothesis_semantics = []
            for item in nli_item['hypothesis']:
                _ensure_not_cancelled(session_key)
                hypothesis_semantics += extract_drs_blocks(item)

            logger.info("Hypothesis semantics: %s", hypothesis_semantics)

            inference_checks = []

            #Efficiency addition so that each formula only has to be converted once
            p_conversions = {}
            h_conversions = {}

            for i,sem in enumerate(premise_semantics):
                _ensure_not_cancelled(session_key)
                prolog_premises, fof_premises = conversion(sem, tptp_type=logic_type, tmp_root=tmp_root)
                p_conversions[f'p_{i}'] = (prolog_premises, fof_premises)

            for j,sem in enumerate(hypothesis_semantics):
                _ensure_not_cancelled(session_key)
                prolog_hypotheses, fof_hypotheses = conversion(sem, tptp_type=logic_type, tmp_root=tmp_root)
                h_conversions[f'h_{j}'] = (prolog_hypotheses, fof_hypotheses)

            logger.info("Premise conversions: %s", p_conversions)
            logger.info("Hypothesis conversions: %s", h_conversions)

            for p_key in p_conversions.keys():
                for h_key in h_conversions.keys():
                    _ensure_not_cancelled(session_key)

                    prolog_premises, fof_premises = p_conversions[p_key]
                    prolog_hypotheses, fof_hypotheses = h_conversions[h_key]

                    for fof_premise in fof_premises:
                        for fof_hypothesis in fof_hypotheses:
                            _ensure_not_cancelled(session_key)
                            logger.info("Processing premise: %s and hypothesis: %s", fof_premise, fof_hypothesis)
                            proof_files, results = run_vampire_batch(
                                fof_premise,
                                fof_hypothesis,
                                nli_item['axioms'],
                                logic_type,
                                vampire_mode,
                                max_duration,
                                output_folder,
                            )
                            logger.debug("Vampire Results: %s", results)

                            consistent, informative, maxim_of_relevance = discourse_checks(data=results)
                            logger.debug("Consistent: %s, Informative: %s, Relevant: %s",  consistent, informative, maxim_of_relevance)

                            svg_output = generate_svg_glyph(results)
                            check = Check(glyph=svg_output, informative=informative, consistent=consistent, relevant=maxim_of_relevance, proof_files=proof_files)

                            inference_checks.append(check)
                            inference_results[id] = inference_checks
                            try:
                                merge_and_save_last_session(
                                    session_key,
                                    {"results": {id: [item.dict() for item in inference_checks]}},
                                )
                            except Exception:
                                logger.warning("Unable to persist last_session to Redis CRUD service", exc_info=True)

                            snapshot_progress("running", id)

            inference_results[id] = inference_checks
            snapshot_progress("running", id)

        snapshot_progress("completed")

        return {"status": "ok"}
    except VampireCancelled:
        snapshot_progress("cancelled")
        return {"status": "cancelled", "results": {key: [item.dict() for item in checks] for key, checks in inference_results.items()}}
    finally:
        _cleanup_tmp_root(tmp_root)


"""
Utilities
"""

def wrap_hyphenated_words(text):
    pattern = r'\b[\w\d]+-[\w\d]+\b'
    return re.sub(pattern, lambda m: f"'{m.group(0)}'", text)



#Otter commands to potentially execute
#bin/vampire --mode model_check  eprover.p
#values for saturation_algorithm: fmb, otter
#bin/vampire --saturation_algorithm fmb  eprover.p

# def extract_fof(text):
#     pattern = r"fof\(\w+,\w+,(.*?)\)\s*"
#     match = re.search(pattern, text)
#     return match.group(1)
