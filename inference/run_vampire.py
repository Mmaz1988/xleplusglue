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


from vampire_call import generate_tptp_files, massacer, generate_svg_glyph, discourse_checks
from vampire_models import VampireRequest, VampireResponse, Context, Item, Check, VampireMultipleRequest, VampireMultipleResponse

logging.basicConfig(level=logging.DEBUG, format="%(asctime)s - %(levelname)s - %(message)s")
logger = logging.getLogger(__name__)

vampire_command = 'vampire'

# Retrieve the helper file path from the environment variable
BOXER = os.getenv("BOXER_PATH", "boxer")

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
def mergeDrs(firstOne,secondOne):
    logger.info("Merging DRSs: %s, %s", firstOne, secondOne)
    callToMerge = "presupDRT:printMerged(" + firstOne + "," + secondOne + ",'mergedRes.txt')."
    useProlog(f"[{os.path.join(BOXER,'presupDRT')}].",callToMerge)

    filepath = 'mergedRes.txt'
    mergedRes = open(filepath, 'r').read()
    if os.path.exists('mergedRes.txt'):
        os.remove('mergedRes.txt')
    return mergedRes

#print boxer output
def printDRS(Drs):
    if not os.path.exists("tmp"):
        os.makedirs("tmp", exist_ok=True)

    Drs = wrap_hyphenated_words(Drs)

    inputDrs = "printDrs:saveToFile(" + Drs + ",'tmp/boxing.txt')."
    useProlog(f"[{os.path.join(BOXER,'printDrs')}].",inputDrs)

    filepath = "tmp/boxing.txt"
    boxed = open(filepath, 'r').read()
    if os.path.exists("tmp/boxing.txt"):
        os.remove("tmp/boxing.txt")
    return boxed


def extract_drs_blocks(text):
    pattern = r"(drs\(.*?\))\n"
    matches = re.findall(pattern, text, re.DOTALL)  # Use DOTALL to match across multiple lines
    return matches


# manipulate a string to be readable by vampire
def inputToFof(inputstring):
    fofstring = inputstring.replace("input_formula","fof")
    return fofstring


#convert drs to fol to tptp and get the vampire output from that
def conversion(formula,tptp_type="fof"):
    logger.info("Converting formula to TPTP: %s", formula)
    if os.path.exists("tmp"):
        #delete contents if not empty
        for file in os.listdir("tmp"):
            os.remove(os.path.join("tmp", file))
        os.rmdir("tmp")
    os.makedirs("tmp", exist_ok=True)
    drs2fol_file = "tmp/folly.txt"
    #get prolog output of drs to fol
    betterformula = "drs2fol:printfol(" + formula + ",'"+ drs2fol_file +"')."
    logger.info("Calling Prolog to convert DRS to FOL: %s", betterformula)
    useProlog(f"[{os.path.join(BOXER,'drs2fol')}].",betterformula)
    logger.info(f"Loading knowledge base [{os.path.join(BOXER,'drs2fol')}].")

    newfol = open(drs2fol_file, 'r').read()
    logger.info("Function conversion generated following formula: " + newfol)
    #now get TPTP string from Prolog
    fof_file = "tmp/fof.txt"

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
    # delete tmp folders and content after use
    os.remove(drs2fol_file)
    os.remove(fof_file)
    os.rmdir("tmp")
    print("Generated TPTP formula: ", data)

    return newfol, wrap_hyphenated_words(data)


#what a DRT input should look like
#class Formula(BaseModel):
#    formula: str
#   newformula: str

def single_vampire_request(request):
    new_context = []
    new_active_indices = []
    current_checks = []

    # Delete tmp folder and all contents with shutil
    if os.path.exists("tmp"):
        shutil.rmtree("tmp")

    logger.info("Received Vampire Request: %s", request)
    readings = extract_drs_blocks(request.hypothesis)
    logger.debug("Readings extracted: %s", readings)

    # if logic_type is zero then use fof, otherwise use tff
    logic_type = "fof" if str(request.vampire_preferences['logic_type']) == '0' else "tff"
    logger.info("Using logic type: %s", logic_type)

    # use proof search based on model building in fof and mixed search in tff
    vampire_mode = []
    if logic_type == "fof":
        vampire_mode = ["-sa", "fmb"]
    elif logic_type == "tff":
        vampire_mode = ["--mode", "casc"]

    # CHeck if vampire preferences have max_duration with default 45 seconds
    max_duration = int(request.vampire_preferences.get('max_duration', 45))
    logger.info("Using Vampire mode: %s with max duration: %d seconds", vampire_mode, max_duration)

    hypotheses = []
    for reading in readings:
        prolog_hypothesis, fof_hypothesis = conversion(reading, tptp_type=logic_type)
        # fof_hypothesis = extract_fof(fof_hypothesis)
        context = Context(original=request.text, prolog_drs=reading, prolog_fol=prolog_hypothesis,
                              tptp=fof_hypothesis, box=printDRS(reading))
        hypotheses.append(context)

    if not request.context:
        new_context = hypotheses
        new_active_indices = [i for i in range(len(hypotheses))]
        logger.info("No context provided. Returning hypotheses.")

    else:
        logger.info("Context provided. Processing hypotheses.")
        logger.debug("First context: %s", request.context[0].tptp)

        active_contexts = request.context

        if request.active_indices:
            active_contexts = [ctx for i, ctx in enumerate(request.context) if i in request.active_indices]

        for ctx in active_contexts:
            for hypothesis in hypotheses:
                output_folder = "tmp/current/"
                generate_tptp_files(ctx.tptp, hypothesis.tptp, axioms=request.axioms, logic=logic_type,
                                        output_folder=output_folder)
                results = massacer(output_folder, mode=vampire_mode, timeout=max_duration, vampire_path="bin")
                logger.debug("Vampire Results: %s", results)

                consistent, informative, maxim_of_relevance = discourse_checks(data=results)
                logger.debug("Consistent: %s, Informative: %s, Relevant: %s",  consistent, informative, maxim_of_relevance)

                #Placeholder code
                if consistent and informative:
                    # Create new context
                    new_prolog = mergeDrs(ctx.prolog_drs,hypothesis.prolog_drs)
                    prolog_hypothesis, fof_hypothesis = conversion(new_prolog,tptp_type=logic_type)
                    # fof_hypothesis = extract_fof(fof_hypothesis)
                    context = Context(original=ctx.original + " " + hypothesis.original,
                                          prolog_drs=new_prolog, prolog_fol=prolog_hypothesis,
                                          tptp=fof_hypothesis, box=printDRS(new_prolog))
                    if context not in new_context:
                        new_context.append(context)
                        svg_output = generate_svg_glyph(results)
                        check = Check(glyph=svg_output, informative=informative, consistent=consistent, relevant= maxim_of_relevance)
                        current_checks.append(check)
                elif ctx not in new_context:
                    # Keep old context
                    new_context.append(ctx)
                    svg_output = generate_svg_glyph(results)
                    check = Check(glyph=svg_output, informative=informative, consistent=consistent, relevant=maxim_of_relevance)
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
    if os.path.exists("tmp"):
        shutil.rmtree("tmp")
    return result


# Define the Pydantic model for request validation
def multiple_vampire_request(request):

    # if logic_type is zero then use fof, otherwise use tff
    logic_type = "fof" if str(request.vampire_preferences['logic_type']) == '0' else "tff"
    logger.info("Using logic type: %s", logic_type)

    # use proof search based on model building in fof and mixed search in tff
    vampire_mode = []
    if logic_type == "fof":
        vampire_mode = ["-sa", "fmb"]
    elif logic_type == "tff":
        vampire_mode = ["--mode", "casc"]

    # CHeck if vampire preferences have max_duration with default 45 seconds
    max_duration = int(request.vampire_preferences.get('max_duration', 45))
    logger.info("Using Vampire mode: %s with max duration: %d seconds", vampire_mode, max_duration)

    # Inference id to Check
    results = {}
    inference_results = {}

    for id, nli_item in request.nli_items.items():
        output_folder = "tmp/current/"
        # merge premises into one drs

        if len(nli_item['premises']) > 1:
            while len(nli_item['premises']) > 1:
                #premise semantics

                #if first is a string extract drs, if first is a list do nothing
                logger.info("Current premises to merge: %s and %s1 ", nli_item['premises'][0], nli_item['premises'][1])
                first = extract_drs_blocks(nli_item['premises'][0]) if isinstance(nli_item['premises'][0], str) else nli_item['premises'][0]
                second = extract_drs_blocks(nli_item['premises'][1]) if isinstance(nli_item['premises'][1], str) else nli_item['premises'][1]

                merged_list = []

                if not request.pruning:
                    for reading1 in first:
                        for reading2 in second:
                            merged = mergeDrs(reading1, reading2)
                            if merged not in merged_list:
                                merged_list.append(merged)
                else:
                    merged = mergeDrs(first[0], second[0])
                    merged_list.append(merged)

                #make merged_list first item of nli_items and ignore second item
                nli_item['premises'] = [merged_list] + nli_item['premises'][2:]

        else:
            nli_item['premises'] = [extract_drs_blocks(nli_item['premises'][0])]

        premise_semantics = nli_item['premises'][0]
        logger.info("Premise semantics: %s", premise_semantics)

        # This might require fixing if there are multiple hyptheses
        hypothesis_semantics = []
        for item in nli_item['hypothesis']:
            hypothesis_semantics += extract_drs_blocks(item)

        logger.info("Hypothesis semantics: %s", hypothesis_semantics)

        inference_checks = []

        for sem1 in premise_semantics:
            for sem2 in hypothesis_semantics:

                prolog_premises, fof_premises = conversion(sem1, tptp_type=logic_type)
                prolog_hypothesis, fof_hypothesis = conversion(sem2, tptp_type=logic_type)

                generate_tptp_files(fof_premises, fof_hypothesis, axioms=nli_item['axioms'], logic=logic_type,
                                    output_folder=output_folder)
                results = massacer(output_folder, mode=vampire_mode, timeout=max_duration, vampire_path="bin")
                logger.debug("Vampire Results: %s", results)

                consistent, informative, maxim_of_relevance = discourse_checks(data=results)
                logger.debug("Consistent: %s, Informative: %s, Relevant: %s",  consistent, informative, maxim_of_relevance)

                svg_output = generate_svg_glyph(results)
                check = Check(glyph=svg_output, informative=informative, consistent=consistent, relevant= maxim_of_relevance)

                inference_checks.append(check)

        inference_results[id] = inference_checks

    result = VampireMultipleResponse(results=inference_results)

    if os.path.exists("tmp"):
        shutil.rmtree("tmp")

    return result


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



