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
from vampire_models import VampireRequest, VampireResponse, Context, Item, Check

logging.basicConfig(level=logging.DEBUG, format="%(asctime)s - %(levelname)s - %(message)s")
logger = logging.getLogger(__name__)

vampire_command = 'vampire'


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


# manipulate a string to be readable by vampire
def inputToFof(inputstring):
    fofstring = inputstring.replace("input_formula","fof")
    return fofstring


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

#print boxer output
def printDRS(Drs):
    if not os.path.exists("tmp"):
        os.makedirs("tmp", exist_ok=True)

    inputDrs = "printDrs:saveToFile(" + Drs + ",'tmp/boxing.txt')."
    useProlog(f"[{os.path.join(BOXER,'printDrs')}].",inputDrs)

    filepath = "tmp/boxing.txt"
    boxed = open(filepath, 'r').read()
    if os.path.exists("tmp/boxing.txt"):
        os.remove("tmp/boxing.txt")
    return boxed


#what a DRT input should look like
#class Formula(BaseModel):
#    formula: str
#   newformula: str

app = FastAPI()
# Enable CORS for all origins (Modify for security in production)
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # Change this to your frontend domain for security (e.g., "http://localhost:4200")
    allow_credentials=True,
    allow_methods=["*"],  # Allow all HTTP methods (GET, POST, OPTIONS, etc.)
    allow_headers=["*"],  # Allow all headers
)


# Retrieve the helper file path from the environment variable
BOXER = os.getenv("BOXER_PATH", "boxer")

@app.get("/")
def root():
    return {"test": "Hello World"}

# where 'pure' proving happens
@app.post("/prove")
def proving(request: Item):
    histresult = str(request.discourseSoFar)
    newresult = str(request.axioms)
    return bloodsucking(histresult, newresult)

# where conversion to TPTP and then proving happens
@app.post("/convert")
def proving(request: Item):
    discourse = str(request.discourseSoFar)
    newdiscourse = str(request.axioms)
    convertedForm = conversion(discourse)
    convertedNewform = conversion(newdiscourse)
    resultList = bloodsucking(convertedForm,convertedNewform)
    # check if vampire concludes satisfiability/consistency
    if "Termination reason: Satisfiable" in resultList[2]:
        newDRS = mergeIfTrue(discourse,newdiscourse)
        resultList.append(newDRS)
        boxedDrs = printDRS(newDRS)
        resultList.append(boxedDrs)
    return resultList




# Define the Pydantic model for request validation

@app.post("/vampire_request")
def process_vampire_request(request: VampireRequest):
    """
    API endpoint to process vampireRequest.
    """

    new_context = []
    new_active_indices = []
    current_checks = []

    # Delete tmp folder and all contents with shutil
    if os.path.exists("tmp"):
        shutil.rmtree("tmp")

    try:
        logger.info("Received Vampire Request: %s", request)
        readings = extract_drs_blocks(request.hypothesis)
        logger.debug("Readings extracted: %s", readings)

        # if logic_type is zero then use fof, otherwise use tff
        logic_type = "fof" if request.vampire_preferences['logic_type'] == 0 else "tff"
        logger.info("Using logic type: %s", logic_type)

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
                    results = massacer(output_folder, mode=["-sa", "fmb"], timeout=7, vampire_path="bin")
                    logger.debug("Vampire Results: %s", results)

                    consistent, informative = discourse_checks(data=results)
                    logger.debug("Consistent: %s, Informative: %s", consistent, informative)

                    #Placeholder code
                    if consistent and informative:
                        # Create new context
                        new_prolog = mergeDrs(ctx.prolog_drs,hypothesis.prolog_drs)
                        prolog_hypothesis, fof_hypothesis = conversion(new_prolog)
                        # fof_hypothesis = extract_fof(fof_hypothesis)
                        context = Context(original=ctx.original + " " + hypothesis.original,
                                          prolog_drs=new_prolog, prolog_fol=prolog_hypothesis,
                                          tptp=fof_hypothesis, box=printDRS(new_prolog))
                        if context not in new_context:
                            new_context.append(context)
                            svg_output = generate_svg_glyph(results)
                            check = Check(glyph=svg_output, informative=informative, consistent=consistent)
                            current_checks.append(check)
                    elif ctx not in new_context:
                        # Keep old context
                        new_context.append(ctx)
                        svg_output = generate_svg_glyph(results)
                        check = Check(glyph=svg_output, informative=informative, consistent=consistent)
                        current_checks.append(check)

            new_active_indices = [i for i in range(len(new_context))]


        # Create singleton list consisting of first hypothesis
        if request.pruning:
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

    except Exception as e:
        logger.error("Exception occurred", exc_info=True)
        if os.path.exists("tmp"):
            shutil.rmtree("tmp")
        raise HTTPException(status_code=500, detail=f"Internal Server Error: {str(e)}")

def extract_drs_blocks(text):
    pattern = r"(drs\(.*?\))\n\n"
    matches = re.findall(pattern, text, re.DOTALL)  # Use DOTALL to match across multiple lines
    return matches

def extract_fof(text):
    pattern = r"fof\(\w+,\w+,(.*?)\)\s*"
    match = re.search(pattern, text)
    return match.group(1)

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
    logger.info("Function conversion generated following formula: ", newfol)
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
    return newfol, data









#Otter commands to potentially execute
#bin/vampire --mode model_check  eprover.p
#values for saturation_algorithm: fmb, otter
#bin/vampire --saturation_algorithm fmb  eprover.p

