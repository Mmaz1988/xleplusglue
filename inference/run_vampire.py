import os
import subprocess
from fastapi import FastAPI
from pydantic import BaseModel



#put input into a temporary file and let vampire work on that
def bloodsucking(history,premises):
    newfilepath = 'inputpremises.p'
    with open(newfilepath, 'w') as file:
        file.write(history)
    with open(newfilepath, 'a') as file:
        file.write(premises)
    vampResult = subprocess.run(['bin/vampire', 'inputpremises.p'], stdout=subprocess.PIPE).stdout.decode('utf-8')
    #if "Termination reason: Satisfiable" in vampResult:
    #    history = history + premises
    resultList = [history, premises, vampResult]
    if os.path.exists("inputpremises.p"):
        os.remove("inputpremises.p")
    return resultList

#convert drs to fol to tptp and get the vampire output from that
def conversion(formula):
    #get prolog output of drs to fol
    betterformula = "drs2fol:printfol(" + formula + ",'folly.txt')."
    useProlog('[drs2fol].',betterformula)
    newfilepath = "folly.txt"
    newfol = open(newfilepath, 'r').read()
    print(newfol)
    #now get TPTP string from Prolog
    betterfol = "fol2tptp(" + newfol + ",'output.txt')."
    useProlog('[fol2tptp].',betterfol)
    filepath = "output.txt"
    data = open(filepath, 'r').read()
    print(data)
    # delete temp files if they haven't been already
    if os.path.exists("output.txt"):
        os.remove("output.txt")
    if os.path.exists("folly.txt"):
        os.remove("folly.txt")
    newstring = inputToFof(data)
    # get vampire to work on the final formula
    newblood = bloodsucking(newstring,"")
    return newblood

# manipulate a string to be readable by vampire
def inputToFof(inputstring):
    fofstring = inputstring.replace("input_formula","fof")
    return fofstring

# function for calling predicate with a specific knowledgebase and input
def useProlog(knowledgeBase,inputString):
    # get Prolog output
    prolog = subprocess.Popen(['swipl'], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    # Load the knowledgebase
    prolog.stdin.write(knowledgeBase + '\n')
    prolog.stdin.flush()
    # Execute a query
    # give Prolog the input query
    prolog.stdin.write(inputString + "\n")
    prolog.stdin.flush()
    # Exit Prolog
    prolog.stdin.write('halt.')
    prolog.stdin.flush()
    # Read Prolog output
    stdout, stderr = prolog.communicate()
    # Print output
    if stdout:
        print("Prolog Output:", stdout)
    if stderr:
        print("Prolog Errors:", stderr)




#what the input item should look like
class Item(BaseModel):
    axioms: str
    premises: str

#what a DRT input should look like
class Formula(BaseModel):
    formula: str

app = FastAPI()

@app.get("/")
def root():
    return {"test": "Hello World"}

# where 'pure' proving happens
@app.post("/prove")
def proving(request: Item):
    histresult = str(request.axioms)
    newresult = str(request.premises)
    return bloodsucking(histresult, newresult)

# where conversion to TPTP and then proving happens
@app.post("/convert")
def proving(request: Formula):
    discourse = str(request.formula)
    return conversion(discourse)


#Otter commands to potentially execute
#bin/vampire --mode model_check  eprover.p
#values for saturation_algorithm: fmb, otter
#bin/vampire --saturation_algorithm fmb  eprover.p

