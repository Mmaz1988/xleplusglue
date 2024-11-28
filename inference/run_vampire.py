import io
import sys
from contextlib import redirect_stdout
import subprocess
from fastapi import FastAPI
from pydantic import BaseModel
from pyswip import Prolog


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
    return resultList

#convert fol to tptp
def conversion(formula):
    #command = "[fol2tptp]."
    #subprocess.run('swipl')
    #convResult = subprocess.run(['swipl','fol2tptp.pl',formula],stdout=subprocess.PIPE)
    #Prolog.consult("fol2tptp.pl")
    #convResult = str(dict(Prolog.query(formula)))
    #return convResult
    #def run_prolog():
    # Start Prolog
#   subprocess
    print("Processing input: " + formula)
    prolog = subprocess.Popen(['swipl'], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    # Load the knowledgebase
    #prolog.stdin.write('X = Mia.\n')
    prolog.stdin.write('[fol2tptp].\n')
    prolog.stdin.flush()
    # Execute a query
    prolog.stdin.write(formula + "\n")
    prolog.stdin.flush()
    # Exit Prolog
    prolog.stdin.write('halt.\n')
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

@app.post("/prove")
def proving(request: Item):
    histresult = str(request.axioms)
    newresult = str(request.premises)
    return bloodsucking(histresult, newresult)

@app.post("/convert")
def proving(request: Formula):
    discourse = str(request.formula)
    return conversion(discourse)


#Otter commands to potentially execute
#bin/vampire --mode model_check  eprover.p
#values for saturation_algorithm: fmb, otter
#bin/vampire --saturation_algorithm fmb  eprover.p

