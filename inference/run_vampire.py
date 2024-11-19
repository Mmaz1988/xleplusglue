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
    #newHistory = history + premises
    #global globalHistory = newHistory
    resultList = [history, premises, vampResult]
    return resultList

#what the input item should look like
class Item(BaseModel):
    axioms: str
    premises: str

app = FastAPI()

@app.get("/")
def root():
    return {"test": "Hello World"}

@app.post("/prove")
def proving(request: Item):
    histresult = str(request.axioms)
    newresult = str(request.premises)
    return bloodsucking(histresult, newresult)


#Otter commands to potentially execute
#bin/vampire --mode model_check  eprover.p
#values for saturation_algorithm: fmb, otter
#bin/vampire --saturation_algorithm fmb  eprover.p

