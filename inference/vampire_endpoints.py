import os
from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
import re
import traceback
import time
import shutil
import logging

from vampire_call import generate_tptp_files, massacer, generate_svg_glyph, discourse_checks
from vampire_models import VampireRequest, VampireMultipleRequest
from  run_vampire import single_vampire_request, multiple_vampire_request

app = FastAPI()
# Enable CORS for all origins (Modify for security in production)
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # Change this to your frontend domain for security (e.g., "http://localhost:4200")
    allow_credentials=True,
    allow_methods=["*"],  # Allow all HTTP methods (GET, POST, OPTIONS, etc.)
    allow_headers=["*"],  # Allow all headers
)

logging.basicConfig(level=logging.DEBUG, format="%(asctime)s - %(levelname)s - %(message)s")
logger = logging.getLogger(__name__)

@app.get("/")
def root():
    return {"test": "Hello World"}

@app.post("/vampire_request")
def process_vampire_request_single(request: VampireRequest):
    """
    API endpoint to process vampireRequest.
    """
    try:
        return single_vampire_request(request)

    except Exception as e:
        logger.error("Exception occurred", exc_info=True)
        if os.path.exists("tmp"):
            shutil.rmtree("tmp")
        raise HTTPException(status_code=500, detail=f"Internal Server Error: {str(e)}")


@app.post("/vampire_multiple_request")
def process_vampire_request_multiple(request: VampireMultipleRequest):
    """
    API endpoint to process vampireRequest.
    """
    try:
        logger.info("Received request: " + str(request))

        return multiple_vampire_request(request)

    except Exception as e:
        logger.error("Exception occurred", exc_info=True)
        if os.path.exists("tmp"):
            shutil.rmtree("tmp")
        raise HTTPException(status_code=500, detail=f"Internal Server Error: {str(e)}")