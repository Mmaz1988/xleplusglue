#!/bin/bash

# Allow the user to specify the Python command (default: python3)
PYTHON_CMD=${1:-python3}

# Define script and argument variables for easy modification
TPTP_CSV="tptp_testsuite_adj.csv"
OUTPUT_DIR="generated_testfiles_adj"
AXIOMS_FILE="degree_axioms.txt"
LOGIC_TYPE="tff"

VAMPIRE_MODE="--mode casc"
VAMPIRE_PATH=""
RESULT_FILE="results.csv"

# Check if virtual environment already exists
if [ ! -d "venv" ]; then
    echo "Creating virtual environment..."
    $PYTHON_CMD -m venv venv
    NEW_VENV_CREATED=true
else
    echo "Virtual environment already exists. Skipping creation."
    NEW_VENV_CREATED=false
fi

# Activate the virtual environment
source venv/bin/activate

# Install dependencies only if venv was just created and requirements.txt exists
if [ "$NEW_VENV_CREATED" = true ] && [ -f "requirements.txt" ]; then
    echo "Installing dependencies..."
    pip install -r requirements.txt
else
    echo "Skipping dependency installation."
fi

# Run the first script to generate TPTP files
$PYTHON_CMD generate_tptp.py "$TPTP_CSV" "$OUTPUT_DIR" --axioms_file "$AXIOMS_FILE" --logic "$LOGIC_TYPE"

# Run the second script to process with Vampire
$PYTHON_CMD vampire_subprocess.py "$OUTPUT_DIR" --mode "$VAMPIRE_MODE" --vampire_path "$VAMPIRE_PATH" --result "$RESULT_FILE"

# Deactivate virtual environment
deactivate
