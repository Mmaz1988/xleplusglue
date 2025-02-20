#!/bin/bash

# Allow the user to specify the Python command (default: python3)
PYTHON_CMD=${1:-python3}

# Define script and argument variables for easy modification
TPTP_CSV="tptp_testsuite_adj.csv"
OUTPUT_DIR="generated_testfiles_speed"
AXIOMS_FILE="degree_axioms_speed.txt"
LOGIC_TYPE="tff"

# Modes: --mode casc, --mode casc_sat, -sa fmb
VAMPIRE_MODE="--mode casc_sat"
VAMPIRE_PATH=""
RESULT_FILE="results_speed.csv"

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

# Construct arguments dynamically to avoid empty strings
TPTP_ARGS=("$TPTP_CSV" "$OUTPUT_DIR" --logic "$LOGIC_TYPE")
if [ -n "$AXIOMS_FILE" ]; then
    TPTP_ARGS+=(--axioms_file "$AXIOMS_FILE")
fi

# Corrected array debugging
echo "TPTP args: ${TPTP_ARGS[@]}"

# Run the first script to generate TPTP files
$PYTHON_CMD generate_tptp.py "${TPTP_ARGS[@]}"

# Construct Vampire arguments dynamically
VAMPIRE_ARGS=("$OUTPUT_DIR" --mode "$VAMPIRE_MODE" --result "$RESULT_FILE")
if [ -n "$VAMPIRE_PATH" ]; then
    VAMPIRE_ARGS+=(--vampire_path "$VAMPIRE_PATH")
fi

# Corrected array debugging
echo "Vampire args: ${VAMPIRE_ARGS[@]}"

# Run the second script to process with Vampire
$PYTHON_CMD vampire_subprocess.py "${VAMPIRE_ARGS[@]}"

# Deactivate virtual environment
deactivate
