#!/bin/bash

# Allow the user to specify the Python command (default: python3)
PYTHON_CMD=${1:-python3}

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
$PYTHON_CMD generate_tptp.py tptp_testsuite_adj.csv generated_testfiles_adj --axioms_file degree_axioms.txt --logic tff

# Run the second script to process with Vampire
$PYTHON_CMD vampire_subprocess.py generated_testfiles_adj --mode "--mode casc" --vampire_path "" --result "results.csv"

# Deactivate virtual environment
deactivate
