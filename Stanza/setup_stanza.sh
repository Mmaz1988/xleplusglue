#!/bin/bash

set -e  # Exit on any error

# Step 1: Create models directory if it doesn't exist
mkdir -p models

# Step 2: Download English model zip file if not already downloaded
EN_MODEL_URL="https://huggingface.co/stanfordnlp/stanza-en/resolve/main/models/default.zip"
EN_MODEL_ZIP="models/default.zip"

if [ ! -f "$EN_MODEL_ZIP" ]; then
    echo "Downloading English model..."
    curl -L -o "$EN_MODEL_ZIP" "$EN_MODEL_URL"
else
    echo "English model already downloaded."
fi

# Step 3: Unzip model into models/en
EN_MODEL_DIR="models/en"
if [ ! -d "$EN_MODEL_DIR" ]; then
    echo "Unzipping English model..."
    mkdir -p "$EN_MODEL_DIR"
    unzip -q "$EN_MODEL_ZIP" -d "$EN_MODEL_DIR"
else
    echo "English model already unzipped."
fi

# Step 4: Download resources JSON
RESOURCES_URL="https://raw.githubusercontent.com/stanfordnlp/stanza-resources/main/resources_1.10.0.json"
RESOURCES_JSON="models/resources.json"

if [ ! -f "$RESOURCES_JSON" ]; then
    echo "Downloading resources.json..."
    curl -L -o "$RESOURCES_JSON" "$RESOURCES_URL"
else
    echo "resources.json already downloaded."
fi

# remove default.zip
rm -f "$EN_MODEL_ZIP"

echo "Setup complete."
