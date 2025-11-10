# Create models directory if it doesn't exist
$modelsPath = "models"
if (-Not (Test-Path $modelsPath)) {
    New-Item -ItemType Directory -Path $modelsPath | Out-Null
}

# Download English model
$enModelUrl = "https://huggingface.co/stanfordnlp/stanza-en/resolve/main/models/default.zip"
$enModelZip = "$modelsPath\default.zip"

if (-Not (Test-Path $enModelZip)) {
    Write-Output "Downloading English model..."
    Invoke-WebRequest -Uri $enModelUrl -OutFile $enModelZip
} else {
    Write-Output "English model already downloaded."
}

# Unzip
$enModelDir = "$modelsPath\en"
if (-Not (Test-Path $enModelDir)) {
    Write-Output "Unzipping English model..."
    Expand-Archive -Path $enModelZip -DestinationPath $enModelDir
} else {
    Write-Output "English model already unzipped."
}

# Download resources.json
$resourcesUrl = "https://raw.githubusercontent.com/stanfordnlp/stanza-resources/main/resources_1.10.0.json"
$resourcesJson = "$modelsPath\resources.json"

if (-Not (Test-Path $resourcesJson)) {
    Write-Output "Downloading resources.json..."
    Invoke-WebRequest -Uri $resourcesUrl -OutFile $resourcesJson
} else {
    Write-Output "resources.json already downloaded."
}

# Remove default.zip
Remove-Item -Path $enModelZip -Force

Write-Output "Setup complete."
