# Setup

If not already there create a directory named `models` in the `Stanza` directory

Download the English model from [Stanza English model](https://huggingface.co/stanfordnlp/stanza-en/blob/main/models/default.zip) 

Download the resources file from [Stanza resources](https://github.com/stanfordnlp/stanza-resources/blob/main/resources_1.10.0.json)

Create a models folder in the Stanza directory and place the downloaded files there. The structure should look like this:

```
Stanza/
 |-- models/
     |-- en <-- contents of the English model in the default.zip file
     |-- resources.json <-- contents of the resources_1.10.0.json file

```

Alternatively, you can use the `setup_stanza.sh` script provided in the `Stanza` directory to automate the setup process. 
This script will download the necessary files and set up the directory structure for you. The script needs to be executed from the terminal or command line.
If it is not executable, you can make it executable by running:

```bash
chmod +x setup_stanza.sh
```

Then run the script:

```bash     
./setup_stanza.sh
```