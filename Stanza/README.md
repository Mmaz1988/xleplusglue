# Usage

If not already there create a directory named `models` in the project root directory

Download the English model from [Stanza English model](https://huggingface.co/stanfordnlp/stanza-en/blob/main/models/default.zip) 

Download the resources file from [Stanza resources](https://github.com/stanfordnlp/stanza-resources/blob/main/resources_1.10.0.json)

Create a models folder in the Stanza directory and place the downloaded files there. The structure should look like this:

```
Stanza/
 |-- models/
     |-- en <-- contents of the English model in the default.zip file
     |-- resources.json <-- contents of the resources_1.10.0.json file

```

```

If starting standalone, create a python environment, install the requiremenmts (e.g. pip install -r requirements.txt) and start the api using `uvicorn main:app --host 0.0.0.0 --port {PORT}` with `{PORT}` beeing the chosen available port for the api.

If started using docker(-compose) simply run `docker compose up --build` to build and start the api. The default port will be 8002.

To use the dependency parser send a json object containing the sentence and the language (abbreviation as used for stanza, visit: https://stanfordnlp.github.io/stanza/available_models.html for available languages) e.g.

    {
        "sentence": "The sun is shining.",
        "language": "en"
    }

to the endpoint `/parse`.