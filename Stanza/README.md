# Usage

If not already there create a directory named `models` in the project root directory

If starting standalone, create a python environment, install the requiremenmts (e.g. pip install -r requirements.txt) and start the api using `uvicorn main:app --host 0.0.0.0 --port {PORT}` with `{PORT}` beeing the chosen available port for the api.

If started using docker(-compose) simply run `docker compose up --build` to build and start the api. The default port will be 8002.

To use the dependency parser send a json object containing the sentence and the language (abbreviation as used for stanza, visit: https://stanfordnlp.github.io/stanza/available_models.html for available languages) e.g.

    {
        "sentence": "The sun is shining.",
        "language": "en"
    }

to the endpoint `/parse`.