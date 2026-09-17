import os
import sys

# Tests use a small multilingual model with XLM-R's tokenizer that is already in the local
# Hugging Face cache, so they run offline and fast; the service default stays XLM-R base.
os.environ.setdefault("LM_MODEL", "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2")
os.environ.setdefault("HF_HUB_OFFLINE", "1")

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
