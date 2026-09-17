import json
import os
from urllib import error as urllib_error
from urllib import request as urllib_request

LIGER_URL = os.environ.get("LIGER_URL", "http://localhost:8080")


class LigerError(RuntimeError):
    pass


def drs_provenance(syntax, drs, timeout=60):
    """POST /drs_provenance: tokens, condition spans and referent descriptions for one DRS."""
    payload = json.dumps({"syntax": syntax, "drs": drs}).encode("utf-8")
    req = urllib_request.Request(
        f"{LIGER_URL}/drs_provenance",
        data=payload,
        headers={"Content-Type": "application/json"},
        method="POST",
    )
    try:
        with urllib_request.urlopen(req, timeout=timeout) as response:
            return json.loads(response.read().decode("utf-8"))
    except urllib_error.HTTPError as e:
        raise LigerError(f"LiGER /drs_provenance returned {e.code}: {e.read().decode('utf-8', 'replace')}") from e
    except urllib_error.URLError as e:
        raise LigerError(f"LiGER /drs_provenance at {LIGER_URL} is unreachable: {e.reason}") from e
