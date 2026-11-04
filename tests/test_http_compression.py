"""gzip both ways for the browser-facing services (Redis/http_compression.py): a gzipped
request body reaches the endpoint plain, a large response goes out gzipped to a client that
accepts it, everything else passes through as before.

    lm/venv/bin/python tests/test_http_compression.py
"""
import gzip
import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from fastapi import FastAPI, Request, Response  # noqa: E402
from fastapi.middleware.cors import CORSMiddleware  # noqa: E402
from fastapi.testclient import TestClient  # noqa: E402

from Redis.http_compression import add_compression  # noqa: E402

app = FastAPI()
add_compression(app)
app.add_middleware(CORSMiddleware, allow_origins=["*"], allow_methods=["*"], allow_headers=["*"])

BIG = json.dumps({"readings": ["([x1],[dog(x1)])"] * 20000}).encode()


@app.put("/echo")
async def echo(request: Request):
    body = await request.body()
    return {"length": len(body), "first": body[:20].decode()}


@app.get("/big")
def big():
    return Response(content=BIG, media_type="application/json")


client = TestClient(app)


def check(condition, message):
    if not condition:
        raise AssertionError(message)
    print(f"  ok: {message}")


def test_a_gzipped_request_body_reaches_the_endpoint_plain():
    response = client.put("/echo", content=gzip.compress(BIG),
                          headers={"Content-Encoding": "gzip", "Content-Type": "application/json"})
    check(response.status_code == 200, "accepted")
    check(response.json() == {"length": len(BIG), "first": BIG[:20].decode()}, "the endpoint read the plain body")


def test_a_plain_request_body_is_untouched():
    response = client.put("/echo", content=b'{"a": 1}', headers={"Content-Type": "application/json"})
    check(response.json() == {"length": 8, "first": '{"a": 1}'}, "plain body as sent")


def test_a_body_that_is_not_gzip_is_refused_with_cors_headers():
    response = client.put("/echo", content=b"not gzip",
                          headers={"Content-Encoding": "gzip", "Origin": "http://localhost:4200"})
    check(response.status_code == 400, "400")
    check(response.headers.get("access-control-allow-origin") == "*", "CORS still answers, so the browser can read it")


def test_a_large_response_is_gzipped_for_a_client_that_accepts_it():
    response = client.get("/big", headers={"Accept-Encoding": "gzip"})
    check(response.headers.get("content-encoding") == "gzip", "gzipped")
    check(int(response.headers["content-length"]) < len(BIG) / 20, "far smaller on the wire")
    check(response.content == BIG, "the same JSON after decoding")
    plain = client.get("/big", headers={"Accept-Encoding": "identity"})
    check("content-encoding" not in plain.headers and plain.content == BIG, "plain for a client that does not")


def main():
    tests = [value for name, value in sorted(globals().items()) if name.startswith("test_")]
    for test in tests:
        print(f"\n{test.__name__}")
        test()
    print(f"\n{len(tests)} test(s) passed.")


if __name__ == "__main__":
    main()
