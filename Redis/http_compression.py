"""gzip in both directions for the services the browser talks to (Redis API :8083, Vampire
:8082), so a large session or document crosses Docker Desktop's port forwarding as a few MB.

A stress-test bank was ~150 MB of JSON in one response (regression_document:stress-test:n0
alone 92 MB) and an autosave wrote a 92 MB document back; transfers like these preceded
Docker Desktop VM network stalls (virtio_net TX timeout, 2026-11-04). The JSON compresses
~50x (n0: 92 MB -> 1.7 MB).

Responses: Starlette's GZipMiddleware, for clients that send Accept-Encoding: gzip (browsers
do; the services' own urllib calls to each other do not, so those stay plain). Requests: a
body sent with Content-Encoding: gzip is decompressed before the endpoint reads it.

Used by both images: Redis/ is copied whole into the redis image, and this file alone into
the vampire image (Docker/Dockerfile-vampire), so it imports nothing of either.
"""
import gzip
import zlib

from starlette.middleware.gzip import GZipMiddleware

# Below this a response is sent as it is: compressing a status poll gains nothing.
MINIMUM_SIZE = 4096
# zlib level 5: within a few percent of 9's size on this JSON, at a fraction of the time.
COMPRESS_LEVEL = 5


class GzipRequestMiddleware:
    """Decompresses a request body sent with Content-Encoding: gzip; any other request
    passes through untouched. A body that is not valid gzip is refused with 400."""

    def __init__(self, app):
        self.app = app

    async def __call__(self, scope, receive, send):
        if scope["type"] != "http" or not _gzip_encoded(scope):
            await self.app(scope, receive, send)
            return
        chunks = []
        more = True
        while more:
            message = await receive()
            if message["type"] == "http.disconnect":
                return
            chunks.append(message.get("body", b""))
            more = message.get("more_body", False)
        try:
            body = gzip.decompress(b"".join(chunks))
        except (OSError, EOFError, zlib.error) as error:
            await _refuse(send, f"request body is not valid gzip: {error}")
            return
        headers = [(name, value) for name, value in scope["headers"]
                   if name.lower() not in (b"content-encoding", b"content-length")]
        headers.append((b"content-length", str(len(body)).encode()))
        delivered = False

        async def replay():
            nonlocal delivered
            if delivered:
                return await receive()
            delivered = True
            return {"type": "http.request", "body": body, "more_body": False}

        await self.app(dict(scope, headers=headers), replay, send)


def add_compression(app):
    """Request decompression and response compression. Call it before adding CORS, so CORS
    stays outermost and a refused gzip body still carries the CORS headers the browser
    needs to read the 400."""
    app.add_middleware(GzipRequestMiddleware)
    app.add_middleware(GZipMiddleware, minimum_size=MINIMUM_SIZE, compresslevel=COMPRESS_LEVEL)


def _gzip_encoded(scope):
    for name, value in scope.get("headers", []):
        if name.lower() == b"content-encoding":
            return value.strip().lower() == b"gzip"
    return False


async def _refuse(send, detail):
    body = ('{"detail": "%s"}' % detail.replace('"', "'")).encode()
    await send({"type": "http.response.start", "status": 400,
                "headers": [(b"content-type", b"application/json"),
                            (b"content-length", str(len(body)).encode())]})
    await send({"type": "http.response.body", "body": body})
