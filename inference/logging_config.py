"""The one place the Vampire service configures logging.

Every other module keeps its own ``getLogger(__name__)`` and only ever emits
records; levels and handlers are decided here, called once from the entrypoint
(``vampire_endpoints``). Previously three modules called ``basicConfig`` and
whichever imported first pinned the root logger at DEBUG for the whole process,
which made the level look per-module configurable when it was not.

Two environment variables:

- ``LOG_LEVEL`` (default ``INFO``) — the root level.
- ``LOG_DIR`` (optional) — when set, ``session_log_file`` writes a per-run file
  named after the same session key the tmp directories use
  (``last_session-<uuid>``), so a log file and its proof files line up.
"""

import logging
import os
import re
import threading
from contextlib import contextmanager

DEFAULT_LOG_LEVEL = "INFO"
LOG_FORMAT = "%(asctime)s - %(levelname)s - %(name)s - %(message)s"

_handler_lock = threading.Lock()


def resolve_level(level_name=None):
    """Turn a level name into a level number, falling back to the default."""
    raw = (level_name or os.getenv("LOG_LEVEL") or DEFAULT_LOG_LEVEL).strip().upper()
    level = logging.getLevelName(raw)
    return level if isinstance(level, int) else logging.INFO


def configure_logging():
    """Set the root level and console format. Safe to call more than once."""
    level = resolve_level()
    logging.basicConfig(level=level, format=LOG_FORMAT)
    # basicConfig does nothing when handlers already exist (uvicorn installs its
    # own), so set the level explicitly as well.
    logging.getLogger().setLevel(level)
    return level


def session_log_path(session_key):
    log_dir = os.getenv("LOG_DIR")
    if not log_dir:
        return None
    safe_key = re.sub(r"[^A-Za-z0-9_.-]+", "_", session_key or "session")
    return os.path.join(log_dir, f"{safe_key}.log")


@contextmanager
def session_log_file(session_key):
    """Mirror this run's log records into ``LOG_DIR/<session_key>.log``.

    A no-op when ``LOG_DIR`` is unset, so the default stays console-only. The
    handler sits on the root logger for the duration of the run: overlapping
    requests therefore each capture the other's lines, which is a fair trade for
    not threading a logger through every call site.
    """
    path = session_log_path(session_key)
    if not path:
        yield None
        return

    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
    handler = logging.FileHandler(path, encoding="utf-8")
    handler.setFormatter(logging.Formatter(LOG_FORMAT))
    root = logging.getLogger()
    with _handler_lock:
        root.addHandler(handler)
    try:
        yield path
    finally:
        with _handler_lock:
            root.removeHandler(handler)
        handler.close()
