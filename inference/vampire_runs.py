"""Which Vampire batch run is active for each session, and how to stop it.

A run used to be identified by nothing but its session key, and cancelled through a flag on
its Redis progress record. Two things went wrong with that (2026-10-06):

* The client clears the progress record before every submission, and that cleared the
  cancel flag along with it. An aborted run never saw its cancel and kept going -- next to
  the run that replaced it, both writing one progress record and one results store, so the
  progress bar alternated between the two runs and results landed after the client had
  closed its report.
* The flag was only read between checks, so a cancel waited for the running Vampire process
  to finish its time limit.

The registry lives in this process: the service runs as a single uvicorn process and is the
only thing that starts Vampire batches. One run per session; a second is refused while the
first runs. Cancel is a per-run event, and it kills the run's Vampire process at once.
"""
import subprocess
import threading
from dataclasses import dataclass, field
from typing import Optional


class RunInProgress(Exception):
    def __init__(self, session_key: str, active_run_id: str):
        super().__init__(f"Vampire run {active_run_id} is still active for session {session_key}")
        self.session_key = session_key
        self.active_run_id = active_run_id


@dataclass
class RunHandle:
    session_key: str
    run_id: str
    cancel_event: threading.Event = field(default_factory=threading.Event)
    processes: set = field(default_factory=set)

    @property
    def cancelled(self) -> bool:
        return self.cancel_event.is_set()


_lock = threading.Lock()
_active: dict = {}
# Run ids cancelled before they started: an abort can reach the service before the run's own
# request does, and that run must stop as soon as it starts rather than run unseen.
_cancelled_before_start: set = set()
_current = threading.local()


def start_run(session_key: str, run_id: str) -> RunHandle:
    with _lock:
        active = _active.get(session_key)
        if active is not None:
            raise RunInProgress(session_key, active.run_id)
        handle = RunHandle(session_key, run_id)
        if run_id in _cancelled_before_start:
            _cancelled_before_start.discard(run_id)
            handle.cancel_event.set()
        _active[session_key] = handle
    _current.handle = handle
    return handle


def finish_run(handle: RunHandle) -> None:
    with _lock:
        if _active.get(handle.session_key) is handle:
            del _active[handle.session_key]
    if getattr(_current, "handle", None) is handle:
        _current.handle = None


def active_run_id(session_key: str) -> Optional[str]:
    with _lock:
        active = _active.get(session_key)
        return active.run_id if active else None


def active_session_keys() -> list:
    with _lock:
        return list(_active)


def cancel_run(session_key: str, run_id: Optional[str] = None) -> Optional[str]:
    """Cancel the session's active run -- only if it is `run_id`, when one is given -- and
    kill its Vampire process. Returns the cancelled run's id, or None if nothing matched (a
    named run that has not started yet is then cancelled the moment it does)."""
    with _lock:
        active = _active.get(session_key)
        if active is None or (run_id is not None and active.run_id != run_id):
            if run_id is not None:
                _cancelled_before_start.add(run_id)
            return None
        active.cancel_event.set()
        processes = list(active.processes)
    for process in processes:
        _kill(process)
    return active.run_id


def current_run() -> Optional[RunHandle]:
    return getattr(_current, "handle", None)


def register_process(process: subprocess.Popen) -> None:
    """Called by whoever starts a Vampire process on the run's thread, so a cancel can kill
    it. A process started after its run was cancelled is killed straight away."""
    handle = current_run()
    if handle is None:
        return
    with _lock:
        handle.processes.add(process)
        cancelled = handle.cancelled
    if cancelled:
        _kill(process)


def unregister_process(process: subprocess.Popen) -> None:
    handle = current_run()
    if handle is None:
        return
    with _lock:
        handle.processes.discard(process)


def _kill(process: subprocess.Popen) -> None:
    try:
        process.kill()
    except Exception:
        pass
