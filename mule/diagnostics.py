"""Opt-in compiler progress and elapsed-time diagnostics on stderr."""

import logging
import sys
from contextlib import contextmanager
from contextvars import ContextVar
from time import perf_counter

logger = logging.getLogger("mule")
_depth = ContextVar("mule_diagnostic_depth", default=0)


def configure(verbosity: int) -> None:
    """Configure the command's diagnostics without affecting program stdout."""
    level = logging.WARNING if not verbosity else logging.INFO
    if verbosity >= 2:
        level = logging.DEBUG
    logger.setLevel(level)
    handler = logging.StreamHandler(sys.stderr)
    handler.setFormatter(logging.Formatter("mule: %(message)s"))
    logger.handlers.clear()
    logger.addHandler(handler)
    logger.propagate = False


@contextmanager
def phase(label: str, *arguments, level: int = logging.INFO):
    """Report entry and completion, including failures, only when enabled."""
    if not logger.isEnabledFor(level):
        yield
        return
    label = label % arguments if arguments else label
    indent = "  " * _depth.get()
    logger.log(level, "%s%s started", indent, label)
    started = perf_counter()
    token = _depth.set(_depth.get() + 1)
    try:
        yield
    except BaseException:
        logger.log(level, "%s%s failed after %.3f s", indent, label, perf_counter() - started)
        raise
    else:
        logger.log(level, "%s%s completed in %.3f s", indent, label, perf_counter() - started)
    finally:
        _depth.reset(token)
