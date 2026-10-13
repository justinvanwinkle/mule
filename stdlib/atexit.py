"""Register functions to be called when a program is closing down.

Compiled programs cannot run Python callbacks from the runtime's exit
path, so this module records registrations and runs them from a
single exit hook the driver installs; until then, ``_run_exitfuncs``
may be called directly, as CPython's own shutdown code does.
"""

__all__ = ["register", "unregister", "_clear", "_run_exitfuncs", "_ncallbacks"]

_callbacks = []


def register(func, *args, **kwargs):
    """Register ``func`` to run at program exit with the given arguments.

    Returns ``func`` so it can be used as a decorator."""
    _callbacks.append((func, args, kwargs))
    return func


def unregister(func):
    """Remove every registration of ``func``; unknown functions are ignored."""
    remaining = []
    for entry in _callbacks:
        if entry[0] is not func:
            remaining.append(entry)
    _callbacks[:] = remaining


def _clear():
    """Clear all registered callbacks."""
    del _callbacks[:]


def _ncallbacks():
    """Return the number of registered callbacks."""
    return len(_callbacks)


def _run_exitfuncs():
    """Run registered callbacks in reverse registration order."""
    while _callbacks:
        func, args, kwargs = _callbacks.pop()
        func(*args, **kwargs)
