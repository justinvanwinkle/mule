"""Python 3.14 monitoring declarations and inactive tool registration.

Compiler event probes are not available yet. Activating a nonempty event mask
raises explicitly instead of accepting callbacks that cannot be delivered.
"""

from _object import code as _Code
from math.integer import _index


class _Events:
    NO_EVENTS = 0
    PY_START = 1
    PY_RESUME = 2
    PY_RETURN = 4
    PY_YIELD = 8
    CALL = 16
    LINE = 32
    INSTRUCTION = 64
    JUMP = 128
    BRANCH_LEFT = 256
    BRANCH_RIGHT = 512
    STOP_ITERATION = 1024
    RAISE = 2048
    EXCEPTION_HANDLED = 4096
    PY_UNWIND = 8192
    PY_THROW = 16384
    RERAISE = 32768
    C_RETURN = 65536
    C_RAISE = 131072
    BRANCH = 262144


events = _Events()
DEBUGGER_ID = 0
COVERAGE_ID = 1
PROFILER_ID = 2
OPTIMIZER_ID = 5
DISABLE = object()
MISSING = object()


class _Tool:
    def __init__(self, identity):
        self.identity = identity
        self.name = None
        self.callbacks = {}

    def claim(self, name):
        if not isinstance(name, str):
            raise ValueError("tool name must be a str")
        if self.name is not None:
            raise ValueError(f"tool {self.identity} is already in use")
        self.name = name

    def clear(self):
        self.callbacks.clear()

    def free(self):
        self.clear()
        self.name = None

    def require_owner(self):
        if self.name is None:
            raise ValueError(f"tool {self.identity} is not in use")

    def register(self, event, callback):
        event = _index(event)
        if event <= 0 or event & (event - 1):
            raise ValueError("The callback can only be set for one event at a time")
        if event >= 1 << 19:
            raise ValueError(f"invalid event {event}")
        previous = self.callbacks.get(event)
        if callback is None:
            self.callbacks.pop(event, None)
        else:
            self.callbacks[event] = callback
        return previous


_tools = tuple(_Tool(identity) for identity in range(6))


def _tool(identity):
    identity = _index(identity)
    if not 0 <= identity < 6:
        raise ValueError(f"invalid tool {identity} (must be between 0 and 5)")
    return _tools[identity]


def use_tool_id(tool_id, name, /):
    _tool(tool_id).claim(name)


def get_tool(tool_id, /):
    return _tool(tool_id).name


def clear_tool_id(tool_id, /):
    _tool(tool_id).clear()


def free_tool_id(tool_id, /):
    _tool(tool_id).free()


def register_callback(tool_id, event, func, /):
    return _tool(tool_id).register(event, func)


def get_events(tool_id, /):
    _tool(tool_id)
    return events.NO_EVENTS


def _mask(mask):
    mask = _index(mask)
    if not 0 <= mask < 1 << 19:
        raise ValueError(f"invalid event set {mask & 0xFFFFFFFF:#x}")
    return mask


def _activate(mask):
    if mask:
        raise NotImplementedError(
            "Mule compiler monitoring event probes are not implemented"
        )


def set_events(tool_id, event_set, /):
    tool = _tool(tool_id)
    mask = _mask(event_set)
    tool.require_owner()
    if mask & (events.C_RETURN | events.C_RAISE) and not mask & events.CALL:
        raise ValueError("cannot set C_RETURN or C_RAISE events independently")
    _activate(mask)


def _code(code):
    if not isinstance(code, _Code):
        raise TypeError("code must be a code object")


def get_local_events(tool_id, code, /):
    _tool(tool_id)
    _code(code)
    return events.NO_EVENTS


def set_local_events(tool_id, code, event_set, /):
    tool = _tool(tool_id)
    _code(code)
    mask = _mask(event_set)
    tool.require_owner()
    if mask & ~((1 << 10) - 1 | events.BRANCH):
        raise ValueError(f"invalid local event set {mask:#x}")
    _activate(mask)


def restart_events():
    # No sites can be disabled while all event masks are empty.
    return None
