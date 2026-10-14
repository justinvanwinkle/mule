"""Manual cycle collection over Mule's independent reference-counted heap."""

from _mule_native import gc_collect as _collect
from _mule_native import gc_objects as _objects
from _mule_native import gc_referents as _referents
from _mule_native import gc_tracked as _tracked
from math.integer import _index

callbacks = []
garbage = []
_collecting = False


class _Unraisable:
    def __init__(self, error, callback):
        self.exc_type = type(error)
        self.exc_value = error
        self.exc_traceback = error.__traceback__
        self.err_msg = 'Exception ignored while calling GC callback ' + repr(callback)
        self.object = None


def _notify(phase, info):
    for callback in tuple(callbacks):
        try:
            callback(phase, info)
        except BaseException as error:
            import sys
            try:
                sys.unraisablehook(_Unraisable(error, callback))
            except BaseException:
                pass


def _generation(value):
    value = _index(value)
    if value < 0 or value > 2:
        raise ValueError("invalid generation")
    return value


def collect(generation=2):
    global _collecting
    generation = _generation(generation)
    if _collecting:
        return 0
    _collecting = True
    try:
        _notify('start', {'generation': generation, 'collected': 0, 'uncollectable': 0})
        result = _collect()
        _notify('stop', {'generation': generation, 'collected': result, 'uncollectable': 0})
        return result
    finally:
        _collecting = False


def get_objects(generation=None):
    if generation is not None:
        _generation(generation)
    return _objects()


def get_referents(*objects):
    result = []
    for obj in objects:
        result.extend(_referents(obj))
    return result


def is_tracked(obj, /):
    return _tracked(obj)


def disable():
    pass


def isenabled():
    return False


def enable():
    raise NotImplementedError("automatic collection requires compiler allocation checks")
