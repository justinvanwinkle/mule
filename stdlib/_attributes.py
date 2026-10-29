"""Attribute builtins and ``type``, compiled by Mule.

Attribute access itself is the runtime's lookup protocol; these functions
apply it to names computed at run time, following CPython's
``Python/bltinmodule.c``.
"""

__speed__ = 3
__mule_builtin_implementation__ = True

from _mule_native import delete_attribute
from _mule_native import get_attribute
from _mule_native import get_attribute_or
from _mule_native import set_attribute

# Compiler-provided physical primitives are not host Python globals.
# ruff: noqa: F821


class _Missing:
    """The absent default of ``getattr``; None is a valid default."""


_MISSING = _Missing()


def _require_name(name):
    if not _text_storage(name):
        raise TypeError(f"attribute name must be string, not '{_type_name(name)}'")


def getattr(obj, name, default=_MISSING, /):
    _require_name(name)
    if default is _MISSING:
        return get_attribute(obj, name)
    # A lookup the runtime fails returns the default without raising; an
    # AttributeError raised by compiled code still arrives here.
    try:
        return get_attribute_or(obj, name, default)
    except AttributeError:
        return default


def hasattr(obj, name, /):
    _require_name(name)
    try:
        return get_attribute_or(obj, name, _MISSING) is not _MISSING
    except AttributeError:
        return False


def setattr(obj, name, value, /):
    _require_name(name)
    set_attribute(obj, name, value)


def delattr(obj, name, /):
    _require_name(name)
    delete_attribute(obj, name)
