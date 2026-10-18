"""Attribute builtins and ``type``, compiled by Mule.

Attribute access itself is the runtime's lookup protocol; these functions
apply it to names computed at run time, following CPython's
``Python/bltinmodule.c``.
"""

from _mule_native import delete_attribute
from _mule_native import get_attribute
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
    try:
        return get_attribute(obj, name)
    except AttributeError:
        return default


def hasattr(obj, name, /):
    _require_name(name)
    try:
        get_attribute(obj, name)
    except AttributeError:
        return False
    return True


def setattr(obj, name, value, /):
    _require_name(name)
    set_attribute(obj, name, value)


def delattr(obj, name, /):
    _require_name(name)
    delete_attribute(obj, name)
