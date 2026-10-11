"""Attribute builtins and ``type``, compiled by Mule.

Attribute access itself is the runtime's lookup protocol; these functions
apply it to names computed at run time, following CPython's
``Python/bltinmodule.c``. Three-argument ``type`` builds a class from a
mapping the way a class statement builds one from its body's namespace.
"""

from _mule_native import delete_attribute as _delete_attribute
from _mule_native import get_attribute as _get_attribute
from _mule_native import new_class as _new_class
from _mule_native import set_attribute as _set_attribute

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
        return _get_attribute(obj, name)
    try:
        return _get_attribute(obj, name)
    except AttributeError:
        return default


def hasattr(obj, name, /):
    _require_name(name)
    try:
        _get_attribute(obj, name)
    except AttributeError:
        return False
    return True


def setattr(obj, name, value, /):
    _require_name(name)
    _set_attribute(obj, name, value)


def delattr(obj, name, /):
    _require_name(name)
    _delete_attribute(obj, name)


def type(*args):
    if len(args) == 1:
        owner = _raised_class(args[0])
        if owner is None or owner is args[0]:
            raise TypeError(
                f"type() of '{_type_name(args[0])}' values is not supported yet"
            )
        return owner
    if len(args) != 3:
        raise TypeError("type() takes 1 or 3 arguments")
    name, bases, namespace = args
    _require_name(name)
    if not _tuple_storage(bases):
        raise TypeError("type.__new__() argument 2 must be tuple")
    if len(bases) > 1:
        raise TypeError("multiple inheritance is not supported yet")
    names, values = [], []
    for key in namespace:
        _require_name(key)
        names.append(key)
        values.append(namespace[key])
    return _new_class(
        name, bases[0] if bases else None, _storage_tuple(names), _storage_tuple(values)
    )
