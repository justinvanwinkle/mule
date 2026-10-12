"""Weak references, compiled by Mule.

This provides the interface of CPython's ``_weakref`` extension module
(``Modules/_weakref.c`` and ``Objects/weakrefobject.c``). A ``ref`` holds a
runtime weak handle: the handle borrows its referent, becomes empty when the
referent is freed, and then calls the reference's callback with the
reference itself.
"""

from _mule_native import allocate_instance as _allocate_instance
from _mule_native import class_of as _class_of
from _mule_native import get_attribute as _get_attribute
from _mule_native import instance_field as _instance_field
from _mule_native import is_instance as _is_instance
from _mule_native import set_attribute as _set_attribute
from _mule_native import set_instance_field as _set_instance_field
from _mule_native import weak_handle as _weak_handle
from _mule_native import weak_owners as _weak_owners
from _mule_native import weak_target as _weak_target


class ref:
    """A reference that does not keep its referent alive."""

    __slots__ = ("_handle", "_hash", "__callback__")

    def __new__(cls, ob, callback=None):
        self = _allocate_instance(cls)
        self._handle = _weak_handle(ob, callback, self)
        self._hash = None
        self.__callback__ = callback
        return self

    def __init__(self, ob, callback=None):
        pass

    def __call__(self):
        return _weak_target(self._handle)

    def __hash__(self):
        if self._hash is None:
            target = self()
            if target is None:
                raise TypeError("weak object has gone away")
            self._hash = hash(target)
        return self._hash

    def __eq__(self, other):
        if not _is_instance(other, ref):
            return False
        mine, theirs = self(), other()
        if mine is None or theirs is None:
            return self is other
        return mine == theirs

    def __ne__(self, other):
        return not self.__eq__(other)

    def __repr__(self):
        target = self()
        if target is None:
            return "<weakref; dead>"
        return f"<weakref; to '{_class_of(target).__qualname__}'>"


ReferenceType = ref


def _target(proxy):
    target = _weak_target(_instance_field(proxy, "_handle"))
    if target is None:
        raise ReferenceError("weakly-referenced object no longer exists")
    return target


class ProxyType:
    """Forwards attribute access to a weakly referenced object."""

    __slots__ = ("_handle", "__callback__")

    def __new__(cls, ob, callback=None):
        self = _allocate_instance(cls)
        _set_instance_field(self, "_handle", _weak_handle(ob, callback, self))
        _set_instance_field(self, "__callback__", callback)
        return self

    def __getattribute__(self, name):
        return _get_attribute(_target(self), name)

    def __setattr__(self, name, value):
        _set_attribute(_target(self), name, value)

    def __bool__(self):
        return bool(_target(self))

    def __repr__(self):
        return f"<weakproxy; to '{_class_of(_target(self)).__qualname__}'>"


class CallableProxyType(ProxyType):
    """A proxy for a callable referent."""

    __slots__ = ()

    def __call__(self, *args):
        return _target(self)(*args)


def proxy(ob, callback=None, /):
    if _get_attribute(_class_of(ob), "__call__") is not None:
        return CallableProxyType(ob, callback)
    return ProxyType(ob, callback)


def getweakrefcount(ob, /):
    return len(_weak_owners(ob))


def getweakrefs(ob, /):
    return _weak_owners(ob)


def _remove_dead_weakref(mapping, key, /):
    if key in mapping and mapping[key]() is None:
        del mapping[key]
