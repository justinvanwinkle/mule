"""Weak references, compiled by Mule.

This provides the interface of CPython's ``_weakref`` extension module
(``Modules/_weakref.c`` and ``Objects/weakrefobject.c``). A ``ref`` holds a
runtime weak handle: the handle borrows its referent, becomes empty when the
referent is freed, and then calls the reference's callback with the
reference itself.
"""

from _mule_native import allocate_instance
from _mule_native import class_of
from _mule_native import get_attribute
from _mule_native import instance_field
from _mule_native import is_instance
from _mule_native import set_attribute
from _mule_native import set_instance_field
from _mule_native import weak_handle
from _mule_native import weak_owners
from _mule_native import weak_target
from _object import _builtin_types, builtin_function_or_method, code, function, method
from mule_types import final

__speed__ = 3
__mule_builtin_implementation__ = True


# Native builtin layouts do not acquire weak-reference support merely because
# their implementation classes are written in Python. User subclasses follow
# their own slot declarations, including inherited weak-reference slots.
_builtin_classes = _builtin_types()


@final
def _referenceable(ob):
    if isinstance(ob, (type, function, builtin_function_or_method, method, code, set, frozenset)):
        return True
    if isinstance(ob, (int, tuple, bytes)):
        return False
    for owner in type(ob).__mro__:
        if owner in _builtin_classes:
            continue
        namespace = vars(owner)
        if "__slots__" not in namespace:
            return True
        slots = namespace["__slots__"]
        if slots == "__weakref__" or not isinstance(slots, str) and "__weakref__" in slots:
            return True
    return False


class ref:
    """A reference that does not keep its referent alive."""

    __slots__ = ("_handle", "_hash", "__callback__")

    def __new__(cls, ob, callback=None):
        if not _referenceable(ob):
            raise TypeError(f"cannot create weak reference to '{type(ob).__name__}' object")
        self = allocate_instance(cls)
        self._handle = weak_handle(ob, callback, self)
        self._hash = None
        self.__callback__ = callback
        return self

    def __init__(self, ob, callback=None):
        pass

    def __call__(self):
        return weak_target(self._handle)

    def __hash__(self):
        if self._hash is None:
            target = self()
            if target is None:
                raise TypeError("weak object has gone away")
            self._hash = hash(target)
        return self._hash

    def __eq__(self, other):
        if not is_instance(other, ref):
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
        return f"<weakref; to '{class_of(target).__qualname__}'>"


ReferenceType = ref


@final
def _target(proxy):
    target = weak_target(instance_field(proxy, "_handle"))
    if target is None:
        raise ReferenceError("weakly-referenced object no longer exists")
    return target


class ProxyType:
    """Forwards attribute access to a weakly referenced object."""

    __slots__ = ("_handle", "__callback__")

    def __new__(cls, ob, callback=None):
        if not _referenceable(ob):
            raise TypeError(f"cannot create weak reference to '{type(ob).__name__}' object")
        self = allocate_instance(cls)
        set_instance_field(self, "_handle", weak_handle(ob, callback, self))
        set_instance_field(self, "__callback__", callback)
        return self

    def __getattribute__(self, name):
        return get_attribute(_target(self), name)

    def __setattr__(self, name, value):
        set_attribute(_target(self), name, value)

    def __bool__(self):
        return bool(_target(self))

    def __repr__(self):
        return f"<weakproxy; to '{class_of(_target(self)).__qualname__}'>"


class CallableProxyType(ProxyType):
    """A proxy for a callable referent."""

    __slots__ = ()

    def __call__(self, *args):
        return _target(self)(*args)


def proxy(ob, callback=None, /):
    if callable(ob):
        return CallableProxyType(ob, callback)
    return ProxyType(ob, callback)


def getweakrefcount(ob, /):
    return len(weak_owners(ob))


def getweakrefs(ob, /):
    return weak_owners(ob)


def _remove_dead_weakref(mapping, key, /):
    if key in mapping and mapping[key]() is None:
        del mapping[key]
