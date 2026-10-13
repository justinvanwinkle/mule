"""Builtin descriptors and ``super``, compiled by Mule.

Class attribute lookup applies the descriptor protocol: a class attribute
whose class defines ``__get__`` computes the value it produces, and one that
also defines ``__set__`` or ``__delete__`` takes precedence over instance
fields. These classes supply Python's builtin descriptors on top of that
protocol, following CPython's ``Objects/funcobject.c`` (``staticmethod``,
``classmethod``), ``Objects/descrobject.c`` (``property``) and
``Objects/typeobject.c`` (``super``). The native imports are physical class
storage operations; the rules here are ordinary compiled Python.
"""

from _mule_native import bind_attribute as _bind_attribute
from _mule_native import bound_method as _bound_method
from _mule_native import class_mro as _class_mro
from _mule_native import class_lookup_or_null as _class_lookup_or_null
from _mule_native import class_lookup as _class_lookup
from _mule_native import class_of as _class_of
from _mule_native import instance_field as _instance_field
from _mule_native import is_class as _is_class
from _mule_native import is_subclass as _is_subclass


class staticmethod:
    """A function stored in a class and returned without binding."""

    def __init__(self, function):
        self.__func__ = function
        self.__wrapped__ = function

    def __get__(self, instance, owner=None):
        return self.__func__


class classmethod:
    """A function stored in a class and bound to the class it is read through."""

    def __init__(self, function):
        self.__func__ = function
        self.__wrapped__ = function

    def __get__(self, instance, owner=None):
        if owner is None:
            owner = _class_of(instance)
        return _bound_method(self.__func__, owner)


class property:
    """A data descriptor computing an attribute through accessor functions."""

    def __init__(self, fget=None, fset=None, fdel=None, doc=None):
        self.fget = fget
        self.fset = fset
        self.fdel = fdel
        self.__doc__ = doc

    def __get__(self, instance, owner=None):
        if instance is None:
            return self
        if self.fget is None:
            raise AttributeError("property has no getter")
        return self.fget(instance)

    def __set__(self, instance, value):
        if self.fset is None:
            raise AttributeError("property has no setter")
        self.fset(instance, value)

    def __delete__(self, instance):
        if self.fdel is None:
            raise AttributeError("property has no deleter")
        self.fdel(instance)

    def getter(self, fget):
        return _class_of(self)(fget, self.fset, self.fdel, self.__doc__)

    def setter(self, fset):
        return _class_of(self)(self.fget, fset, self.fdel, self.__doc__)

    def deleter(self, fdel):
        return _class_of(self)(self.fget, self.fset, fdel, self.__doc__)


class super:
    """Attribute lookup that starts after ``__thisclass__`` in the class
    hierarchy of ``__self__``, binding what it finds to ``__self__``.

    The search follows the MRO of ``__self__``'s class (or of ``__self__``
    itself when it is a class), starting just after ``__thisclass__``."""

    def __init__(self, type, obj):
        self.__thisclass__ = type
        self.__self__ = obj

    def __getattribute__(self, name):
        thisclass = _instance_field(self, "__thisclass__")
        obj = _instance_field(self, "__self__")
        # When obj is a class, it selects its own hierarchy only if it is
        # a subclass of thisclass (the classmethod case). A class that is
        # merely an instance of thisclass, as a class is of its metaclass,
        # searches the hierarchy of its own class instead.
        if _is_class(obj) and _is_subclass(obj, thisclass):
            owner, instance = obj, None
        else:
            owner, instance = _class_of(obj), obj
        order = _class_mro(owner)
        index = 0
        while index < len(order) and order[index] is not thisclass:
            index += 1
        index += 1
        while index < len(order):
            value = _class_lookup_or_null(order[index], name)
            if value is not None:
                return _bind_attribute(value, instance, owner)
            index += 1
        raise AttributeError("'super' object has no attribute '" + name + "'")
