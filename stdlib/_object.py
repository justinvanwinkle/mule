"""``object``, ``type`` and the classes of builtin values, compiled by Mule.

``object`` is every class's root and ``type`` every class's default
metaclass, following CPython's ``Objects/typeobject.c``. The other classes
here give a class to each kind of runtime value that is not an instance of a
compiled class (``type(None)``, ``type(1)``, ``type(len)`` and so on), so that
``type``, ``isinstance`` and method lookup apply to every value. Their
operations remain the runtime's native ones. This module is imported before
any other compiled builtin defines a class, so those classes derive from
``object``.
"""

from _mule_native import allocate_instance as _new_instance
from _mule_native import class_base as _class_base
from _mule_native import class_lookup as _class_lookup
from _mule_native import class_name as _class_name
from _mule_native import class_own_names as _class_own_names
from _mule_native import delete_attribute as _delete_attribute
from _mule_native import generic_get_attribute as _generic_get_attribute
from _mule_native import generic_set_attribute as _generic_set_attribute
from _mule_native import instance_items as _instance_items
from _mule_native import is_class as _is_class
from _mule_native import is_instance as _is_instance
from _mule_native import is_subclass as _is_subclass
from _mule_native import new_class as _new_class
from _mule_native import register_type as _register_type
from _mule_native import set_class_name as _set_class_name
from _mule_native import type_of as _type_of

# The runtime's conversion functions, captured before the classes below
# take over their builtin names.
_convert_int = int
_convert_float = float
_convert_bool = bool


class object:
    """The base of every class."""

    def __new__(cls, *args):
        if args and _overrides(cls, "__init__") is False:
            raise TypeError(f"{_class_name(cls)}() takes no arguments")
        return _new_instance(cls)

    def __init__(self, *args):
        cls = _type_of(self)
        if args and (_overrides(cls, "__init__") or not _overrides(cls, "__new__")):
            raise TypeError(
                "object.__init__() takes exactly one argument (the instance to initialize)"
            )

    def __getattribute__(self, name):
        return _generic_get_attribute(self, name)

    def __setattr__(self, name, value):
        _generic_set_attribute(self, name, value)

    def __delattr__(self, name):
        _delete_attribute(self, name)

    def __init_subclass__(cls):
        pass


_register_type("object", object)


def _overrides(cls, name):
    """Whether ``cls`` defines ``name`` somewhere other than ``object``."""
    return _class_lookup(cls, name) is not _class_lookup(object, name)


class type(object):
    """The class of classes: ``type(value)`` is a value's class, and
    ``type(name, bases, namespace)`` creates a class."""

    def __new__(metaclass, *args, **keywords):
        if len(args) == 1 and metaclass is type and not keywords:
            return _type_of(args[0])
        if len(args) != 3:
            raise TypeError("type() takes 1 or 3 arguments")
        name, bases, namespace = args
        if not _is_instance(name, str):
            raise TypeError("type.__new__() argument 1 must be str")
        if not _is_instance(bases, tuple):
            raise TypeError("type.__new__() argument 2 must be tuple")
        if len(bases) > 1:
            raise TypeError("multiple inheritance is not supported yet")
        fields = ()
        names, values = [], []
        for key in namespace:
            value = namespace[key]
            if key == "__mule_fields__":
                fields = value
                continue
            # These hooks are implicit class methods (CPython's type_new).
            if key in ("__init_subclass__", "__class_getitem__") and _is_instance(
                value, function
            ):
                value = classmethod(value)
            names.append(key)
            values.append(value)
        if "__qualname__" not in namespace:
            names.append("__qualname__")
            values.append(name)
        if "__doc__" not in namespace:
            names.append("__doc__")
            values.append(None)
        cls = _new_class(
            metaclass,
            name,
            bases[0] if bases else None,
            tuple(names),
            tuple(values),
            fields,
        )
        index = 0
        while index < len(names):
            hook = getattr(_type_of(values[index]), "__set_name__", None)
            if hook is not None:
                hook(values[index], cls, names[index])
            index += 1
        super(cls, cls).__init_subclass__(**keywords)
        return cls

    def __init__(cls, *args, **keywords):
        pass

    def __prepare__(metaclass, name, bases, **keywords):
        return {}

    def mro(cls):
        order = []
        current = cls
        while current is not None:
            order.append(current)
            current = _class_base(current)
        return order

    def __instancecheck__(cls, instance):
        return _is_instance(instance, cls)

    def __subclasscheck__(cls, subclass):
        if not _is_class(subclass):
            raise TypeError("issubclass() arg 1 must be a class")
        return _is_subclass(subclass, cls)

    def __subclasses__(cls):
        return []


_register_type("type", type)

# Descriptor classes derive from object, so they are created after it.
from _descriptors import classmethod, property, staticmethod, super  # noqa: E402
from _attributes import getattr, hasattr  # noqa: E402

object.__init_subclass__ = classmethod(object.__init_subclass__)
type.__prepare__ = classmethod(type.__prepare__)


def _bases(cls):
    base = _class_base(cls)
    return () if base is None else (base,)


def _namespace(cls):
    own = {}
    for name in _class_own_names(cls):
        own[name] = _class_lookup(cls, name)
    return mappingproxy(own)


object.__class__ = property(_type_of)
type.__name__ = property(_class_name, _set_class_name)
type.__base__ = property(_class_base)
type.__bases__ = property(_bases)
type.__mro__ = property(lambda cls: tuple(cls.mro()))
type.__dict__ = property(_namespace)


class _InstanceDict(object):
    """An instance's ``__dict__``: a live view of its attributes."""

    __slots__ = ("_owner",)

    def __init__(self, owner):
        self._owner = owner

    def __getitem__(self, key):
        for name, value in _instance_items(self._owner):
            if name == key:
                return value
        raise KeyError(key)

    def __setitem__(self, key, value):
        _generic_set_attribute(self._owner, key, value)

    def __delitem__(self, key):
        try:
            _delete_attribute(self._owner, key)
        except AttributeError:
            raise KeyError(key)

    def __contains__(self, key):
        for name, value in _instance_items(self._owner):
            if name == key:
                return True
        return False

    def __iter__(self):
        return iter(self.keys())

    def __len__(self):
        return len(_instance_items(self._owner))

    def keys(self):
        return [name for name, value in _instance_items(self._owner)]

    def values(self):
        return [value for name, value in _instance_items(self._owner)]

    def items(self):
        return list(_instance_items(self._owner))

    def get(self, key, default=None):
        for name, value in _instance_items(self._owner):
            if name == key:
                return value
        return default

    def setdefault(self, key, default=None):
        if key in self:
            return self[key]
        self[key] = default
        return default

    def pop(self, key, *default):
        if key in self:
            value = self[key]
            del self[key]
            return value
        if default:
            return default[0]
        raise KeyError(key)

    def update(self, other=(), **keywords):
        items = other.items() if hasattr(other, "keys") else other
        for key, value in items:
            self[key] = value
        for key in keywords:
            self[key] = keywords[key]

    def copy(self):
        return dict(_instance_items(self._owner))

    def __eq__(self, other):
        return self.copy() == other

    def __repr__(self):
        return repr(self.copy())


object.__dict__ = property(_InstanceDict)


def vars(*args):
    if not args:
        raise TypeError("vars() without an argument is not supported yet")
    return args[0].__dict__


class mappingproxy(object):
    """A read-only view of a mapping, as ``type.__dict__`` returns."""

    def __init__(self, mapping):
        self._mapping = mapping

    def __getitem__(self, key):
        return self._mapping[key]

    def __contains__(self, key):
        return key in self._mapping

    def __iter__(self):
        return iter(self._mapping)

    def __len__(self):
        return len(self._mapping)

    def get(self, key, default=None):
        return self._mapping.get(key, default)

    def keys(self):
        return self._mapping.keys()

    def values(self):
        return self._mapping.values()

    def items(self):
        return self._mapping.items()

    def copy(self):
        return self._mapping.copy()

    def __repr__(self):
        return f"mappingproxy({self._mapping!r})"


class NoneType(object):
    def __new__(cls):
        return None

    def __repr__(self):
        return "None"

    def __bool__(self):
        return False


class int(object):
    def __new__(cls, *args):
        if cls is not int and cls is not bool:
            raise TypeError("int subclasses are not supported yet")
        return _convert_int(*args)

    def __index__(self):
        return self

    def __int__(self):
        return self

    def __float__(self):
        return _convert_float(self)

    def __round__(self, ndigits=None):
        if ndigits is None or ndigits >= 0:
            return self
        scale = 10 ** -ndigits
        quotient, remainder = self // scale, self % scale
        doubled = remainder * 2
        if doubled > scale or (doubled == scale and quotient % 2):
            quotient += 1
        return quotient * scale

    def __divmod__(self, other):
        if not isinstance(other, int):
            return NotImplemented
        return (self // other, self % other)

    def __trunc__(self):
        return self

    def __floor__(self):
        return self

    def __ceil__(self):
        return self

    def bit_length(self):
        value = -self if self < 0 else self
        length = 0
        while value:
            value >>= 1
            length += 1
        return length

    def conjugate(self):
        return self

    @property
    def real(self):
        return self

    @property
    def imag(self):
        return 0

    @property
    def numerator(self):
        return self

    @property
    def denominator(self):
        return 1


class bool(int):
    def __new__(cls, *args):
        return _convert_bool(*args)


class float(object):
    def __new__(cls, *args):
        if cls is not float:
            raise TypeError("float subclasses are not supported yet")
        return _convert_float(*args)

    def __float__(self):
        return self

    def is_integer(self):
        return self == _convert_int(self)

    def __round__(self, ndigits=None):
        # Round half to even (CPython's float_round); with ndigits, the
        # decimal scaling is binary, so it can differ from CPython's
        # correctly rounded result in the last place.
        if ndigits is None:
            whole = _convert_int(self)
            fraction = self - whole
            if fraction > 0.5 or (fraction == 0.5 and whole % 2):
                whole += 1
            elif fraction < -0.5 or (fraction == -0.5 and whole % 2):
                whole -= 1
            return whole
        scale = 10.0 ** ndigits
        return float.__round__(self * scale) / scale

    def __trunc__(self):
        return _convert_int(self)

    def __floor__(self):
        whole = _convert_int(self)
        return whole - 1 if whole > self else whole

    def __ceil__(self):
        whole = _convert_int(self)
        return whole + 1 if whole < self else whole

    def __divmod__(self, other):
        return (self // other, self % other)

    def conjugate(self):
        return self

    @property
    def real(self):
        return self

    @property
    def imag(self):
        return 0.0


class code(object):
    """A function's compiled-code metadata."""

    def __init__(self, name, filename):
        self.co_name = name
        self.co_qualname = name
        self.co_filename = filename
        self.co_flags = 0


class function(object):
    """The class of functions defined in compiled Python."""

    @property
    def __code__(self):
        return code(self.__name__, None)

    @property
    def __defaults__(self):
        return None

    @property
    def __kwdefaults__(self):
        return None

    @property
    def __annotations__(self):
        return {}


class builtin_function_or_method(object):
    """The class of functions the runtime implements natively."""


class method(object):
    """A function bound to the instance it was read from."""


class module(object):
    """A module namespace."""


# Bootstrap code runs before the exception classes exist, so it cannot use
# loops, which end by raising StopIteration.
_register_type("NoneType", NoneType)
_register_type("int", int)
_register_type("bool", bool)
_register_type("float", float)
_register_type("function", function)
_register_type("builtin_function_or_method", builtin_function_or_method)
_register_type("method", method)
_register_type("module", module)


def isinstance(obj, classinfo, /):
    if _is_instance(classinfo, tuple):
        for candidate in classinfo:
            if isinstance(obj, candidate):
                return True
        return False
    if _type_of(classinfo) is type:
        return _is_instance(obj, classinfo)
    check = getattr(_type_of(classinfo), "__instancecheck__", None)
    if check is None:
        raise TypeError(
            "isinstance() arg 2 must be a type, a tuple of types, or a union"
        )
    return bool(check(classinfo, obj))


def issubclass(cls, classinfo, /):
    if _is_instance(classinfo, tuple):
        for candidate in classinfo:
            if issubclass(cls, candidate):
                return True
        return False
    if _type_of(classinfo) is type:
        if not _is_class(cls):
            raise TypeError("issubclass() arg 1 must be a class")
        return _is_subclass(cls, classinfo)
    check = getattr(_type_of(classinfo), "__subclasscheck__", None)
    if check is None:
        raise TypeError(
            "issubclass() arg 2 must be a class, a tuple of classes, or a union"
        )
    return bool(check(classinfo, cls))
