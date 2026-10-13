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
from _mule_native import bound_method as _bound_method
from _mule_native import class_base as _class_base
from _mule_native import class_bases as _class_bases
from _mule_native import class_lookup as _class_lookup
from _mule_native import class_mro as _class_mro
from _mule_native import class_name as _class_name
from _mule_native import class_own_names as _class_own_names
from _mule_native import default_repr as _default_repr
from _mule_native import delete_attribute as _delete_attribute
from _mule_native import generic_get_attribute as _generic_get_attribute
from _mule_native import generic_set_attribute as _generic_set_attribute
from _mule_native import identity_hash as _identity_hash
from _mule_native import instance_items as _instance_items
from _mule_native import is_class as _is_class
from _mule_native import is_instance as _is_instance
from _mule_native import is_subclass as _is_subclass
from _mule_native import new_class as _new_class
from _mule_native import namespace_items as _namespace_items
from _mule_native import namespace_bind as _namespace_bind
from _mule_native import namespace_unbind as _namespace_unbind
from _mule_native import math_float_format as _math_float_format
from _mule_native import register_type as _register_type
from _mule_native import traceback_field as _traceback_field
from _mule_native import set_class_name as _set_class_name
from _mule_native import type_of as _type_of
from _mule_native import typed_value as _typed_value
from _mule_native import function_attributes as _function_attributes

# The runtime's conversion functions, captured before the classes below
# take over their builtin names.
_convert_int = int
_convert_float = float
_convert_bool = bool


def _format_number(value, kind, format_spec):
    """Apply a numeric format spec. ``kind`` picks the conversion family:
    the float kinds format like float, ``s`` like str; signs, width,
    grouping, and alignment are applied here."""
    fill = " "
    align = ">"
    sign_option = "-"
    alternate = False
    zero = False
    width = 0
    group = False
    precision = -1
    index = 0
    size = len(format_spec)
    if index + 1 < size and format_spec[index + 1] in "<>^=":
        fill = format_spec[index]
        align = format_spec[index + 1]
        index += 2
    elif index < size and format_spec[index] in "<>^=":
        align = format_spec[index]
        index += 1
    if index < size and format_spec[index] in "+- ":
        sign_option = format_spec[index]
        index += 1
    if index < size and format_spec[index] == "#":
        alternate = True
        index += 1
    if index < size and format_spec[index] == "0":
        zero = True
        if fill == " ":
            fill = "0"
        if align == ">":
            align = "="
        index += 1
    while index < size and "0" <= format_spec[index] <= "9":
        width = width * 10 + "0123456789".index(format_spec[index])
        index += 1
    if index < size and format_spec[index] == ",":
        group = True
        index += 1
    if index < size and format_spec[index] == ".":
        index += 1
        precision = 0
        while index < size and "0" <= format_spec[index] <= "9":
            precision = precision * 10 + "0123456789".index(format_spec[index])
            index += 1
    conversion = format_spec[index] if index < size else ""
    prefix = ""
    if kind == "float":
        codes = {"f": 0, "e": 1, "E": 2, "g": 3, "G": 4, "%": 5, "": 6}
        if conversion not in codes:
            raise ValueError("unknown format code '" + conversion + "' for object of type 'float'")
        if precision < 0:
            precision = 6
        body = _math_float_format(value, codes[conversion], precision, alternate)
    elif conversion in "eEfFgG%" and kind == "int":
        codes = {"f": 0, "e": 1, "E": 2, "g": 3, "G": 4, "%": 5}
        if precision < 0:
            precision = 6
        body = _math_float_format(_convert_float(value), codes[conversion], precision, alternate)
        kind = "float"
    elif kind == "int":
        number = value
        negative = number < 0
        if negative:
            number = -number
        if conversion in ("d", ""):
            body = str(number)
        elif conversion in "xXo":
            digits = "0123456789abcdef" if conversion != "X" else "0123456789ABCDEF"
            base = 8 if conversion == "o" else 16
            body = ""
            if number == 0:
                body = "0"
            while number > 0:
                body = digits[number % base] + body
                number = number // base
            if alternate:
                prefix = "0o" if conversion == "o" else ("0x" if conversion == "x" else "0X")
        elif conversion == "b":
            body = ""
            if number == 0:
                body = "0"
            while number > 0:
                body = "01"[number % 2] + body
                number = number // 2
            if alternate:
                prefix = "0b"
        elif conversion == "c":
            if negative or value > 0x10FFFF:
                raise OverflowError("%c arg not in range(0x110000)")
            return chr(value)
        else:
            raise ValueError("unknown format code '" + conversion + "' for object of type 'int'")
        if precision >= 0:
            while len(body) < precision:
                body = "0" + body
        body = ("-" if negative else "") + body
    else:
        if conversion not in ("s", ""):
            raise ValueError("unknown format code '" + conversion + "' for object of type 'str'")
        body = value
        if precision >= 0:
            body = body[:precision]
    if group and kind != "str":
        if "e" in body or "E" in body:
            body = body
        else:
            dot = body.find(".")
            head = body if dot < 0 else body[:dot]
            tail = "" if dot < 0 else body[dot:]
            digits = head[1:] if head[:1] in "+- " else head
            signpart = head[: len(head) - len(digits)]
            grouped = ""
            count = 0
            at = len(digits)
            while at > 0:
                start = at - 3 if at >= 3 else 0
                piece = digits[start:at]
                grouped = piece + ("," + grouped if grouped else "")
                at = start
                count += 1
            body = signpart + grouped + tail
    sign = ""
    digits = body
    if digits[:1] == "-":
        sign = "-"
        digits = digits[1:]
    elif kind != "str" and sign_option == "+":
        sign = "+"
    elif kind != "str" and sign_option == " ":
        sign = " "
    text = sign + prefix + digits
    if len(text) < width:
        pad = width - len(text)
        if align == "<":
            text = text + fill * pad
        elif align == "^":
            before = pad // 2
            text = fill * before + text + fill * (pad - before)
        elif align == "=":
            text = sign + prefix + fill * pad + digits
        else:
            text = fill * pad + text
    return text


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

    def __reduce_ex__(self, protocol):
        # The default recipe copyreg provides: call the class with no
        # arguments to reconstruct. Pickle state is not supported yet.
        return (_type_of(self), ())

    def __reduce__(self):
        return self.__reduce_ex__(2)

    def __delattr__(self, name):
        _delete_attribute(self, name)

    # The runtime applies these defaults itself when a class inherits them
    # (see ClassLayout::special); they exist for explicit calls such as
    # ``object.__repr__(self)`` and must match that built-in behavior.
    def __repr__(self):
        return _default_repr(self)

    def __str__(self):
        return _type_of(self).__repr__(self)

    def __format__(self, format_spec):
        if format_spec:
            raise TypeError(
                f"unsupported format string passed to {_class_name(_type_of(self))}.__format__"
            )
        return str(self)

    def __hash__(self):
        return _identity_hash(self)

    def __eq__(self, other):
        return True if self is other else NotImplemented

    def __ne__(self, other):
        result = _type_of(self).__eq__(self, other)
        return result if result is NotImplemented else not result

    def __lt__(self, other):
        return NotImplemented

    def __le__(self, other):
        return NotImplemented

    def __gt__(self, other):
        return NotImplemented

    def __ge__(self, other):
        return NotImplemented

    def __init_subclass__(cls, **keywords):
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
        if "__annotations__" not in namespace:
            names.append("__annotations__")
            values.append({})
        # The metaclass of the new class is the most derived metaclass of
        # the bases, as type_new computes it; it starts as the metaclass
        # this call was made through (type itself for a plain type() call).
        winner = metaclass
        for base in bases:
            candidate = _type_of(base)
            if _is_subclass(candidate, winner):
                winner = candidate
            elif not _is_subclass(winner, candidate):
                raise TypeError(
                    "metaclass conflict: the metaclass of a derived class "
                    "must be a (non-strict) subclass of the metaclasses "
                    "of all its bases"
                )
        cls = _new_class(
            winner,
            name,
            bases,
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
        return list(_class_mro(cls))

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


def _class_annotations(cls):
    # A class without stored annotations has an empty dict, as
    # type.__annotations__ provides in CPython. Classes keep their own
    # stored dict when they define annotations.
    if "__annotations__" in _class_own_names(cls):
        return _namespace(cls)["__annotations__"]
    return {}


type.__annotations__ = property(_class_annotations)


def _bases(cls):
    return _class_bases(cls)


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


def globals(namespace):
    # The compiler supplies the calling function's namespace as the one
    # argument, as it supplies the implicit arguments of super(); see
    # _CallTarget.visit_Name in the compiler.
    return _GlobalsDict(namespace)


class _GlobalsDict(object):
    """The live dict of a module's globals: reads and writes go to the
    module namespace itself, so it behaves as ``globals()`` must."""

    def __init__(self, namespace):
        self._namespace = namespace

    def __getitem__(self, key):
        for name, value in _namespace_items(self._namespace):
            if name == key:
                return value
        raise KeyError(key)

    def __setitem__(self, key, value):
        _namespace_bind(self._namespace, key, value)

    def __delitem__(self, key):
        if not _namespace_unbind(self._namespace, key):
            raise KeyError(key)

    def __contains__(self, key):
        for name, value in _namespace_items(self._namespace):
            if name == key:
                return True
        return False

    def __iter__(self):
        return iter(self.keys())

    def __len__(self):
        return len(_namespace_items(self._namespace))

    def keys(self):
        return [name for name, value in _namespace_items(self._namespace)]

    def values(self):
        return [value for name, value in _namespace_items(self._namespace)]

    def items(self):
        return list(_namespace_items(self._namespace))

    def get(self, key, default=None):
        for name, value in _namespace_items(self._namespace):
            if name == key:
                return value
        return default

    def pop(self, key, *default):
        for name, value in _namespace_items(self._namespace):
            if name == key:
                _namespace_unbind(self._namespace, key)
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
        return dict(_namespace_items(self._namespace))

    def __eq__(self, other):
        return self.copy() == other

    def __repr__(self):
        return repr(self.copy())


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


def _complex_parts(value):
    if isinstance(value, complex):
        return value.real, value.imag
    if isinstance(value, float):
        return value, 0.0
    if isinstance(value, int):
        return _convert_float(value), 0.0
    return None


class complex(object):
    """A complex number, stored as two float fields."""

    def __new__(cls, real=0, imag=0):
        if isinstance(real, str):
            if imag != 0:
                raise TypeError("complex() can't take second arg if first is a string")
            return cls._parse(real)
        parts = _complex_parts(real)
        if parts is None:
            raise TypeError("complex() first argument must be a string or a number")
        real_part, imag_part = parts
        if isinstance(imag, str):
            raise TypeError("complex() second arg can't be a string")
        imag_parts = _complex_parts(imag)
        if imag_parts is None:
            raise TypeError("complex() second argument must be a number")
        real_part = real_part - imag_parts[1]
        imag_part = imag_part + imag_parts[0]
        instance = _new_instance(cls)
        instance._real = _convert_float(real_part)
        instance._imag = _convert_float(imag_part)
        return instance

    @classmethod
    def _parse(cls, text):
        value = text.strip().replace(" ", "")
        if not value.endswith(("j", "J")):
            instance = _new_instance(cls)
            instance._real = _convert_float(float(value))
            instance._imag = 0.0
            return instance
        body = value[:-1]
        split = -1
        index = 1
        while index < len(body):
            if body[index] in "+-" and body[index - 1] not in "eE":
                split = index
            index += 1
        if split < 0:
            real_part, imag_part = 0.0, float(body) if body not in ("", "+", "-") else (1.0 if body != "-" else -1.0)
        else:
            real_part = float(body[:split])
            imag_text = body[split:]
            imag_part = 1.0 if imag_text == "+" else (-1.0 if imag_text == "-" else float(imag_text))
        instance = _new_instance(cls)
        instance._real = _convert_float(real_part)
        instance._imag = _convert_float(imag_part)
        return instance

    @property
    def real(self):
        return self._real

    @property
    def imag(self):
        return self._imag

    def conjugate(self):
        instance = _new_instance(_type_of(self))
        instance._real = self._real
        instance._imag = -self._imag
        return instance

    def __complex__(self):
        return self

    def __add__(self, other):
        parts = _complex_parts(other)
        if parts is None:
            return NotImplemented
        return _make_complex(_type_of(self), self._real + parts[0], self._imag + parts[1])

    def __radd__(self, other):
        return self.__add__(other)

    def __sub__(self, other):
        parts = _complex_parts(other)
        if parts is None:
            return NotImplemented
        return _make_complex(_type_of(self), self._real - parts[0], self._imag - parts[1])

    def __rsub__(self, other):
        parts = _complex_parts(other)
        if parts is None:
            return NotImplemented
        return _make_complex(_type_of(self), parts[0] - self._real, parts[1] - self._imag)

    def __mul__(self, other):
        parts = _complex_parts(other)
        if parts is None:
            return NotImplemented
        return _make_complex(
            _type_of(self),
            self._real * parts[0] - self._imag * parts[1],
            self._real * parts[1] + self._imag * parts[0],
        )

    def __rmul__(self, other):
        return self.__mul__(other)

    def __truediv__(self, other):
        parts = _complex_parts(other)
        if parts is None:
            return NotImplemented
        denominator = parts[0] * parts[0] + parts[1] * parts[1]
        if denominator == 0:
            raise ZeroDivisionError("complex division by zero")
        return _make_complex(
            _type_of(self),
            (self._real * parts[0] + self._imag * parts[1]) / denominator,
            (self._imag * parts[0] - self._real * parts[1]) / denominator,
        )

    def __rtruediv__(self, other):
        parts = _complex_parts(other)
        if parts is None:
            return NotImplemented
        return _make_complex(_type_of(self), parts[0], parts[1]).__truediv__(self)

    def __neg__(self):
        return _make_complex(_type_of(self), -self._real, -self._imag)

    def __pos__(self):
        return self

    def __abs__(self):
        return (self._real * self._real + self._imag * self._imag) ** 0.5

    def __bool__(self):
        return self._real != 0 or self._imag != 0

    def __eq__(self, other):
        parts = _complex_parts(other)
        if parts is None:
            return NotImplemented
        return self._real == parts[0] and self._imag == parts[1]

    def __ne__(self, other):
        result = self.__eq__(other)
        return result if result is NotImplemented else not result

    def __hash__(self):
        return hash((self._real, self._imag))

    def __repr__(self):
        if self._real == 0:
            return repr(self._imag) + "j"
        sign = "+" if self._imag >= 0 else "-"
        return "(" + repr(self._real) + sign + repr(abs(self._imag)) + "j)"


def _make_complex(cls, real, imag):
    instance = _new_instance(cls)
    instance._real = _convert_float(real)
    instance._imag = _convert_float(imag)
    return instance


class int(object):
    def __new__(cls, *args):
        if args:
            value = args[0]
            # Latin-1 preserves every byte so invalid input reaches the integer
            # parser as ValueError rather than failing UTF-8 decoding first.
            if isinstance(value, bytes):
                args = (bytes.decode(value, "latin-1"),) + args[1:]
            elif isinstance(value, bytearray):
                args = (bytearray.decode(value, "latin-1"),) + args[1:]
        value = _convert_int(*args)
        if cls is not int and cls is not bool:
            return _typed_value(value, cls)
        return value

    def __format__(self, format_spec=""):
        if format_spec == "":
            return str(self)
        return _format_number(self, "int", format_spec)

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

    def to_bytes(self, length=1, byteorder='little', *, signed=False):
        if length < 0:
            raise ValueError("length argument must be non-negative")
        value = self
        if value < 0:
            if not signed:
                raise OverflowError("can't convert negative int to unsigned")
            value += 1 << (8 * length)
            if value < 0:
                raise OverflowError("int too large to convert")
        elif value >= (1 << (8 * length)):
            raise OverflowError("int too large to convert")
        result = bytearray(length)
        for index in range(length):
            shift = 8 * index if byteorder == 'little' else 8 * (length - 1 - index)
            result[index] = (value >> shift) & 0xFF
        return bytes(result)

    @classmethod
    def from_bytes(cls, data, byteorder='little', *, signed=False):
        value = 0
        for index in range(len(data)):
            shift = 8 * index if byteorder == 'little' else 8 * (len(data) - 1 - index)
            value |= data[index] << shift
        if signed and len(data) and (data[0] if byteorder == 'big' else data[len(data) - 1]) & 0x80:
            value -= 1 << (8 * len(data))
        return value

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
        value = _convert_float(*args)
        if cls is not float:
            return _typed_value(value, cls)
        return value

    def __format__(self, format_spec=""):
        if format_spec == "":
            return str(self)
        return _format_number(self, "float", format_spec)

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

    def __init__(self, name, filename, qualname=None, firstlineno=0):
        self.co_name = name
        self.co_qualname = name if qualname is None else qualname
        self.co_filename = filename
        self.co_firstlineno = firstlineno
        self.co_flags = 0


class function(object):
    """The class of functions defined in compiled Python."""

    # The runtime supplies a function's real globals and closure cells
    # before class lookup (see function_attribute in the runtime), so these
    # getters only serve values with no native function behind them. Their
    # presence on the class is what types.py reads, as it reads __code__,
    # to name the descriptor types.
    @property
    def __globals__(self):
        return {}

    @property
    def __closure__(self):
        return None

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

    @property
    def __dict__(self):
        return _GlobalsDict(_function_attributes(self))

    def __get__(self, instance, owner=None):
        # Functions are descriptors: reading one through an instance binds
        # it, and through a class returns the function itself. Attribute
        # lookup applies this natively; this method exists for code that
        # looks the descriptor protocol up by name.
        if instance is None:
            return self
        return _bound_method(self, instance)


class builtin_function_or_method(object):
    """The class of functions the runtime implements natively."""

    def __get__(self, instance, owner=None):
        if instance is None:
            return self
        return _bound_method(self, instance)


class method(object):
    """A function bound to the instance it was read from."""


class module(object):
    """A module namespace."""


class traceback(object):
    """One entry of an exception's traceback: a frame the exception passed
    through, the line it was running, and the entry nearer the raise."""

    @property
    def tb_frame(self):
        return _traceback_field(self, 0)

    @property
    def tb_lineno(self):
        return _traceback_field(self, 1)

    @property
    def tb_next(self):
        return _traceback_field(self, 2)

    @property
    def tb_lasti(self):
        # Compiled code has no bytecode offsets.
        return -1


class frame(object):
    """A function call, live or one an exception passed through. Compiled
    frames do not keep their local variables; ``f_locals`` is the frame's
    globals under the framelocalsproxy type, as it is for a module frame."""

    @property
    def f_code(self):
        info = _traceback_field(self, 0)
        return code(info[0], info[2], info[1], info[3])

    @property
    def f_lineno(self):
        return _traceback_field(self, 1)

    @property
    def f_globals(self):
        return _traceback_field(self, 2)

    @property
    def f_back(self):
        return _traceback_field(self, 3)

    @property
    def f_locals(self):
        result = {}
        for name, value in _namespace_items(_traceback_field(self, 2)):
            result[name] = value
        return framelocalsproxy(result)


class framelocalsproxy(object):
    """The mapping type of a frame's ``f_locals`` (PEP 667)."""

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
        return repr(self._mapping)


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
_register_type("traceback", traceback)
_register_type("frame", frame)


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
