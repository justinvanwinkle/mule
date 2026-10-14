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
from _mule_native import default_construct as _default_construct
from _mule_native import bind_instance_attribute as _bind_attribute
from _mule_native import bound_method as _bound_method
from _mule_native import bound_method_receiver as _bound_method_receiver
from _mule_native import bound_method_function as _bound_method_function
from _mule_native import class_base as _class_base
from _mule_native import class_bases as _class_bases
from _mule_native import class_subclasses as _class_subclasses
from _mule_native import class_lookup as _class_lookup
from _mule_native import class_lookup_default as _class_lookup_default
from _mule_native import class_mro as _class_mro
from _mule_native import class_name as _class_name
from _mule_native import class_own_names as _class_own_names
from _mule_native import default_repr as _default_repr
from _mule_native import generic_get_attribute as _generic_get_attribute
from _mule_native import generic_set_attribute as _generic_set_attribute
from _mule_native import generic_delete_attribute as _generic_delete_attribute
from _mule_native import identity_hash as _identity_hash
from _mule_native import instance_items as _instance_items
from _mule_native import instance_extra_items as _instance_extra_items
from _mule_native import instance_set_binding as _instance_set_binding
from _mule_native import instance_delete_binding as _instance_delete_binding
from _mule_native import instance_field as _instance_field
from _mule_native import set_instance_field as _set_instance_field
from _mule_native import delete_instance_field as _delete_instance_field
from _mule_native import storage_dictionary as _storage_dictionary
from _mule_native import is_class as _is_class
from _mule_native import is_instance as _is_instance
from _mule_native import is_subclass as _is_subclass
from _mule_native import new_class as _new_class
from _mule_native import namespace_items as _namespace_items
from _mule_native import namespace_binding as _namespace_binding
from _mule_native import namespace_bind as _namespace_bind
from _mule_native import namespace_unbind as _namespace_unbind
from _mule_native import math_float_bits as _math_float_bits
from _mule_native import math_float_format as _math_float_format
from _mule_native import numeric_negate as _numeric_negate
from _mule_native import numeric_invert as _numeric_invert
from _mule_native import integer_decimal as _integer_decimal
from _mule_native import math_fabs as _math_fabs
from _mule_native import register_type as _register_type
from _mule_native import traceback_field as _traceback_field
from _mule_native import set_class_name as _set_class_name
from _mule_native import type_of as _type_of
from _mule_native import typed_value as _typed_value
from _mule_native import function_attributes as _function_attributes
from _mule_native import code_metadata as _code_metadata
from _mule_native import code_cache as _code_cache
from _mule_native import set_code_cache as _set_code_cache
from _mule_native import function_captured_defaults as _function_captured_defaults
from _mule_native import function_positional_defaults as _function_positional_defaults
from _mule_native import set_function_positional_defaults as _set_function_positional_defaults
from _mule_native import function_keyword_defaults as _function_keyword_defaults
from _mule_native import set_function_keyword_defaults as _set_function_keyword_defaults
from _mule_native import function_annotate as _function_annotate
from _mule_native import function_annotations as _function_annotations
from _mule_native import set_function_annotate as _set_function_annotate
from _mule_native import set_function_annotations as _set_function_annotations
from _mule_native import byte_length as _byte_length
from _mule_native import read_byte as _read_byte

# The runtime's conversion functions, captured before the classes below
# take over their builtin names.
_convert_int = int
_convert_float = float
_convert_bool = bool


def _bytes_special(value):
    """Apply __bytes__ once, distinguishing absence from a non-callable value."""
    missing = _binding_missing()
    owner = _type_of(value)
    method = _class_lookup_default(owner, "__bytes__", missing)
    if method is missing:
        return False, None
    try:
        method = _bind_attribute(method, value, owner)
    except AttributeError:
        return False, None
    result = method()
    if not _is_instance(result, bytes):
        raise TypeError(f"__bytes__ returned non-bytes (type {_type_of(result).__name__})")
    return True, result


def _index_integer(value):
    if _is_instance(value, int):
        return _convert_int(value)
    owner = _type_of(value)
    missing = _binding_missing()
    method = _class_lookup_default(owner, "__index__", missing)
    if method is missing:
        raise TypeError(f"'{owner.__name__}' object cannot be interpreted as an integer")
    result = _bind_attribute(method, value, owner)()
    if not _is_instance(result, int):
        raise TypeError(f"__index__ returned non-int (type {_type_of(result).__name__})")
    return _convert_int(result)


def _integer_byteorder(value, operation):
    if not _is_instance(value, str):
        name = "None" if value is None else _type_of(value).__name__
        raise TypeError(f"{operation}() argument 'byteorder' must be str, not {name}")
    order = str.encode(value, "utf-8")
    if order == b"little":
        return False
    if order == b"big":
        return True
    raise ValueError("byteorder must be either 'little' or 'big'")


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

    def __mule_annotation_mapping__(self):
        raise TypeError("__annotations__ must be a dict or None")

    def __dir__(self):
        try:
            names = set(self.__dict__)
        except AttributeError:
            names = set()
        for cls in _class_mro(_type_of(self)):
            names.update(_class_own_names(cls))
        return list(names)

    def __mule_exit_status__(self):
        return (1, str(self))

    def __setattr__(self, name, value):
        try:
            descriptor = _class_lookup(_type_of(self), name)
        except AttributeError:
            _generic_set_attribute(self, name, value)
            return
        descriptor_type = _type_of(descriptor)
        try:
            setter = _class_lookup(descriptor_type, "__set__")
        except AttributeError:
            try:
                _class_lookup(descriptor_type, "__delete__")
            except AttributeError:
                _generic_set_attribute(self, name, value)
                return
            raise AttributeError(f"attribute {name!r} is read-only")
        _bind_attribute(setter, descriptor, descriptor_type)(self, value)

    def __reduce_ex__(self, protocol):
        from _pickle_support import reduce_ex

        return reduce_ex(self, protocol)

    def __reduce__(self):
        from _pickle_support import reduce

        return reduce(self, 0)

    def __getstate__(self):
        from _pickle_support import getstate

        return getstate(self)

    def __delattr__(self, name):
        try:
            descriptor = _class_lookup(_type_of(self), name)
        except AttributeError:
            _generic_delete_attribute(self, name)
            return
        descriptor_type = _type_of(descriptor)
        try:
            deleter = _class_lookup(descriptor_type, "__delete__")
        except AttributeError:
            try:
                _class_lookup(descriptor_type, "__set__")
            except AttributeError:
                _generic_delete_attribute(self, name)
                return
            raise AttributeError(f"attribute {name!r} cannot be deleted")
        _bind_attribute(deleter, descriptor, descriptor_type)(self)

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
        owner = _type_of(self)
        method = _class_lookup(owner, '__eq__')
        result = _bind_attribute(method, self, owner)(other)
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

    def __subclasshook__(cls, subclass, /):
        return NotImplemented


_register_type("object", object)


def _overrides(cls, name):
    """Whether ``cls`` defines ``name`` somewhere other than ``object``."""
    return _class_lookup(cls, name) is not _class_lookup(object, name)


_slot_descriptors_ready = False


def _declared_slots(name, namespace):
    slots = namespace.get('__slots__', ())
    if isinstance(slots, str):
        slots = (slots,)
    result = []
    for slot in slots:
        if not isinstance(slot, str):
            raise TypeError("__slots__ items must be strings, not '" + type(slot).__name__ + "'")
        if not slot.isidentifier():
            raise TypeError('__slots__ must be identifiers')
        if slot in ('__dict__', '__weakref__'):
            continue
        if slot in namespace:
            raise ValueError("'" + slot + "' in __slots__ conflicts with class variable")
        if slot.startswith('__') and not slot.endswith('__'):
            prefix = name.lstrip('_')
            if prefix:
                slot = '_' + prefix + slot
        result.append(slot)
    return result


class type(object):
    """The class of classes: ``type(value)`` is a value's class, and
    ``type(name, bases, namespace)`` creates a class."""

    def __call__(cls, *args, **kwargs):
        if not _is_class(cls):
            raise TypeError('type.__call__ requires a class')
        return _default_construct(cls, tuple(args), tuple(kwargs), tuple(kwargs.values()))

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
        original_bases = bases
        resolved = []
        changed = False
        for base in bases:
            if _is_class(base):
                resolved.append(base)
                continue
            method = getattr(base, "__mro_entries__", None)
            if method is None:
                resolved.append(base)
                continue
            entries = method(bases)
            if not _is_instance(entries, tuple):
                raise TypeError("__mro_entries__ must return a tuple")
            changed = True
            for entry in entries:
                resolved.append(entry)
        bases = tuple(resolved)
        slots = _declared_slots(name, namespace) if '__slots__' in namespace else []
        fields = list(namespace.get('__mule_fields__', ()))
        for slot in slots:
            if slot not in fields:
                fields.append(slot)
        names, values = [], []
        for key in namespace:
            value = namespace[key]
            if key == "__mule_fields__":
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
        if changed and "__orig_bases__" not in namespace:
            names.append("__orig_bases__")
            values.append(original_bases)
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
            tuple(fields),
        )
        if _slot_descriptors_ready:
            for slot in slots:
                accessor = _SlotAccessor(slot)
                setattr(cls, slot, member_descriptor(cls, slot, property(accessor.get, accessor.set, accessor.delete)))
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
        return list(_class_subclasses(cls))

    def __dir__(cls):
        names = set()
        for base in _class_mro(cls):
            names.update(_class_own_names(base))
        return list(names)


_register_type("type", type)

# Descriptor classes derive from object, so they are created after it.
from _descriptors import classmethod, property, staticmethod, super  # noqa: E402
from _attributes import getattr, hasattr  # noqa: E402

object.__init_subclass__ = classmethod(object.__init_subclass__)
object.__subclasshook__ = classmethod(object.__subclasshook__)
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
    return mappingproxy(_ClassDict(cls))


object.__class__ = property(_type_of)
type.__name__ = property(_class_name, _set_class_name)
type.__base__ = property(_class_base)
type.__bases__ = property(_bases)
type.__mro__ = property(_class_mro)
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
        _instance_set_binding(self._owner, key, value)

    def __delitem__(self, key):
        if not _instance_delete_binding(self._owner, key):
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

    def _get_value(self, key, default):
        return self.get(key, default)

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


def _object_dictionary(owner):
    namespace = _storage_dictionary(owner)
    if namespace is None:
        return _InstanceDict(owner)
    return _GlobalsDict(namespace)


object.__dict__ = property(_object_dictionary)


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

    def _get_value(self, key, default):
        return self.get(key, default)

    def setdefault(self, key, default=None):
        try:
            return self[key]
        except KeyError:
            self[key] = default
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


class _ClassDict(object):
    """Read the class's own current bindings, without descriptor binding."""

    def __init__(self, owner):
        self._owner = owner

    def __getitem__(self, key):
        if key not in _class_own_names(self._owner):
            raise KeyError(key)
        return _class_lookup(self._owner, key)

    def __contains__(self, key):
        return key in _class_own_names(self._owner)

    def __iter__(self):
        return iter(self.keys())

    def __len__(self):
        return len(self.keys())

    def keys(self):
        return _class_own_names(self._owner)

    def values(self):
        return [self[key] for key in self.keys()]

    def items(self):
        return [(key, self[key]) for key in self.keys()]

    def get(self, key, default=None):
        return self[key] if key in self else default

    def copy(self):
        return dict(self.items())

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

    def __getattribute__(self, name):
        if name == "__class__":
            return _type_of(self)
        return _generic_get_attribute(self, name)

    def __mule_exit_status__(self):
        return (0, None)

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
    def __repr__(self):
        if not _is_instance(self, int):
            raise TypeError(f"descriptor '__repr__' requires a 'int' object but received a '{_type_of(self).__name__}'")
        return _integer_decimal(self)

    def __pos__(self):
        return _convert_int(self)

    def __neg__(self):
        return _numeric_negate(_convert_int(self))

    def __invert__(self):
        return _numeric_invert(_convert_int(self))

    def __abs__(self):
        value = _convert_int(self)
        return _numeric_negate(value) if value < 0 else value
    __slots__ = ()

    def __new__(cls, *args):
        if args:
            value = args[0]
            # Latin-1 preserves every byte so invalid input reaches the integer
            # parser as ValueError rather than failing UTF-8 decoding first.
            if isinstance(value, bytes):
                args = (bytes.decode(value, "latin-1"),) + args[1:]
            elif isinstance(value, bytearray):
                args = (bytearray.decode(value, "latin-1"),) + args[1:]
        if len(args) == 1:
            source = args[0]
            owner = _type_of(source)
            try:
                method = _class_lookup(owner, "__int__")
            except AttributeError:
                pass
            else:
                value = _bind_attribute(method, source, owner)()
                if not _is_instance(value, int):
                    raise TypeError(f"__int__ returned non-int (type {_type_of(value).__name__})")
                args = (value,)
        value = _convert_int(*args)
        if cls is not int and cls is not bool:
            return _typed_value(value, cls)
        return value

    def __format__(self, format_spec=""):
        if format_spec == "":
            return str(self)
        return _format_number(self, "int", format_spec)

    def __index__(self):
        if not _is_instance(self, int):
            raise TypeError(f"descriptor '__index__' requires a 'int' object but received a '{_type_of(self).__name__}'")
        return _convert_int(self)

    def __int__(self):
        if not _is_instance(self, int):
            raise TypeError(f"descriptor '__int__' requires a 'int' object but received a '{_type_of(self).__name__}'")
        return _convert_int(self)

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
        if not _is_instance(self, int):
            raise TypeError(f"descriptor '__divmod__' requires a 'int' object but received a '{_type_of(self).__name__}'")
        if not isinstance(other, int):
            return NotImplemented
        left, right = _convert_int(self), _convert_int(other)
        if right == 0:
            raise ZeroDivisionError('division by zero')
        return (left // right, left % right)

    def __rdivmod__(self, other):
        if not _is_instance(self, int):
            raise TypeError(f"descriptor '__rdivmod__' requires a 'int' object but received a '{_type_of(self).__name__}'")
        if not isinstance(other, int):
            return NotImplemented
        left, right = _convert_int(other), _convert_int(self)
        if right == 0:
            raise ZeroDivisionError('division by zero')
        return (left // right, left % right)

    def to_bytes(self, length=1, byteorder='big', *, signed=False):
        big_endian = _integer_byteorder(byteorder, "to_bytes")
        value = _convert_int(self)
        length = _index_integer(length)
        if length < 0:
            raise ValueError("length argument must be non-negative")
        if value < 0:
            if not signed:
                raise OverflowError("can't convert negative int to unsigned")
            if length and value < -(1 << (8 * length - 1)):
                raise OverflowError("int too big to convert")
            value += 1 << (8 * length)
            if value < 0:
                raise OverflowError("int too big to convert")
        elif value >= (1 << (8 * length - (1 if signed and length else 0))):
            raise OverflowError("int too big to convert")
        result = bytearray(length)
        for index in range(length):
            shift = 8 * (length - 1 - index) if big_endian else 8 * index
            result[index] = (value >> shift) & 0xFF
        return bytes(result)

    @classmethod
    def from_bytes(cls, data, byteorder='big', *, signed=False):
        big_endian = _integer_byteorder(byteorder, "from_bytes")
        converted, result = _bytes_special(data)
        if converted:
            data = result
        elif _is_instance(data, int) or _is_instance(data, str):
            raise TypeError(f"cannot convert '{_type_of(data).__name__}' object to bytes")
        if _is_instance(data, bytes):
            count = _byte_length(data)
            value = 0
            for index in range(count):
                position = index if big_endian else count - index - 1
                value = (value << 8) | _read_byte(data, position)
            if signed and count and _read_byte(data, 0 if big_endian else count - 1) & 0x80:
                value -= 1 << (8 * count)
            return cls(value)
        value = 0
        count, first, last = 0, 0, 0
        for item in data:
            item = _index_integer(item)
            if not 0 <= item <= 255:
                raise ValueError("bytes must be in range(0, 256)")
            if not count:
                first = item
            last = item
            value = value * 256 + item if big_endian else value | (item << (8 * count))
            count += 1
        if signed and count and (first if big_endian else last) & 0x80:
            value -= 1 << (8 * count)
        return cls(value)

    def __trunc__(self):
        return self

    def __floor__(self):
        return _convert_int(self)

    def __ceil__(self):
        return _convert_int(self)

    def __mule_exit_status__(self):
        value = _convert_int(self)
        if value < -9223372036854775808 or value > 9223372036854775807:
            return (255, None)
        return (value & 255, None)

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
    __slots__ = ()

    def __new__(cls, *args):
        return _convert_bool(*args)

    def __repr__(self):
        if not _is_instance(self, bool):
            raise TypeError(f"descriptor '__repr__' requires a 'bool' object but received a '{_type_of(self).__name__}'")
        return "True" if self else "False"


_FLOAT_INFINITY = _convert_float('inf')


class float(object):
    def __int__(self):
        if not _is_instance(self, float):
            raise TypeError(f"descriptor '__int__' requires a 'float' object but received a '{_type_of(self).__name__}'")
        value = _convert_float(self)
        if value != value:
            raise ValueError('cannot convert float NaN to integer')
        if value == _FLOAT_INFINITY or value == -_FLOAT_INFINITY:
            raise OverflowError('cannot convert float infinity to integer')
        return _convert_int(value)

    def __pos__(self):
        return _convert_float(self)

    def __neg__(self):
        return _numeric_negate(_convert_float(self))

    def __abs__(self):
        return _math_fabs(_convert_float(self))
    __slots__ = ()

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

    def hex(self, /):
        if not _is_instance(self, float):
            raise TypeError(f"descriptor 'hex' for 'float' objects doesn't apply to a '{_type_of(self).__name__}' object")
        bits = _math_float_bits(self)
        negative = bits >> 63
        exponent = (bits >> 52) & 0x7ff
        fraction = bits & ((1 << 52) - 1)
        sign = "-" if negative else ""
        if exponent == 0x7ff:
            return "nan" if fraction else sign + "inf"
        if exponent == 0 and fraction == 0:
            return sign + "0x0.0p+0"
        leading = "1" if exponent else "0"
        power = exponent - 1023 if exponent else -1022
        digits = ""
        for shift in range(48, -1, -4):
            digits += "0123456789abcdef"[(fraction >> shift) & 15]
        return sign + "0x" + leading + "." + digits + "p" + ("+" if power >= 0 else "-") + str(abs(power))

    def as_integer_ratio(self, /):
        if not _is_instance(self, float):
            raise TypeError(f"descriptor 'as_integer_ratio' for 'float' objects doesn't apply to a '{_type_of(self).__name__}' object")
        bits = _math_float_bits(self)
        exponent = (bits >> 52) & 0x7ff
        numerator = bits & ((1 << 52) - 1)
        if exponent == 0x7ff:
            if numerator:
                raise ValueError("cannot convert NaN to integer ratio")
            raise OverflowError("cannot convert Infinity to integer ratio")
        if exponent:
            numerator |= 1 << 52
            power = exponent - 1075
        else:
            power = -1074
        if numerator == 0:
            return 0, 1
        while power < 0 and numerator & 1 == 0:
            numerator >>= 1
            power += 1
        if bits >> 63:
            numerator = -numerator
        if power >= 0:
            return numerator << power, 1
        return numerator, 1 << -power

    def is_integer(self, /):
        if not _is_instance(self, float):
            raise TypeError(f"descriptor 'is_integer' for 'float' objects doesn't apply to a '{_type_of(self).__name__}' object")
        bits = _math_float_bits(self)
        exponent = (bits >> 52) & 0x7ff
        fraction = bits & ((1 << 52) - 1)
        if exponent == 0x7ff:
            return False
        if exponent == 0:
            return fraction == 0
        power = exponent - 1023
        if power < 0:
            return False
        if power >= 52:
            return True
        return fraction & ((1 << (52 - power)) - 1) == 0

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
        if not _is_instance(self, float):
            raise TypeError(f"descriptor '__divmod__' requires a 'float' object but received a '{_type_of(self).__name__}'")
        if not isinstance(other, (int, float)):
            return NotImplemented
        left, right = _convert_float(self), _convert_float(other)
        if right == 0.0:
            raise ZeroDivisionError('division by zero')
        return (left // right, left % right)

    def __rdivmod__(self, other):
        if not _is_instance(self, float):
            raise TypeError(f"descriptor '__rdivmod__' requires a 'float' object but received a '{_type_of(self).__name__}'")
        if not isinstance(other, (int, float)):
            return NotImplemented
        left, right = _convert_float(other), _convert_float(self)
        if right == 0.0:
            raise ZeroDivisionError('division by zero')
        return (left // right, left % right)

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

    def __init__(self, name, filename, qualname=None, firstlineno=0,
                 argcount=0, posonly=0, kwonly=0, flags=0, varnames=(),
                 cellvars=(), freevars=(), positional_defaults=0, keyword_defaults=(),
                 ast=None, mode=None):
        self._metadata = (name, filename, name if qualname is None else qualname,
                          firstlineno, argcount, posonly, kwonly, flags, varnames,
                          cellvars, freevars, positional_defaults, keyword_defaults)
        self._native_origin = None
        self._ast = ast
        self._mode = mode

    @property
    def co_nlocals(self):
        return len(self.co_varnames)

    def replace(self, /, **changes):
        fields = (
            "co_name", "co_filename", "co_qualname", "co_firstlineno",
            "co_argcount", "co_posonlyargcount", "co_kwonlyargcount", "co_flags",
            "co_varnames", "co_cellvars", "co_freevars",
        )
        metadata = list(self._metadata)
        for name, value in changes.items():
            if name not in fields:
                raise TypeError(f"replace() got an unexpected keyword argument '{name}'")
            index = fields.index(name)
            if 3 <= index <= 7:
                if not isinstance(value, int):
                    raise TypeError(f"'{type(value).__name__}' object cannot be interpreted as an integer")
                if value < 0:
                    raise ValueError(f"{name} must be a positive integer")
                if value >= 1 << 31:
                    raise OverflowError("Python int too large to convert to C int")
            elif index <= 2:
                if not isinstance(value, str):
                    raise TypeError(f"replace() argument '{name}' must be str, not {type(value).__name__}")
            elif not isinstance(value, tuple):
                raise TypeError(f"replace() argument '{name}' must be tuple, not {type(value).__name__}")
            metadata[index] = value
        result = code(*metadata, ast=self._ast, mode=self._mode)
        result._native_origin = self if self._native_origin is None else self._native_origin
        return result


def _code_field(index):
    return property(lambda self: self._metadata[index])


def _install_code_fields():
    # Called after sequence/descriptor builtins finish bootstrapping.
    for index, name in enumerate((
        "co_name", "co_filename", "co_qualname", "co_firstlineno", "co_argcount",
        "co_posonlyargcount", "co_kwonlyargcount", "co_flags", "co_varnames",
        "co_cellvars", "co_freevars", "_positional_defaults", "_keyword_defaults",
    )):
        setattr(code, name, _code_field(index))


def _code_for(receiver, fallback=None):
    cached = _code_cache(receiver)
    if cached is not None:
        return cached
    metadata = _code_metadata(receiver)
    if metadata is None:
        if fallback is None:
            raise AttributeError("native callable has no Python code metadata")
        cached = code(fallback[0], fallback[2], fallback[1], fallback[3])
    else:
        cached = code(*metadata)
    _set_code_cache(receiver, cached)
    return cached


class function(object):
    """The class of functions defined in compiled Python."""

    # The runtime supplies a function's real globals and closure cells
    # before class lookup (see function_attribute in the runtime), so these
    # getters only serve values with no native function behind them. Their
    # presence on the class is what types.py reads, as it reads __code__,
    # to name the descriptor types.
    @property
    def __globals__(self):
        return _generic_get_attribute(self, "__globals__")

    @property
    def __closure__(self):
        return _generic_get_attribute(self, "__closure__")

    @property
    def __code__(self):
        return _code_for(self)

    @__code__.setter
    def __code__(self, value):
        if not isinstance(value, code):
            raise TypeError("__code__ must be set to a code object")
        original = _code_for(self)
        if value is not original and value._native_origin is not original:
            raise NotImplementedError("transplanting native function code is not implemented")
        for index, (before, after) in enumerate(zip(original._metadata, value._metadata)):
            if index == 7:
                if (before ^ after) & ~0x100:
                    raise NotImplementedError("changing native execution flags is not implemented")
            elif before != after:
                raise NotImplementedError("changing native code layout or debug metadata is not implemented")
        _namespace_bind(_function_attributes(self), "__code__", value)

    @property
    def __defaults__(self):
        defaults = _function_positional_defaults(self)
        if defaults is not None:
            return defaults
        count = self.__code__._positional_defaults
        if not count:
            return None
        captured = _function_captured_defaults(self)
        defaults = tuple(captured[index] for index in range(count))
        _set_function_positional_defaults(self, defaults)
        return defaults

    @__defaults__.setter
    def __defaults__(self, value):
        _namespace_bind(_function_attributes(self), "__defaults__", value)

    @property
    def __kwdefaults__(self):
        defaults = _function_keyword_defaults(self)
        if defaults is not None:
            return defaults
        metadata = self.__code__
        names = metadata._keyword_defaults
        if not names:
            return None
        captured = _function_captured_defaults(self)
        defaults = {name: captured[metadata._positional_defaults + index]
                    for index, name in enumerate(names)}
        _set_function_keyword_defaults(self, defaults)
        return defaults

    @__kwdefaults__.setter
    def __kwdefaults__(self, value):
        _namespace_bind(_function_attributes(self), "__kwdefaults__", value)

    @property
    def __annotations__(self):
        annotations = _function_annotations(self)
        if annotations is None:
            annotate = _function_annotate(self)
            annotations = {} if annotate is None else annotate(1).__mule_annotation_mapping__()
            _set_function_annotations(self, annotations)
        return annotations

    @__annotations__.setter
    def __annotations__(self, value):
        if value is not None:
            value = value.__mule_annotation_mapping__()
        _set_function_annotations(self, value)
        _set_function_annotate(self, None)

    @__annotations__.deleter
    def __annotations__(self):
        _set_function_annotations(self, None)

    @property
    def __annotate__(self):
        return _function_annotate(self)

    @__annotate__.setter
    def __annotate__(self, value):
        if value is not None and not callable(value):
            raise TypeError("__annotate__ must be callable or None")
        _set_function_annotate(self, value)
        if value is not None:
            _set_function_annotations(self, None)

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


_builtin_type_cache = None


def _builtin_types():
    global _builtin_type_cache
    if _builtin_type_cache is None:
        import builtins
        _builtin_type_cache = tuple(
            owner for value in vars(builtins).values() if isinstance(value, type)
            for owner in value.__mro__
        )
    return _builtin_type_cache


class getset_descriptor(property):
    """A builtin attribute's named accessor, distinct from user properties."""

    def __init__(self, owner, name, accessor):
        property.__init__(self, accessor.fget, accessor.fset, accessor.fdel, accessor.__doc__)
        self.__objclass__ = owner
        self.__name__ = name
        self.__qualname__ = owner.__qualname__ + "." + name

    def __get__(self, instance, owner=None):
        if instance is None:
            return self
        self._check(instance)
        return property.__get__(self, instance, owner)

    def __set__(self, instance, value):
        self._check(instance)
        return property.__set__(self, instance, value)

    def __delete__(self, instance):
        self._check(instance)
        return property.__delete__(self, instance)

    def _check(self, instance):
        if not _is_instance(instance, self.__objclass__):
            raise TypeError(f"descriptor '{self.__name__}' for '{self.__objclass__.__name__}' objects doesn't apply to a '{_type_of(instance).__name__}' object")


class member_descriptor(getset_descriptor):
    """A builtin object's exposed stored field."""


class _SlotAccessor:
    def __init__(self, name):
        self.name = name

    def get(self, instance):
        return _instance_field(instance, self.name)

    def set(self, instance, value):
        _set_instance_field(instance, self.name, value)

    def delete(self, instance):
        _delete_instance_field(instance, self.name)


def _install_builtin_descriptors():
    global _slot_descriptors_ready
    _slot_descriptors_ready = True
    from _descriptors import _enable_metadata
    _enable_metadata()
    for owner, names in (
        (object, ("__class__", "__dict__")),
        (type, ("__name__", "__base__", "__bases__", "__mro__", "__dict__", "__annotations__")),
        (function, ("__code__", "__defaults__", "__kwdefaults__", "__annotations__", "__annotate__", "__dict__")),
    ):
        for name in names:
            setattr(owner, name, getset_descriptor(owner, name, _class_lookup(owner, name)))
    for owner, names in ((function, ("__globals__", "__closure__")), (method, ("__self__", "__func__"))):
        for name in names:
            setattr(owner, name, member_descriptor(owner, name, _class_lookup(owner, name)))


class builtin_function_or_method(object):
    """The class of functions the runtime implements natively."""

    def __get__(self, instance, owner=None):
        if instance is None:
            return self
        return _bound_method(self, instance)


class method(object):
    """A function bound to the instance it was read from."""

    def __new__(cls, function, instance, /):
        if not callable(function):
            raise TypeError('first argument must be callable')
        if instance is None:
            raise TypeError('instance must not be None')
        return _bound_method(function, instance)

    def __init__(self, function, instance, /):
        pass

    @property
    def __self__(self):
        return _bound_method_receiver(self)

    @property
    def __func__(self):
        return _bound_method_function(self)

    def __getattribute__(self, name):
        try:
            return _generic_get_attribute(self, name)
        except AttributeError:
            return getattr(_bound_method_function(self), name)


class module(object):
    """A module namespace."""

    def __init__(self, name, doc=None):
        self.__name__ = name
        self.__doc__ = doc
        self.__package__ = None
        self.__loader__ = None
        self.__spec__ = None

    def __getattribute__(self, name):
        try:
            return _generic_get_attribute(self, name)
        except AttributeError:
            hook = _namespace_binding(self, "__getattr__")
            if hook is None:
                raise
            return hook[0](name)

    @property
    def __dict__(self):
        if _namespace_items(self) is None:
            return _InstanceDict(self)
        return _GlobalsDict(self)

    def __dir__(self):
        namespace = self.__dict__
        if "__dir__" in namespace:
            return namespace["__dir__"]()
        return list(namespace)


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
        return _code_for(self, info)

    @property
    def f_lineno(self):
        return _traceback_field(self, 1)

    @property
    def f_globals(self):
        return _GlobalsDict(_traceback_field(self, 2))

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
