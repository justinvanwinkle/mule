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

__speed__ = 3
__mule_builtin_implementation__ = True

from _mule_native import allocate_instance
from _mule_native import default_construct
from _mule_native import bind_instance_attribute
from _mule_native import bound_method
from _mule_native import bound_method_receiver
from _mule_native import bound_method_function
from _mule_native import class_base
from _mule_native import class_bases
from _mule_native import set_class_bases
from _mule_native import class_subclasses
from _mule_native import class_lookup
from _mule_native import class_lookup_default
from _mule_native import class_mro
from _mule_native import class_name
from _mule_native import class_qualname
from _mule_native import class_bind
from _mule_native import class_own_names
from _mule_native import default_repr
from _mule_native import generic_get_attribute
from _mule_native import generic_set_attribute
from _mule_native import generic_delete_attribute
from _mule_native import identity_hash
from _mule_native import storage_hash
from _mule_native import instance_items
from _mule_native import instance_extra_items
from _mule_native import instance_set_binding
from _mule_native import instance_delete_binding
from _mule_native import instance_field
from _mule_native import set_instance_field
from _mule_native import delete_instance_field
from _mule_native import storage_dictionary
from _mule_native import is_class
from _mule_native import is_instance as _raw_is_instance
from _mule_native import is_subclass


def is_instance(value, cls):
    return is_subclass(type_of(value), cls)
from _mule_native import new_class
from _mule_native import namespace_items
from _mule_native import namespace_binding
from _mule_native import namespace_bind
from _mule_native import namespace_unbind
from _mule_native import math_cos
from _mule_native import math_exp
from _mule_native import math_float_bits
from _mule_native import math_float_format
from _mule_native import math_log
from _mule_native import math_sin
from _mule_native import math_sqrt
from _mule_native import numeric_negate
from _mule_native import numeric_invert
from _mule_native import integer_decimal
from _mule_native import math_fabs
from _mule_native import register_type
from _mule_native import traceback_field
from _mule_native import set_class_name
from _mule_native import set_class_qualname
from _mule_native import type_of
from _mule_native import typed_value
from _mule_native import function_attributes
from _mule_native import function_globals
from _mule_native import code_metadata
from _mule_native import code_cache
from _mule_native import set_code_cache
from _mule_native import function_captured_defaults
from _mule_native import function_positional_defaults
from _mule_native import set_function_positional_defaults
from _mule_native import function_keyword_defaults
from _mule_native import set_function_keyword_defaults
from _mule_native import function_annotate
from _mule_native import function_annotations
from _mule_native import set_function_annotate
from _mule_native import set_function_annotations
from _mule_native import byte_length
from _mule_native import read_byte
from _mule_native import mark_wrapper_descriptor
from _mule_native import weak_handle
from _mule_native import weak_target

# The runtime's conversion functions, captured before the classes below
# take over their builtin names.
_convert_int = int
_convert_float = float
_convert_bool = bool
_MISSING = _binding_missing()


def _bytes_special(value):
    """Apply __bytes__ once, distinguishing absence from a non-callable value."""
    missing = _binding_missing()
    owner = type_of(value)
    method = class_lookup_default(owner, "__bytes__", missing)
    if method is missing:
        return False, b""
    try:
        method = bind_instance_attribute(method, value, owner)
    except AttributeError:
        return False, b""
    result = method()
    if not is_instance(result, bytes):
        raise TypeError(f"__bytes__ returned non-bytes (type {type_of(result).__name__})")
    return True, result


def _index_integer(value):
    if type_of(value) is int:
        return _integer_storage(value)
    if is_instance(value, int):
        return _integer_storage(_convert_int(value))
    owner = type_of(value)
    missing = _binding_missing()
    method = class_lookup_default(owner, "__index__", missing)
    if method is missing:
        raise TypeError(f"'{owner.__name__}' object cannot be interpreted as an integer")
    result = bind_instance_attribute(method, value, owner)()
    if not is_instance(result, int):
        raise TypeError(f"__index__ returned non-int (type {type_of(result).__name__})")
    return _integer_storage(_convert_int(result))


def _integer_byteorder(value, operation):
    if not is_instance(value, str):
        name = "None" if value is None else type_of(value).__name__
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
    align = ">" if kind != "str" else "<"
    has_align = False
    sign_option = ""
    coerce_zero = False
    alternate = False
    zero = False
    width = 0
    group = ""
    precision = -1
    index = 0
    size = len(format_spec)
    if index + 1 < size and format_spec[index + 1] in "<>^=":
        fill = format_spec[index]
        align = format_spec[index + 1]
        has_align = True
        index += 2
    elif index < size and format_spec[index] in "<>^=":
        align = format_spec[index]
        has_align = True
        index += 1
    if index < size and format_spec[index] in "+- ":
        sign_option = format_spec[index]
        index += 1
    if index < size and format_spec[index] == "z":
        coerce_zero = True
        index += 1
    if index < size and format_spec[index] == "#":
        alternate = True
        index += 1
    if index < size and format_spec[index] == "0":
        zero = True
        if not has_align:
            fill = "0"
            align = "=" if kind != "str" else ">"
        index += 1
    while index < size and "0" <= format_spec[index] <= "9":
        width = width * 10 + "0123456789".index(format_spec[index])
        index += 1
    if index < size and format_spec[index] in ",_":
        group = format_spec[index]
        index += 1
    if index < size and format_spec[index] == ".":
        index += 1
        if index >= size or not ("0" <= format_spec[index] <= "9"):
            raise ValueError("Format specifier missing precision")
        precision = 0
        while index < size and "0" <= format_spec[index] <= "9":
            precision = precision * 10 + "0123456789".index(format_spec[index])
            index += 1
    conversion = format_spec[index:] if index < size else ""
    if len(conversion) > 1:
        raise ValueError(f"Invalid format specifier '{format_spec}' for object of type '{kind}'")
    if kind == "str":
        if align == "=":
            raise ValueError("'=' alignment not allowed in string format specifier")
        if sign_option:
            raise ValueError("Sign not allowed in string format specifier")
        if coerce_zero:
            raise ValueError("Negative zero coercion (z) not allowed in string format specifier")
        if alternate:
            raise ValueError("Alternate form (#) not allowed in string format specifier")
        if group:
            raise ValueError(f"Cannot specify '{group}' with 's'.")
    prefix = ""
    if kind == "float":
        codes = {"f": 0, "F": 7, "e": 1, "E": 2, "g": 3, "G": 4, "n": 3, "%": 5, "": 6}
        if conversion not in codes:
            raise ValueError("unknown format code '" + conversion + "' for object of type 'float'")
        val = _convert_float(value)
        if coerce_zero and val == 0.0:
            val = 0.0
        raw_prec = precision
        if precision < 0 and conversion != "":
            raw_prec = 6
        body = math_float_format(val, codes[conversion], raw_prec, alternate)
        if coerce_zero and body.startswith("-") and val == 0.0:
            body = body[1:]
        elif coerce_zero and body.startswith("-"):
            # Check if rounded value is all zero digits
            check_digits = body[1:]
            exp_idx = check_digits.find("e")
            if exp_idx < 0:
                exp_idx = check_digits.find("E")
            if exp_idx >= 0:
                check_digits = check_digits[:exp_idx]
            if check_digits.endswith("%"):
                check_digits = check_digits[:-1]
            all_zero = True
            for ch in check_digits:
                if ch != "0" and ch != ".":
                    all_zero = False
                    break
            if all_zero and check_digits:
                body = body[1:]
    elif conversion and conversion in "eEfFgG%" and kind == "int":
        codes = {"f": 0, "F": 7, "e": 1, "E": 2, "g": 3, "G": 4, "%": 5}
        if precision < 0:
            precision = 6
        val = _convert_float(value)
        if coerce_zero and val == 0.0:
            val = 0.0
        body = math_float_format(val, codes[conversion], precision, alternate)
        kind = "float"
    elif kind == "int":
        if coerce_zero:
            raise ValueError("Negative zero coercion (z) not allowed in integer format specifier")
        if precision >= 0:
            raise ValueError("Precision not allowed in integer format specifier")
        number = _convert_int(value)
        negative = number < 0
        if negative:
            number = -number
        if conversion in ("d", "n", ""):
            body = integer_decimal(number)
        elif conversion and conversion in "xXo":
            if group == ",":
                raise ValueError(f"Cannot specify ',' with '{conversion}'.")
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
            if group == ",":
                raise ValueError("Cannot specify ',' with 'b'.")
            body = ""
            if number == 0:
                body = "0"
            while number > 0:
                body = "01"[number % 2] + body
                number = number // 2
            if alternate:
                prefix = "0b"
        elif conversion == "c":
            if sign_option:
                raise ValueError("Sign not allowed with integer format specifier 'c'")
            if alternate:
                raise ValueError("Alternate form (#) not allowed with integer format specifier 'c'")
            if group:
                raise ValueError(f"Cannot specify '{group}' with 'c'.")
            if negative or value > 0x10FFFF:
                raise OverflowError("%c arg not in range(0x110000)")
            text = chr(value)
            if len(text) < width:
                pad = width - len(text)
                if align == "<":
                    text = text + fill * pad
                elif align == "^":
                    before = pad // 2
                    text = fill * before + text + fill * (pad - before)
                else:
                    text = fill * pad + text
            return text
        else:
            raise ValueError("unknown format code '" + conversion + "' for object of type 'int'")
        body = ("-" if negative else "") + body
    else:
        if conversion not in ("s", ""):
            raise ValueError("unknown format code '" + conversion + "' for object of type 'str'")
        body = value
        if precision >= 0:
            body = body[:precision]
    sign = ""
    digits = body
    if kind != "str":
        if digits[:1] == "-":
            sign = "-"
            digits = digits[1:]
        elif sign_option == "+":
            sign = "+"
        elif sign_option == " ":
            sign = " "
    if group and kind != "str":
        group_size = 4 if (kind == "int" and conversion in ("b", "o", "x", "X")) else 3
        exp_pos = digits.find("e")
        if exp_pos < 0:
            exp_pos = digits.find("E")
        dot = digits.find(".")
        pct = digits.endswith("%")
        if dot >= 0:
            head = digits[:dot]
            tail = digits[dot:]
        elif exp_pos >= 0:
            head = digits[:exp_pos]
            tail = digits[exp_pos:]
        elif pct:
            head = digits[:-1]
            tail = "%"
        else:
            head = digits
            tail = ""
        if head in ("inf", "INF", "nan", "NAN"):
            pass
        else:
            if zero and not has_align and width > 0:
                overhead = len(sign) + len(prefix) + len(tail)
                target_len = width - overhead
                # Group digits from right to left, padding with leading zeros up to target_len
                grouped = ""
                at = len(head)
                while at > 0:
                    start = at - group_size if at >= group_size else 0
                    piece = head[start:at]
                    grouped = piece + (group + grouped if grouped else "")
                    at = start
                while len(grouped) < target_len:
                    # Distance from next separator from the left
                    first_sep = grouped.find(group)
                    first_len = len(grouped) if first_sep < 0 else first_sep
                    if first_len == group_size:
                        grouped = "0" + group + grouped
                    else:
                        grouped = "0" + grouped
                digits = grouped + tail
            else:
                grouped = ""
                at = len(head)
                while at > 0:
                    start = at - group_size if at >= group_size else 0
                    piece = head[start:at]
                    grouped = piece + (group + grouped if grouped else "")
                    at = start
                digits = grouped + tail
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
            raise TypeError(f"{class_name(cls)}() takes no arguments")
        return allocate_instance(cls)

    def __init__(self, *args):
        cls = type_of(self)
        if args and (_overrides(cls, "__init__") or not _overrides(cls, "__new__")):
            raise TypeError(
                "object.__init__() takes exactly one argument (the instance to initialize)"
            )

    def __getattribute__(self, name):
        return generic_get_attribute(self, name)

    def __mule_annotation_mapping__(self):
        raise TypeError("__annotations__ must be a dict or None")

    def __dir__(self):
        try:
            names = set(self.__dict__)
        except AttributeError:
            names = set()
        for cls in class_mro(type_of(self)):
            names.update(class_own_names(cls))
        return list(names)

    def __mule_exit_status__(self):
        return (1, str(self))

    def __setattr__(self, name, value):
        missing = _binding_missing()
        descriptor = class_lookup_default(type_of(self), name, missing)
        if descriptor is missing:
            generic_set_attribute(self, name, value)
            return
        descriptor_type = type_of(descriptor)
        setter = class_lookup_default(descriptor_type, "__set__", missing)
        if setter is missing:
            if class_lookup_default(descriptor_type, "__delete__", missing) is missing:
                generic_set_attribute(self, name, value)
                return
            raise AttributeError(f"attribute {name!r} is read-only")
        bind_instance_attribute(setter, descriptor, descriptor_type)(self, value)

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
        missing = _binding_missing()
        descriptor = class_lookup_default(type_of(self), name, missing)
        if descriptor is missing:
            generic_delete_attribute(self, name)
            return
        descriptor_type = type_of(descriptor)
        deleter = class_lookup_default(descriptor_type, "__delete__", missing)
        if deleter is missing:
            if class_lookup_default(descriptor_type, "__set__", missing) is missing:
                generic_delete_attribute(self, name)
                return
            raise AttributeError(f"attribute {name!r} cannot be deleted")
        bind_instance_attribute(deleter, descriptor, descriptor_type)(self)

    # The runtime applies these defaults itself when a class inherits them
    # (see ClassLayout::special); they exist for explicit calls such as
    # ``object.__repr__(self)`` and must match that built-in behavior.
    def __repr__(self):
        return default_repr(self)

    def __str__(self):
        return type_of(self).__repr__(self)

    def __format__(self, format_spec):
        if format_spec:
            raise TypeError(
                f"unsupported format string passed to {class_name(type_of(self))}.__format__"
            )
        return str(self)

    def __hash__(self):
        return identity_hash(self)

    def __eq__(self, other):
        return True if self is other else NotImplemented

    def __ne__(self, other):
        owner = type_of(self)
        method = class_lookup(owner, '__eq__')
        result = bind_instance_attribute(method, self, owner)(other)
        return result if result is NotImplemented else not result

    def __lt__(self, other):
        return NotImplemented

    def __le__(self, other):
        return NotImplemented

    def __gt__(self, other):
        return NotImplemented

    def __ge__(self, other):
        return NotImplemented

    def __init_subclass__(cls):
        pass

    def __subclasshook__(cls, subclass, /):
        return NotImplemented


register_type("object", object)


def _overrides(cls, name):
    """Whether ``cls`` defines ``name`` somewhere other than ``object``."""
    return class_lookup(cls, name) is not class_lookup(object, name)


_slot_descriptors_ready = False


def _declared_slots(name, namespace):
    raw_slots = namespace.get('__slots__', ())
    if isinstance(raw_slots, str):
        slots = (raw_slots,)
    else:
        slots = raw_slots
    result = []
    all_slots = []
    for slot in slots:
        if not isinstance(slot, str):
            raise TypeError("__slots__ items must be strings, not '" + type(slot).__name__ + "'")
        if not slot.isidentifier():
            raise TypeError('__slots__ must be identifiers')
        if slot in ('__dict__', '__weakref__'):
            all_slots.append(slot)
            continue
        if slot in namespace:
            raise ValueError("'" + slot + "' in __slots__ conflicts with class variable")
        if slot.startswith('__') and not slot.endswith('__'):
            prefix = name.lstrip('_')
            if prefix:
                slot = '_' + prefix + slot
        result.append(slot)
        all_slots.append(slot)
    return result, tuple(all_slots)


class type(object):
    """The class of classes: ``type(value)`` is a value's class, and
    ``type(name, bases, namespace)`` creates a class."""

    def __call__(cls, *args, **kwargs):
        if not is_class(cls):
            raise TypeError('type.__call__ requires a class')
        return default_construct(cls, tuple(args), tuple(kwargs), tuple(kwargs.values()))

    def __new__(metaclass, *args, **keywords):
        if len(args) == 1 and metaclass is type and not keywords:
            return type_of(args[0])
        if len(args) != 3:
            raise TypeError("type() takes 1 or 3 arguments")
        name, bases, namespace = args
        if not is_instance(name, str):
            raise TypeError("type.__new__() argument 1 must be str")
        if not is_instance(bases, tuple):
            raise TypeError("type.__new__() argument 2 must be tuple")
        original_bases = bases
        resolved = []
        changed = False
        for base in bases:
            if is_class(base):
                resolved.append(base)
                continue
            method = getattr(base, "__mro_entries__", None)
            if method is None:
                resolved.append(base)
                continue
            entries = method(bases)
            if not is_instance(entries, tuple):
                raise TypeError("__mro_entries__ must return a tuple")
            changed = True
            for entry in entries:
                resolved.append(entry)
        bases = tuple(resolved)
        if _slot_descriptors_ready:
            for base in bases:
                if base is bool or base is NoneType:
                    raise TypeError(
                        f"type '{class_name(base)}' is not an acceptable base type"
                    )
        if '__slots__' in namespace:
            slots, raw_slot_tuple = _declared_slots(name, namespace)
        else:
            slots, raw_slot_tuple = [], None
        fields = list(namespace.get('__mule_fields__', ()))
        for slot in slots:
            if slot not in fields:
                fields.append(slot)
        names, values = [], []
        if raw_slot_tuple is not None:
            names.append("__mule_slots__")
            values.append(raw_slot_tuple)
        for key in namespace:
            value = namespace[key]
            if key == "__mule_fields__":
                continue
            # These hooks are implicit class/static methods (CPython's type_new).
            if key in ("__init_subclass__", "__class_getitem__") and is_instance(
                value, function
            ):
                value = classmethod(value)
            elif key == "__new__" and is_instance(value, function):
                value = staticmethod(value)
            names.append(key)
            values.append(value)
        if changed and "__orig_bases__" not in namespace:
            names.append("__orig_bases__")
            values.append(original_bases)
        # The metaclass of the new class is the most derived metaclass of
        # the bases, as type_new computes it; it starts as the metaclass
        # this call was made through (type itself for a plain type() call).
        winner = metaclass
        for base in bases:
            candidate = type_of(base)
            if is_subclass(candidate, winner):
                winner = candidate
            elif not is_subclass(winner, candidate):
                raise TypeError(
                    "metaclass conflict: the metaclass of a derived class "
                    "must be a (non-strict) subclass of the metaclasses "
                    "of all its bases"
                )
        if winner is not metaclass:
            winner_new = class_lookup(winner, "__new__")
            if winner_new is not class_lookup(type, "__new__"):
                return winner.__new__(
                    winner, name, original_bases, namespace, **keywords
                )
        cls = new_class(
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
                class_bind(cls, slot, member_descriptor(cls, slot, property(accessor.get, accessor.set, accessor.delete)))
        complete_class(cls, raw_slot_tuple)
        index = 0
        while index < len(names):
            hook = getattr(type_of(values[index]), "__set_name__", None)
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
        return list(class_mro(cls))

    def __instancecheck__(cls, instance):
        return _raw_is_instance(instance, cls)

    def __subclasscheck__(cls, subclass):
        if not is_class(subclass):
            raise TypeError("issubclass() arg 1 must be a class")
        return is_subclass(subclass, cls)

    def __subclasses__(cls):
        return list(class_subclasses(cls))

    def __dir__(cls):
        names = set()
        for base in class_mro(cls):
            names.update(class_own_names(base))
        return list(names)


register_type("type", type)

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
    if "__annotations__" in class_own_names(cls):
        annotations = _namespace(cls)["__annotations__"]
        if annotations is not None:
            return annotations
    if "__annotate__" in class_own_names(cls):
        annotate = _namespace(cls)["__annotate__"]
        if annotate is not None:
            annotations = annotate(1).__mule_annotation_mapping__()
            generic_set_attribute(cls, "__annotations__", annotations)
            return annotations
    annotations = {}
    generic_set_attribute(cls, "__annotations__", annotations)
    return annotations


def _class_annotate(cls):
    if "__annotate__" in class_own_names(cls):
        return _namespace(cls)["__annotate__"]
    return None


def _class_type_params(cls):
    if "__type_params__" in class_own_names(cls):
        return _namespace(cls)["__type_params__"]
    return ()


def _set_class_annotations(cls, value):
    generic_set_attribute(cls, "__annotations__", value)


def _del_class_annotations(cls):
    generic_delete_attribute(cls, "__annotations__")


def _set_class_annotate(cls, value):
    generic_set_attribute(cls, "__annotate__", value)


def _set_class_type_params(cls, value):
    generic_set_attribute(cls, "__type_params__", value)


type.__annotations__ = property(
    _class_annotations, _set_class_annotations, _del_class_annotations
)
type.__annotate__ = property(_class_annotate, _set_class_annotate)
type.__type_params__ = property(_class_type_params, _set_class_type_params)


def _bases(cls):
    return class_bases(cls)


def _set_bases(cls, bases):
    # type_set_bases: argument checks here, layout checks in set_class_bases.
    name = class_name(cls)
    if cls.__module__ == "builtins":
        raise TypeError(f"cannot set '__bases__' attribute of immutable type '{name}'")
    if type_of(bases) is not tuple:
        raise TypeError(
            f"can only assign tuple to {name}.__bases__, not {type_of(bases).__name__}"
        )
    if not bases:
        raise TypeError(f"can only assign non-empty tuple to {name}.__bases__, not ()")
    for base in bases:
        if not is_class(base):
            raise TypeError(
                f"{name}.__bases__ must be tuple of classes, not '{type_of(base).__name__}'"
            )
    set_class_bases(cls, bases)


def _namespace(cls):
    return mappingproxy(_ClassDict(cls))


# These fill the builtin types' own namespaces directly, as PyType_Ready does;
# an assignment would run the data descriptors type already has.
class_bind(object, "__class__", property(type_of))
class_bind(type, "__name__", property(class_name, set_class_name))
class_bind(type, "__qualname__", property(class_qualname, set_class_qualname))
class_bind(type, "__base__", property(class_base))
class_bind(type, "__bases__", property(_bases, _set_bases))
class_bind(type, "__mro__", property(class_mro))
class_bind(type, "__dict__", property(_namespace))


class _InstanceDict(object):
    """An instance's ``__dict__``: a live view of its attributes."""

    __slots__ = ("_owner",)

    def __init__(self, owner):
        self._owner = owner

    def __getitem__(self, key):
        for name, value in instance_items(self._owner):
            if name == key:
                return value
        raise KeyError(key)

    def __setitem__(self, key, value):
        instance_set_binding(self._owner, key, value)

    def __delitem__(self, key):
        if not instance_delete_binding(self._owner, key):
            raise KeyError(key)

    def __contains__(self, key):
        for name, value in instance_items(self._owner):
            if name == key:
                return True
        return False

    def __iter__(self):
        return iter(self.keys())

    def __len__(self):
        return len(instance_items(self._owner))

    def keys(self):
        return [name for name, value in instance_items(self._owner)]

    def values(self):
        return [value for name, value in instance_items(self._owner)]

    def items(self):
        return list(instance_items(self._owner))

    def get(self, key, default=None):
        for name, value in instance_items(self._owner):
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

    def clear(self):
        for name, value in instance_items(self._owner):
            instance_delete_binding(self._owner, name)

    def copy(self):
        return dict(instance_items(self._owner))

    def __eq__(self, other):
        return self.copy() == other

    def __repr__(self):
        return repr(self.copy())


_GLOBALS_DICTS = None
_INSTANCE_DICTS = None


def _instance_dict(owner):
    global _INSTANCE_DICTS
    if _INSTANCE_DICTS is None:
        _INSTANCE_DICTS = {}
    key = identity_hash(owner)
    bucket = _INSTANCE_DICTS.get(key)
    if bucket is not None:
        live = []
        found = None
        for handle in bucket:
            wrapper = weak_target(handle)
            if wrapper is not None:
                live.append(handle)
                if wrapper._owner is owner:
                    found = wrapper
        if found is not None:
            _INSTANCE_DICTS[key] = live
            return found
        bucket = live
    wrapper = _InstanceDict(owner)
    handle = weak_handle(wrapper, None, None)
    if bucket is None:
        _INSTANCE_DICTS[key] = [handle]
    else:
        bucket.append(handle)
        _INSTANCE_DICTS[key] = bucket
    return wrapper


def _globals_dict(namespace):
    global _GLOBALS_DICTS
    if namespace is None:
        return None
    if namespace_items(namespace) is None:
        return namespace
    # _object loads before the dictionary protocol it depends on.
    if _GLOBALS_DICTS is None:
        _GLOBALS_DICTS = {}
    key = identity_hash(namespace)
    bucket = _GLOBALS_DICTS.get(key)
    if bucket is not None:
        live = []
        found = None
        for handle in bucket:
            wrapper = weak_target(handle)
            if wrapper is not None:
                live.append(handle)
                if wrapper._namespace is namespace:
                    found = wrapper
        if found is not None:
            _GLOBALS_DICTS[key] = live
            return found
        bucket = live
    wrapper = _GlobalsDict(namespace)
    handle = weak_handle(wrapper, None, None)
    if bucket is None:
        _GLOBALS_DICTS[key] = [handle]
    else:
        bucket.append(handle)
        _GLOBALS_DICTS[key] = bucket
    return wrapper


def _object_dictionary(owner):
    namespace = storage_dictionary(owner)
    if namespace is None:
        return _instance_dict(owner)
    if is_instance(namespace, dict):
        return namespace
    return _globals_dict(namespace)


def _replace_dictionary(dictionary, value):
    if not (
        is_instance(value, dict)
        or is_instance(value, _InstanceDict)
        or is_instance(value, _GlobalsDict)
    ):
        raise TypeError(
            f"__dict__ must be set to a dictionary, not a '{type_of(value).__name__}'"
        )
    entries = list(value.items())
    dictionary.clear()
    for key, item in entries:
        dictionary[key] = item


def _set_object_dictionary(owner, value):
    _replace_dictionary(_object_dictionary(owner), value)


def _delete_object_dictionary(owner):
    _object_dictionary(owner).clear()


class_bind(
    object,
    "__dict__",
    property(_object_dictionary, _set_object_dictionary, _delete_object_dictionary),
)


def _first_weak_reference(owner):
    # _weakref imports this module, so it is imported when first used.
    from _weakref import getweakrefs

    references = getweakrefs(owner)
    return references[0] if references else None


def _declares_instance_storage(owner, name):
    """Whether ``owner`` itself gives its instances ``name`` storage.

    A class adds an instance ``__dict__`` or ``__weakref__`` when it defines
    that name and its ``__slots__``, if any, request it: type_new's
    ``add_dict`` and ``add_weak``, recorded here by the namespace entry that
    ``complete_class`` binds or the class body defines. ``object`` binds a
    ``__dict__`` accessor without giving its own instances one.
    """
    if owner is object:
        return False
    names = class_own_names(owner)
    if name not in names:
        return False
    if "__slots__" not in names:
        return True
    slots = class_lookup(owner, "__slots__")
    return slots == name or not is_instance(slots, str) and name in slots


def complete_class(cls, declared_slots):
    """Fill a new class's own namespace as CPython's PyType_Ready does.

    After the class body's entries and slot members come the ``__dict__``
    and ``__weakref__`` accessors the class introduces (``type_add_getset``)
    and then ``__doc__`` when the body defined none (``type_dict_set_doc``).
    Entries the body already defined are kept. ``declared_slots`` is the
    class's ``__slots__`` as a tuple, or None when it declared none.
    """
    if not _slot_descriptors_ready:
        return
    ancestors = class_mro(cls)[1:]
    if declared_slots is None:
        wants_dict = wants_weak = True
    else:
        wants_dict = "__dict__" in declared_slots
        wants_weak = "__weakref__" in declared_slots
    names = class_own_names(cls)
    if (
        wants_dict
        and "__dict__" not in names
        and not any(_declares_instance_storage(owner, "__dict__") for owner in ancestors)
    ):
        accessor = property(
            _object_dictionary, _set_object_dictionary, _delete_object_dictionary
        )
        class_bind(cls, "__dict__", getset_descriptor(cls, "__dict__", accessor))
    # Variable-size builtins cannot carry a weak reference list (CPython
    # requires tp_itemsize == 0); the others already have one.
    weak_capable = not any(
        owner in (int, tuple, bytes)
        or owner in (type, function, builtin_function_or_method, method, code, set, frozenset)
        or _declares_instance_storage(owner, "__weakref__")
        for owner in ancestors
    )
    if wants_weak and "__weakref__" not in names and weak_capable:
        accessor = property(_first_weak_reference)
        class_bind(cls, "__weakref__", getset_descriptor(cls, "__weakref__", accessor))
    if "__doc__" not in names:
        class_bind(cls, "__doc__", None)


def vars(*args):
    if len(args) > 1:
        raise TypeError(f"vars() takes at most 1 argument ({len(args)} given)")
    if not args:
        raise TypeError("vars() without an argument is not supported yet")
    try:
        return args[0].__dict__
    except AttributeError:
        raise TypeError("vars() argument must have __dict__ attribute") from None


def globals(namespace):
    # The compiler supplies the calling function's namespace as the one
    # argument, as it supplies the implicit arguments of super(); see
    # _CallTarget.visit_Name in the compiler.
    return _globals_dict(namespace)


def locals():
    raise RuntimeError("locals() requires a frame")


class _GlobalsDict(object):
    """The live dict of a module's globals: reads and writes go to the
    module namespace itself, so it behaves as ``globals()`` must."""

    def __init__(self, namespace):
        self._namespace = namespace

    def __getitem__(self, key):
        for name, value in namespace_items(self._namespace):
            if name == key:
                return value
        raise KeyError(key)

    def __setitem__(self, key, value):
        namespace_bind(self._namespace, key, value)

    def __delitem__(self, key):
        if not namespace_unbind(self._namespace, key):
            raise KeyError(key)

    def __contains__(self, key):
        for name, value in namespace_items(self._namespace):
            if name == key:
                return True
        return False

    def __iter__(self):
        return iter(self.keys())

    def __len__(self):
        return len(namespace_items(self._namespace))

    def keys(self):
        return [name for name, value in namespace_items(self._namespace)]

    def values(self):
        return [value for name, value in namespace_items(self._namespace)]

    def items(self):
        return list(namespace_items(self._namespace))

    def get(self, key, default=None):
        for name, value in namespace_items(self._namespace):
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
        for name, value in namespace_items(self._namespace):
            if name == key:
                namespace_unbind(self._namespace, key)
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

    def clear(self):
        for name, value in namespace_items(self._namespace):
            namespace_unbind(self._namespace, name)

    def copy(self):
        return dict(namespace_items(self._namespace))

    def __getattr__(self, name):
        for key, value in namespace_items(self._namespace):
            if key == name:
                return value
        raise AttributeError(f"'_GlobalsDict' object has no attribute {name!r}")

    def __eq__(self, other):
        return self.copy() == other

    def __repr__(self):
        return repr(self.copy())


class _ClassDict(object):
    """Read the class's own current bindings, without descriptor binding."""

    def __init__(self, owner):
        self._owner = owner

    def __getitem__(self, key):
        if key not in class_own_names(self._owner):
            raise KeyError(key)
        return class_lookup(self._owner, key)

    def __contains__(self, key):
        return key in class_own_names(self._owner)

    def __iter__(self):
        return iter(self.keys())

    def __len__(self):
        return len(self.keys())

    def keys(self):
        return class_own_names(self._owner)

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

    __module__ = "builtins"

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
    __module__ = "builtins"

    def __new__(cls):
        return None

    def __getattribute__(self, name):
        if name == "__class__":
            return type_of(self)
        return generic_get_attribute(self, name)

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


def _coerce_complex_arg(value, try_complex):
    parts = _complex_parts(value)
    if parts is not None:
        return parts
    owner = type_of(value)
    missing = _MISSING
    if try_complex:
        method = class_lookup_default(owner, "__complex__", missing)
        if method is not missing:
            res = bind_instance_attribute(method, value, owner)()
            if not is_instance(res, complex):
                raise TypeError(f"__complex__ returned non-complex (type {type_of(res).__name__})")
            return res.real, res.imag
    for name, expected in (("__float__", float), ("__index__", int)):
        method = class_lookup_default(owner, name, missing)
        if method is not missing:
            res = bind_instance_attribute(method, value, owner)()
            if not is_instance(res, expected):
                raise TypeError(f"{name} returned non-{expected.__name__} (type {type_of(res).__name__})")
            return _convert_float(res), 0.0
    return None


def _atan_unit(t):
    if t > 0.41421356237309503:
        s = (t - 1.0) / (t + 1.0)
        offset = 0.7853981633974483
    else:
        s = t
        offset = 0.0
    s2 = s * s
    poly = -0.3333333333333333 + s2 * (
        0.2
        + s2
        * (
            -0.14285714285714285
            + s2
            * (
                0.1111111111111111
                + s2
                * (
                    -0.09090909090909091
                    + s2
                    * (
                        0.07692307692307693
                        + s2
                        * (
                            -0.06666666666666667
                            + s2
                            * (
                                0.058823529411764705
                                + s2 * (-0.05263157894736842 + s2 * 0.047619047619047616)
                            )
                        )
                    )
                )
            )
        )
    )
    return offset + s + s * s2 * poly


def _atan2(y, x):
    ay = math_fabs(y)
    ax = math_fabs(x)
    if ax == 0.0 and ay == 0.0:
        angle = 0.0 if x >= 0.0 else 3.141592653589793
    elif ax >= ay:
        angle = _atan_unit(ay / ax)
        if x < 0.0:
            angle = 3.141592653589793 - angle
    else:
        angle = 1.5707963267948966 - _atan_unit(ax / ay)
        if x < 0.0:
            angle = 3.141592653589793 - angle
    return -angle if (math_float_bits(y) >> 63) else angle


def _complex_pow(ar, ai, br, bi):
    if br == 0.0 and bi == 0.0:
        return 1.0, 0.0
    if ar == 0.0 and ai == 0.0:
        if bi != 0.0 or br < 0.0:
            raise ZeroDivisionError("0.0 cannot be raised to a negative or complex power")
        return 0.0, 0.0
    if bi == 0.0 and -100.0 <= br <= 100.0:
        exp_int = _convert_int(br)
        if _convert_float(exp_int) == br:
            negative = exp_int < 0
            n = -exp_int if negative else exp_int
            rr, ri = 1.0, 0.0
            cr, ci = ar, ai
            while n > 0:
                if n & 1:
                    rr, ri = rr * cr - ri * ci, rr * ci + ri * cr
                cr, ci = cr * cr - ci * ci, 2.0 * cr * ci
                n >>= 1
            if negative:
                denom = rr * rr + ri * ri
                if denom == 0.0:
                    raise ZeroDivisionError("complex division by zero")
                rr, ri = rr / denom, -ri / denom
            return rr, ri
    vabs = math_sqrt(ar * ar + ai * ai)
    at = _atan2(ai, ar)
    length = vabs ** br
    phase = at * br
    if bi != 0.0:
        length = length * math_exp(-at * bi)
        phase = phase + bi * math_log(vabs)
    return length * math_cos(phase), length * math_sin(phase)


class complex(object):
    """A complex number, stored as two float fields."""

    def __new__(cls, real=_MISSING, imag=_MISSING):
        has_imag = imag is not _MISSING
        if real is _MISSING:
            real = 0.0
        if not has_imag:
            imag = 0.0
        if isinstance(real, str):
            if has_imag:
                raise TypeError("complex() can't take second arg if first is a string")
            return cls._parse(real)
        if isinstance(imag, str):
            raise TypeError("complex() second arg can't be a string")
        parts = _coerce_complex_arg(real, True)
        if parts is None:
            raise TypeError("complex() first argument must be a string or a number")
        real_part, imag_part = parts
        if has_imag:
            imag_parts = _coerce_complex_arg(imag, True)
            if imag_parts is None:
                raise TypeError("complex() second argument must be a number")
            if imag_parts[1] != 0.0 or (math_float_bits(imag_parts[1]) >> 63) != 0:
                real_part = real_part - imag_parts[1]
            if imag_part == 0.0 and (math_float_bits(imag_part) >> 63) == 0:
                imag_part = imag_parts[0]
            else:
                imag_part = imag_part + imag_parts[0]
        instance = allocate_instance(cls)
        instance._real = _convert_float(real_part)
        instance._imag = _convert_float(imag_part)
        return instance

    @classmethod
    def _parse(cls, text):
        value = text.strip()
        if value.startswith("(") and value.endswith(")"):
            value = value[1:-1].strip()
        for ch in value:
            if ch in " \t\n\r\v\f":
                raise ValueError("complex() arg is a malformed string")
        if not value:
            raise ValueError("complex() arg is a malformed string")
        if not value.endswith(("j", "J")):
            instance = allocate_instance(cls)
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
        instance = allocate_instance(cls)
        instance._real = _convert_float(real_part)
        instance._imag = _convert_float(imag_part)
        return instance

    @property
    def real(self):
        if not is_instance(self, complex):
            raise TypeError(f"descriptor 'real' for 'complex' objects doesn't apply to a '{type_of(self).__name__}' object")
        return self._real

    @property
    def imag(self):
        if not is_instance(self, complex):
            raise TypeError(f"descriptor 'imag' for 'complex' objects doesn't apply to a '{type_of(self).__name__}' object")
        return self._imag

    def conjugate(self):
        if not is_instance(self, complex):
            raise TypeError(f"descriptor 'conjugate' for 'complex' objects doesn't apply to a '{type_of(self).__name__}' object")
        return _make_complex(complex, self._real, -self._imag)

    def __complex__(self):
        if not is_instance(self, complex):
            raise TypeError(f"descriptor '__complex__' requires a 'complex' object but received a '{type_of(self).__name__}'")
        if type_of(self) is complex:
            return self
        return _make_complex(complex, self._real, self._imag)

    def __add__(self, other):
        if not is_instance(self, complex):
            raise TypeError(f"descriptor '__add__' requires a 'complex' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, complex) and (is_instance(other, int) or is_instance(other, float)):
            return _make_complex(complex, self._real + _convert_float(other), self._imag)
        parts = _complex_parts(other)
        if parts is None:
            return NotImplemented
        return _make_complex(complex, self._real + parts[0], self._imag + parts[1])

    def __radd__(self, other):
        if not is_instance(self, complex):
            raise TypeError(f"descriptor '__radd__' requires a 'complex' object but received a '{type_of(self).__name__}'")
        return self.__add__(other)

    def __sub__(self, other):
        if not is_instance(self, complex):
            raise TypeError(f"descriptor '__sub__' requires a 'complex' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, complex) and (is_instance(other, int) or is_instance(other, float)):
            return _make_complex(complex, self._real - _convert_float(other), self._imag)
        parts = _complex_parts(other)
        if parts is None:
            return NotImplemented
        return _make_complex(complex, self._real - parts[0], self._imag - parts[1])

    def __rsub__(self, other):
        if not is_instance(self, complex):
            raise TypeError(f"descriptor '__rsub__' requires a 'complex' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, complex) and (is_instance(other, int) or is_instance(other, float)):
            return _make_complex(complex, _convert_float(other) - self._real, -self._imag)
        parts = _complex_parts(other)
        if parts is None:
            return NotImplemented
        return _make_complex(complex, parts[0] - self._real, parts[1] - self._imag)

    def __mul__(self, other):
        if not is_instance(self, complex):
            raise TypeError(f"descriptor '__mul__' requires a 'complex' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, complex) and (is_instance(other, int) or is_instance(other, float)):
            scalar = _convert_float(other)
            return _make_complex(complex, self._real * scalar, self._imag * scalar)
        parts = _complex_parts(other)
        if parts is None:
            return NotImplemented
        return _make_complex(
            complex,
            self._real * parts[0] - self._imag * parts[1],
            self._real * parts[1] + self._imag * parts[0],
        )

    def __rmul__(self, other):
        if not is_instance(self, complex):
            raise TypeError(f"descriptor '__rmul__' requires a 'complex' object but received a '{type_of(self).__name__}'")
        return self.__mul__(other)

    def __truediv__(self, other):
        if not is_instance(self, complex):
            raise TypeError(f"descriptor '__truediv__' requires a 'complex' object but received a '{type_of(self).__name__}'")
        parts = _complex_parts(other)
        if parts is None:
            return NotImplemented
        denominator = parts[0] * parts[0] + parts[1] * parts[1]
        if denominator == 0:
            raise ZeroDivisionError("complex division by zero")
        return _make_complex(
            complex,
            (self._real * parts[0] + self._imag * parts[1]) / denominator,
            (self._imag * parts[0] - self._real * parts[1]) / denominator,
        )

    def __rtruediv__(self, other):
        if not is_instance(self, complex):
            raise TypeError(f"descriptor '__rtruediv__' requires a 'complex' object but received a '{type_of(self).__name__}'")
        parts = _complex_parts(other)
        if parts is None:
            return NotImplemented
        return _make_complex(complex, parts[0], parts[1]).__truediv__(self)

    def __pow__(self, other, mod=None):
        if not is_instance(self, complex):
            raise TypeError(f"descriptor '__pow__' requires a 'complex' object but received a '{type_of(self).__name__}'")
        if mod is not None:
            raise ValueError("complex modulo")
        parts = _complex_parts(other)
        if parts is None:
            return NotImplemented
        rr, ri = _complex_pow(self._real, self._imag, parts[0], parts[1])
        return _make_complex(complex, rr, ri)

    def __rpow__(self, other, mod=None):
        if not is_instance(self, complex):
            raise TypeError(f"descriptor '__rpow__' requires a 'complex' object but received a '{type_of(self).__name__}'")
        if mod is not None:
            raise ValueError("complex modulo")
        parts = _complex_parts(other)
        if parts is None:
            return NotImplemented
        rr, ri = _complex_pow(parts[0], parts[1], self._real, self._imag)
        return _make_complex(complex, rr, ri)

    def __neg__(self):
        if not is_instance(self, complex):
            raise TypeError(f"descriptor '__neg__' requires a 'complex' object but received a '{type_of(self).__name__}'")
        return _make_complex(complex, -self._real, -self._imag)

    def __pos__(self):
        if not is_instance(self, complex):
            raise TypeError(f"descriptor '__pos__' requires a 'complex' object but received a '{type_of(self).__name__}'")
        return _make_complex(complex, self._real, self._imag)

    def __abs__(self):
        if not is_instance(self, complex):
            raise TypeError(f"descriptor '__abs__' requires a 'complex' object but received a '{type_of(self).__name__}'")
        return math_sqrt(self._real * self._real + self._imag * self._imag)

    def __bool__(self):
        if not is_instance(self, complex):
            raise TypeError(f"descriptor '__bool__' requires a 'complex' object but received a '{type_of(self).__name__}'")
        return self._real != 0 or self._imag != 0

    def __eq__(self, other):
        if not is_instance(self, complex):
            raise TypeError(f"descriptor '__eq__' requires a 'complex' object but received a '{type_of(self).__name__}'")
        parts = _complex_parts(other)
        if parts is None:
            return NotImplemented
        return self._real == parts[0] and self._imag == parts[1]

    def __ne__(self, other):
        if not is_instance(self, complex):
            raise TypeError(f"descriptor '__ne__' requires a 'complex' object but received a '{type_of(self).__name__}'")
        result = self.__eq__(other)
        return result if result is NotImplemented else not result

    def __hash__(self):
        if not is_instance(self, complex):
            raise TypeError(f"descriptor '__hash__' requires a 'complex' object but received a '{type_of(self).__name__}'")
        real_hash = hash(self._real)
        imag_hash = hash(self._imag)
        combined = (
            (real_hash & 0xFFFFFFFFFFFFFFFF)
            + 1000003 * (imag_hash & 0xFFFFFFFFFFFFFFFF)
        ) & 0xFFFFFFFFFFFFFFFF
        if combined >= 1 << 63:
            combined -= 1 << 64
        return -2 if combined == -1 else combined

    def __format__(self, format_spec=""):
        if not is_instance(self, complex):
            raise TypeError(f"descriptor '__format__' requires a 'complex' object but received a '{type_of(self).__name__}'")
        if format_spec == "":
            return str(self)
        fill = " "
        align = ">"
        has_align = False
        zero = False
        width = 0
        index = 0
        size = len(format_spec)
        if index + 1 < size and format_spec[index + 1] in "<>^=":
            fill = format_spec[index]
            align = format_spec[index + 1]
            has_align = True
            index += 2
        elif index < size and format_spec[index] in "<>^=":
            align = format_spec[index]
            has_align = True
            index += 1
        if align == "=":
            raise ValueError("'=' alignment flag is not allowed in complex format specifier")
        sign_part = ""
        if index < size and format_spec[index] in "+- ":
            sign_part = format_spec[index]
            index += 1
        z_part = ""
        if index < size and format_spec[index] == "z":
            z_part = "z"
            index += 1
        alt_part = ""
        if index < size and format_spec[index] == "#":
            alt_part = "#"
            index += 1
        if index < size and format_spec[index] == "0":
            raise ValueError("Zero padding is not allowed in complex format specifier")
        while index < size and "0" <= format_spec[index] <= "9":
            width = width * 10 + "0123456789".index(format_spec[index])
            index += 1
        rest = format_spec[index:]
        conversion = rest[-1:] if rest and rest[-1:] not in "0123456789.,_" else ""
        if conversion == "%":
            raise ValueError("unknown format code '%' for object of type 'complex'")
        if not conversion and not rest and not sign_part and not z_part and not alt_part:
            body = repr(self)
        else:
            real_spec = sign_part + z_part + alt_part + rest
            imag_sign = "+"
            imag_spec = imag_sign + z_part + alt_part + rest
            real_text = _format_number(self._real, "float", real_spec)
            imag_text = _format_number(self._imag, "float", imag_spec)
            if not conversion and self._real == 0.0 and (math_float_bits(self._real) >> 63) == 0:
                imag_only_spec = sign_part + z_part + alt_part + rest
                body = _format_number(self._imag, "float", imag_only_spec) + "j"
            elif not conversion:
                body = "(" + real_text + imag_text + "j)"
            else:
                body = real_text + imag_text + "j"
        if len(body) < width:
            pad = width - len(body)
            if align == "<":
                body = body + fill * pad
            elif align == "^":
                before = pad // 2
                body = fill * before + body + fill * (pad - before)
            else:
                body = fill * pad + body
        return body

    def __repr__(self):
        if not is_instance(self, complex):
            raise TypeError(f"descriptor '__repr__' requires a 'complex' object but received a '{type_of(self).__name__}'")
        real_text = repr(self._real)
        if real_text.endswith(".0"):
            real_text = real_text[:-2]
        imag_text = repr(self._imag)
        if imag_text.endswith(".0"):
            imag_text = imag_text[:-2]
        if self._real == 0.0 and real_text == "0":
            return imag_text + "j"
        sign = "" if imag_text.startswith("-") else "+"
        return "(" + real_text + sign + imag_text + "j)"


def _make_complex(cls, real, imag):
    instance = allocate_instance(cls)
    instance._real = _convert_float(real)
    instance._imag = _convert_float(imag)
    return instance


def integer_hash(value):
    """Hash integer storage with Python's signed numeric-hash modulus."""
    value = _integer_storage(value)
    modulus = 2305843009213693951
    if value > -modulus and value < modulus:
        return -2 if value == -1 else value
    result = (-value if value < 0 else value) % modulus
    if value < 0:
        result = -result
    return -2 if result == -1 else result


class int(object):
    def __hash__(self):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__hash__' requires a 'int' object but received a '{type_of(self).__name__}'")
        return integer_hash(self)

    def __repr__(self):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__repr__' requires a 'int' object but received a '{type_of(self).__name__}'")
        return integer_decimal(self)

    def __pos__(self):
        return _convert_int(self)

    def __neg__(self):
        return numeric_negate(_convert_int(self))

    def __invert__(self):
        return numeric_invert(_convert_int(self))

    def __abs__(self):
        value = _convert_int(self)
        return numeric_negate(value) if value < 0 else value

    def __bool__(self):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__bool__' requires a 'int' object but received a '{type_of(self).__name__}'")
        return _convert_int(self) != 0
    __slots__ = ()

    def __new__(cls, x=_MISSING, /, base=_MISSING):
        if base is _MISSING and cls is int and x is not _MISSING:
            if _text_storage(x):
                return _convert_int(x)
            if _type_of(x) is int:
                return x
            if _type_of(x) is bool:
                return 1 if x else 0
        if x is _MISSING:
            if base is not _MISSING:
                raise TypeError("int() missing string argument")
            value = 0
        elif base is _MISSING:
            # Latin-1 preserves every byte so invalid input reaches the integer
            # parser as ValueError rather than failing UTF-8 decoding first.
            if isinstance(x, bytes):
                x = bytes.decode(x, "latin-1")
            elif isinstance(x, bytearray):
                x = bytearray.decode(x, "latin-1")
            elif is_instance(x, int):
                owner = type_of(x)
                missing = _MISSING
                method = class_lookup_default(owner, "__int__", missing)
                if method is not missing and method is not class_lookup_default(int, "__int__", missing):
                    res = bind_instance_attribute(method, x, owner)()
                    if not is_instance(res, int):
                        raise TypeError(f"__int__ returned non-int (type {type_of(res).__name__})")
                    x = _convert_int(res)
                else:
                    x = _convert_int(x)
            elif not is_instance(x, str):
                owner = type_of(x)
                missing = _MISSING
                converted = False
                for hook in ("__int__", "__index__"):
                    method = class_lookup_default(owner, hook, missing)
                    if method is not missing:
                        res = bind_instance_attribute(method, x, owner)()
                        if not is_instance(res, int):
                            raise TypeError(f"{hook} returned non-int (type {type_of(res).__name__})")
                        x = _convert_int(res)
                        converted = True
                        break
                if not converted:
                    raise TypeError(
                        f"int() argument must be a string, a bytes-like object or a real number, not '{owner.__name__}'"
                    )
            value = _convert_int(x)
        else:
            if isinstance(x, bytes):
                x = bytes.decode(x, "latin-1")
            elif isinstance(x, bytearray):
                x = bytearray.decode(x, "latin-1")
            elif not is_instance(x, str):
                raise TypeError("int() can't convert non-string with explicit base")
            value = _convert_int(x, _index_integer(base))
        if cls is not int and cls is not bool:
            return typed_value(value, cls)
        return value

    def __format__(self, format_spec=""):
        if format_spec == "":
            return str(self)
        return _format_number(self, "int", format_spec)

    def __index__(self):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__index__' requires a 'int' object but received a '{type_of(self).__name__}'")
        return _convert_int(self)

    def __int__(self):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__int__' requires a 'int' object but received a '{type_of(self).__name__}'")
        return _convert_int(self)

    def __float__(self):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__float__' requires a 'int' object but received a '{type_of(self).__name__}'")
        return _convert_float(self)

    def __round__(self, ndigits=None):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__round__' requires a 'int' object but received a '{type_of(self).__name__}'")
        value = _convert_int(self)
        if ndigits is None:
            return value
        ndigits = _index_integer(ndigits)
        if ndigits >= 0:
            return value
        scale = 10 ** -ndigits
        quotient, remainder = value // scale, value % scale
        doubled = remainder * 2
        if doubled > scale or (doubled == scale and quotient % 2):
            quotient += 1
        return quotient * scale

    def __add__(self, other):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__add__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        return _convert_int(self) + _convert_int(other)

    def __radd__(self, other):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__radd__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        return _convert_int(other) + _convert_int(self)

    def __sub__(self, other):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__sub__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        return _convert_int(self) - _convert_int(other)

    def __rsub__(self, other):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__rsub__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        return _convert_int(other) - _convert_int(self)

    def __mul__(self, other):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__mul__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        return _convert_int(self) * _convert_int(other)

    def __rmul__(self, other):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__rmul__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        return _convert_int(other) * _convert_int(self)

    def __truediv__(self, other):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__truediv__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        return _convert_int(self) / _convert_int(other)

    def __rtruediv__(self, other):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__rtruediv__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        return _convert_int(other) / _convert_int(self)

    def __floordiv__(self, other):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__floordiv__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        return _convert_int(self) // _convert_int(other)

    def __rfloordiv__(self, other):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__rfloordiv__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        return _convert_int(other) // _convert_int(self)

    def __mod__(self, other):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__mod__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        return _convert_int(self) % _convert_int(other)

    def __rmod__(self, other):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__rmod__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        return _convert_int(other) % _convert_int(self)

    def __pow__(self, other, mod=None):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__pow__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        if mod is None:
            return _convert_int(self) ** _convert_int(other)
        if not is_instance(mod, int):
            return NotImplemented
        return pow(_convert_int(self), _convert_int(other), _convert_int(mod))

    def __rpow__(self, other, mod=None):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__rpow__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        if mod is None:
            return _convert_int(other) ** _convert_int(self)
        if not is_instance(mod, int):
            return NotImplemented
        return pow(_convert_int(other), _convert_int(self), _convert_int(mod))

    def __lshift__(self, other):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__lshift__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        return _convert_int(self) << _convert_int(other)

    def __rlshift__(self, other):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__rlshift__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        return _convert_int(other) << _convert_int(self)

    def __rshift__(self, other):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__rshift__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        return _convert_int(self) >> _convert_int(other)

    def __rrshift__(self, other):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__rrshift__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        return _convert_int(other) >> _convert_int(self)

    def __and__(self, other):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__and__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        return _convert_int(self) & _convert_int(other)

    def __rand__(self, other):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__rand__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        return _convert_int(other) & _convert_int(self)

    def __or__(self, other):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__or__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        return _convert_int(self) | _convert_int(other)

    def __ror__(self, other):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__ror__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        return _convert_int(other) | _convert_int(self)

    def __xor__(self, other):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__xor__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        return _convert_int(self) ^ _convert_int(other)

    def __rxor__(self, other):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__rxor__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        return _convert_int(other) ^ _convert_int(self)

    def __divmod__(self, other):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__divmod__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
            return NotImplemented
        left, right = _convert_int(self), _convert_int(other)
        if right == 0:
            raise ZeroDivisionError('division by zero')
        return (left // right, left % right)

    def __rdivmod__(self, other):
        if not is_instance(self, int):
            raise TypeError(f"descriptor '__rdivmod__' requires a 'int' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int):
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
        elif is_instance(data, int) or is_instance(data, str):
            raise TypeError(f"cannot convert '{type_of(data).__name__}' object to bytes")
        if is_instance(data, bytes):
            count = byte_length(data)
            value = 0
            for index in range(count):
                position = index if big_endian else count - index - 1
                value = (value << 8) | read_byte(data, position)
            if signed and count and read_byte(data, 0 if big_endian else count - 1) & 0x80:
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
        return _convert_int(self)

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
        if not is_instance(self, int):
            raise TypeError(f"descriptor 'bit_length' for 'int' objects doesn't apply to a '{type_of(self).__name__}' object")
        value = -self if self < 0 else self
        length = 0
        while value:
            value >>= 1
            length += 1
        return length

    def bit_count(self):
        if not is_instance(self, int):
            raise TypeError(f"descriptor 'bit_count' for 'int' objects doesn't apply to a '{type_of(self).__name__}' object")
        value = -_convert_int(self) if self < 0 else _convert_int(self)
        count = 0
        while value:
            value &= value - 1
            count += 1
        return count

    def as_integer_ratio(self):
        if not is_instance(self, int):
            raise TypeError(f"descriptor 'as_integer_ratio' for 'int' objects doesn't apply to a '{type_of(self).__name__}' object")
        return (_convert_int(self), 1)

    def is_integer(self):
        if not is_instance(self, int):
            raise TypeError(f"descriptor 'is_integer' for 'int' objects doesn't apply to a '{type_of(self).__name__}' object")
        return True

    def conjugate(self):
        if not is_instance(self, int):
            raise TypeError(f"descriptor 'conjugate' for 'int' objects doesn't apply to a '{type_of(self).__name__}' object")
        return _convert_int(self)

    @property
    def real(self):
        if not is_instance(self, int):
            raise TypeError(f"descriptor 'real' for 'int' objects doesn't apply to a '{type_of(self).__name__}' object")
        return _convert_int(self)

    @property
    def imag(self):
        if not is_instance(self, int):
            raise TypeError(f"descriptor 'imag' for 'int' objects doesn't apply to a '{type_of(self).__name__}' object")
        return 0

    @property
    def numerator(self):
        if not is_instance(self, int):
            raise TypeError(f"descriptor 'numerator' for 'int' objects doesn't apply to a '{type_of(self).__name__}' object")
        return _convert_int(self)

    @property
    def denominator(self):
        if not is_instance(self, int):
            raise TypeError(f"descriptor 'denominator' for 'int' objects doesn't apply to a '{type_of(self).__name__}' object")
        return 1


class bool(int):
    __slots__ = ()

    def __new__(cls, *args):
        count = _storage_length(args)
        if count == 0:
            return False
        if count > 1:
            raise TypeError(f"bool expected at most 1 argument, got {count}")
        return True if _storage_read(args, 0) else False

    def __repr__(self):
        if not is_instance(self, bool):
            raise TypeError(f"descriptor '__repr__' requires a 'bool' object but received a '{type_of(self).__name__}'")
        return "True" if self else "False"

    def __and__(self, other):
        if not is_instance(self, bool):
            raise TypeError(f"descriptor '__and__' requires a 'bool' object but received a '{type_of(self).__name__}'")
        if is_instance(other, bool):
            return bool(self and other)
        return int.__and__(self, other)

    def __rand__(self, other):
        if not is_instance(self, bool):
            raise TypeError(f"descriptor '__rand__' requires a 'bool' object but received a '{type_of(self).__name__}'")
        if is_instance(other, bool):
            return bool(other and self)
        return int.__rand__(self, other)

    def __or__(self, other):
        if not is_instance(self, bool):
            raise TypeError(f"descriptor '__or__' requires a 'bool' object but received a '{type_of(self).__name__}'")
        if is_instance(other, bool):
            return bool(self or other)
        return int.__or__(self, other)

    def __ror__(self, other):
        if not is_instance(self, bool):
            raise TypeError(f"descriptor '__ror__' requires a 'bool' object but received a '{type_of(self).__name__}'")
        if is_instance(other, bool):
            return bool(other or self)
        return int.__ror__(self, other)

    def __xor__(self, other):
        if not is_instance(self, bool):
            raise TypeError(f"descriptor '__xor__' requires a 'bool' object but received a '{type_of(self).__name__}'")
        if is_instance(other, bool):
            return bool(self) is not bool(other)
        return int.__xor__(self, other)

    def __rxor__(self, other):
        if not is_instance(self, bool):
            raise TypeError(f"descriptor '__rxor__' requires a 'bool' object but received a '{type_of(self).__name__}'")
        if is_instance(other, bool):
            return bool(other) is not bool(self)
        return int.__rxor__(self, other)


_FLOAT_INFINITY = _convert_float('inf')


def _round_int_ldexp(mantissa, exp, negative):
    if mantissa == 0:
        return -0.0 if negative else 0.0
    bl = mantissa.bit_length()
    top_exp = exp + bl - 1
    if top_exp > 1023:
        raise OverflowError("hexadecimal value too large to represent as a float")
    if top_exp < -1075:
        return -0.0 if negative else 0.0
    target_lsb = top_exp - 52 if top_exp >= -1022 else -1074
    shift = exp - target_lsb
    if shift >= 0:
        scaled = mantissa << shift
    else:
        rshift = -shift
        half = 1 << (rshift - 1)
        rem = mantissa & ((1 << rshift) - 1)
        scaled = mantissa >> rshift
        if rem > half or (rem == half and (scaled & 1)):
            scaled += 1
        if top_exp >= -1022 and scaled >= (1 << 53):
            scaled >>= 1
            top_exp += 1
            target_lsb += 1
            if top_exp > 1023:
                raise OverflowError("hexadecimal value too large to represent as a float")
    result = _convert_float(scaled) * (2.0 ** target_lsb) if target_lsb >= -1022 else (_convert_float(scaled) * (2.0 ** -1022)) * (2.0 ** (target_lsb + 1022))
    return -result if negative else result


def _round_ratio_to_float(num, den, negative):
    if num == 0:
        return -0.0 if negative else 0.0
    q, r = num // den, num % den
    if q == 0 and r == 0:
        return -0.0 if negative else 0.0
    # Find floor(log2(num / den))
    est = num.bit_length() - den.bit_length()
    if est >= 0:
        if num < (den << est):
            est -= 1
    else:
        if (num << -est) < den:
            est -= 1
    if est > 1023:
        raise OverflowError("rounded value too large to represent")
    if est < -1075:
        return -0.0 if negative else 0.0
    target_lsb = est - 52 if est >= -1022 else -1074
    if target_lsb >= 0:
        scaled_num = num
        scaled_den = den << target_lsb
    else:
        scaled_num = num << -target_lsb
        scaled_den = den
    sig, rem = scaled_num // scaled_den, scaled_num % scaled_den
    doubled = rem * 2
    if doubled > scaled_den or (doubled == scaled_den and (sig & 1)):
        sig += 1
    if est >= -1022 and sig >= (1 << 53):
        sig >>= 1
        est += 1
        target_lsb += 1
        if est > 1023:
            raise OverflowError("rounded value too large to represent")
    result = (
        _convert_float(sig) * (2.0 ** target_lsb)
        if target_lsb >= -1022
        else (_convert_float(sig) * (2.0 ** -1022)) * (2.0 ** (target_lsb + 1022))
    )
    return -result if negative else result


class float(object):
    def __hash__(self):
        if not is_instance(self, float):
            raise TypeError(f"descriptor '__hash__' requires a 'float' object but received a '{type_of(self).__name__}'")
        # The primitive copies binary64 storage without invoking __float__.
        value = _convert_float(self)
        if value != value:
            return identity_hash(self)
        return storage_hash(value)

    def __repr__(self):
        if not is_instance(self, float):
            raise TypeError(f"descriptor '__repr__' requires a 'float' object but received a '{type_of(self).__name__}'")
        return _render_representation(_convert_float(self))

    def __int__(self):
        if not is_instance(self, float):
            raise TypeError(f"descriptor '__int__' requires a 'float' object but received a '{type_of(self).__name__}'")
        value = _convert_float(self)
        if value != value:
            raise ValueError('cannot convert float NaN to integer')
        if value == _FLOAT_INFINITY or value == -_FLOAT_INFINITY:
            raise OverflowError('cannot convert float infinity to integer')
        return _convert_int(value)

    def __pos__(self):
        return _convert_float(self)

    def __neg__(self):
        return numeric_negate(_convert_float(self))

    def __abs__(self):
        return math_fabs(_convert_float(self))

    def __bool__(self):
        if not is_instance(self, float):
            raise TypeError(f"descriptor '__bool__' requires a 'float' object but received a '{type_of(self).__name__}'")
        return _convert_float(self) != 0.0
    __slots__ = ()

    def __new__(cls, x=_MISSING, /):
        if x is _MISSING:
            value = 0.0
        elif cls is float and _type_of(x) is float:
            return x
        elif isinstance(x, bytes):
            value = _convert_float(bytes.decode(x, "latin-1"))
        elif isinstance(x, bytearray):
            value = _convert_float(bytearray.decode(x, "latin-1"))
        elif is_instance(x, float):
            owner = type_of(x)
            missing = _MISSING
            method = class_lookup_default(owner, "__float__", missing)
            if method is not missing and method is not class_lookup_default(float, "__float__", missing):
                res = bind_instance_attribute(method, x, owner)()
                if not is_instance(res, float):
                    raise TypeError(f"__float__ returned non-float (type {type_of(res).__name__})")
                value = _convert_float(res)
            else:
                value = _convert_float(x)
        elif is_instance(x, int) or is_instance(x, str):
            value = _convert_float(x)
        else:
            owner = type_of(x)
            missing = _MISSING
            converted = False
            for hook, expected in (("__float__", float), ("__index__", int)):
                method = class_lookup_default(owner, hook, missing)
                if method is not missing:
                    res = bind_instance_attribute(method, x, owner)()
                    if not is_instance(res, expected):
                        raise TypeError(f"{hook} returned non-{expected.__name__} (type {type_of(res).__name__})")
                    value = _convert_float(res)
                    converted = True
                    break
            if not converted:
                raise TypeError(
                    f"float() argument must be a string or a real number, not '{owner.__name__}'"
                )
        if cls is not float:
            return typed_value(value, cls)
        return value

    def __format__(self, format_spec=""):
        if format_spec == "":
            return str(self)
        return _format_number(self, "float", format_spec)

    def __float__(self):
        if not is_instance(self, float):
            raise TypeError(f"descriptor '__float__' requires a 'float' object but received a '{type_of(self).__name__}'")
        return _convert_float(self)

    @classmethod
    def fromhex(cls, string, /):
        if not is_instance(string, str):
            raise TypeError(f"fromhex() argument must be a str, not {type_of(string).__name__}")
        s = string.strip()
        if not s:
            raise ValueError("invalid hexadecimal floating-point string")
        negative = False
        if s[0] in "+-":
            negative = s[0] == "-"
            s = s[1:]
        lower = s.lower()
        if lower in ("inf", "infinity"):
            value = -_FLOAT_INFINITY if negative else _FLOAT_INFINITY
            return typed_value(value, cls) if cls is not float else value
        if lower == "nan":
            value = _convert_float("-nan" if negative else "nan")
            return typed_value(value, cls) if cls is not float else value
        if lower.startswith("0x"):
            s = s[2:]
        if "p" in s or "P" in s:
            p_idx = s.find("p") if "p" in s else s.find("P")
            mant_str = s[:p_idx]
            exp_str = s[p_idx + 1 :]
            if not exp_str or exp_str in ("+", "-"):
                raise ValueError("invalid hexadecimal floating-point string")
            for ch in exp_str[1:] if exp_str[0] in "+-" else exp_str:
                if not ("0" <= ch <= "9"):
                    raise ValueError("invalid hexadecimal floating-point string")
            exp_val = _convert_int(exp_str)
        else:
            mant_str = s
            exp_val = 0
        if "." in mant_str:
            dot = mant_str.find(".")
            if "." in mant_str[dot + 1 :]:
                raise ValueError("invalid hexadecimal floating-point string")
            int_part = mant_str[:dot]
            frac_part = mant_str[dot + 1 :]
        else:
            int_part = mant_str
            frac_part = ""
        if not int_part and not frac_part:
            raise ValueError("invalid hexadecimal floating-point string")
        hex_digits = int_part + frac_part
        mantissa = 0
        for ch in hex_digits:
            pos = "0123456789abcdefABCDEF".find(ch)
            if pos < 0:
                raise ValueError("invalid hexadecimal floating-point string")
            digit = pos if pos < 16 else pos - 6
            mantissa = (mantissa << 4) | digit
        total_exp = exp_val - 4 * len(frac_part)
        value = _round_int_ldexp(mantissa, total_exp, negative)
        return typed_value(value, cls) if cls is not float else value

    def hex(self, /):
        if not is_instance(self, float):
            raise TypeError(f"descriptor 'hex' for 'float' objects doesn't apply to a '{type_of(self).__name__}' object")
        bits = math_float_bits(self)
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
        if not is_instance(self, float):
            raise TypeError(f"descriptor 'as_integer_ratio' for 'float' objects doesn't apply to a '{type_of(self).__name__}' object")
        bits = math_float_bits(self)
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
        if not is_instance(self, float):
            raise TypeError(f"descriptor 'is_integer' for 'float' objects doesn't apply to a '{type_of(self).__name__}' object")
        bits = math_float_bits(self)
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
        if not is_instance(self, float):
            raise TypeError(f"descriptor '__round__' requires a 'float' object but received a '{type_of(self).__name__}'")
        value = _convert_float(self)
        if ndigits is None:
            if value != value:
                raise ValueError("cannot convert float NaN to integer")
            if value == _FLOAT_INFINITY or value == -_FLOAT_INFINITY:
                raise OverflowError("cannot convert float infinity to integer")
            whole = _convert_int(value)
            fraction = value - whole
            if fraction > 0.5 or (fraction == 0.5 and whole % 2):
                whole += 1
            elif fraction < -0.5 or (fraction == -0.5 and whole % 2):
                whole -= 1
            return whole
        ndigits = _index_integer(ndigits)
        if value != value or value == _FLOAT_INFINITY or value == -_FLOAT_INFINITY or value == 0.0:
            return value
        bits = math_float_bits(value)
        negative = (bits >> 63) != 0
        if ndigits > 325:
            return value
        if ndigits < -310:
            return -0.0 if negative else 0.0
        num, den = self.as_integer_ratio()
        if num < 0:
            num = -num
        if ndigits >= 0:
            scale = 10 ** ndigits
            num *= scale
        else:
            scale = 10 ** -ndigits
            den *= scale
        quotient, remainder = num // den, num % den
        doubled = remainder * 2
        if doubled > den or (doubled == den and (quotient & 1)):
            quotient += 1
        if quotient == 0:
            return -0.0 if negative else 0.0
        if ndigits >= 0:
            return _round_ratio_to_float(quotient, scale, negative)
        return _round_ratio_to_float(quotient * scale, 1, negative)

    def __trunc__(self):
        return self.__int__()

    def __floor__(self):
        value = self.__int__()
        return value - 1 if value > _convert_float(self) else value

    def __ceil__(self):
        value = self.__int__()
        return value + 1 if value < _convert_float(self) else value

    def __add__(self, other):
        if not is_instance(self, float):
            raise TypeError(f"descriptor '__add__' requires a 'float' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int) and not is_instance(other, float):
            return NotImplemented
        return _convert_float(self) + _convert_float(other)

    def __radd__(self, other):
        if not is_instance(self, float):
            raise TypeError(f"descriptor '__radd__' requires a 'float' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int) and not is_instance(other, float):
            return NotImplemented
        return _convert_float(other) + _convert_float(self)

    def __sub__(self, other):
        if not is_instance(self, float):
            raise TypeError(f"descriptor '__sub__' requires a 'float' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int) and not is_instance(other, float):
            return NotImplemented
        return _convert_float(self) - _convert_float(other)

    def __rsub__(self, other):
        if not is_instance(self, float):
            raise TypeError(f"descriptor '__rsub__' requires a 'float' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int) and not is_instance(other, float):
            return NotImplemented
        return _convert_float(other) - _convert_float(self)

    def __mul__(self, other):
        if not is_instance(self, float):
            raise TypeError(f"descriptor '__mul__' requires a 'float' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int) and not is_instance(other, float):
            return NotImplemented
        return _convert_float(self) * _convert_float(other)

    def __rmul__(self, other):
        if not is_instance(self, float):
            raise TypeError(f"descriptor '__rmul__' requires a 'float' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int) and not is_instance(other, float):
            return NotImplemented
        return _convert_float(other) * _convert_float(self)

    def __truediv__(self, other):
        if not is_instance(self, float):
            raise TypeError(f"descriptor '__truediv__' requires a 'float' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int) and not is_instance(other, float):
            return NotImplemented
        return _convert_float(self) / _convert_float(other)

    def __rtruediv__(self, other):
        if not is_instance(self, float):
            raise TypeError(f"descriptor '__rtruediv__' requires a 'float' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int) and not is_instance(other, float):
            return NotImplemented
        return _convert_float(other) / _convert_float(self)

    def __floordiv__(self, other):
        if not is_instance(self, float):
            raise TypeError(f"descriptor '__floordiv__' requires a 'float' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int) and not is_instance(other, float):
            return NotImplemented
        return _convert_float(self) // _convert_float(other)

    def __rfloordiv__(self, other):
        if not is_instance(self, float):
            raise TypeError(f"descriptor '__rfloordiv__' requires a 'float' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int) and not is_instance(other, float):
            return NotImplemented
        return _convert_float(other) // _convert_float(self)

    def __mod__(self, other):
        if not is_instance(self, float):
            raise TypeError(f"descriptor '__mod__' requires a 'float' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int) and not is_instance(other, float):
            return NotImplemented
        return _convert_float(self) % _convert_float(other)

    def __rmod__(self, other):
        if not is_instance(self, float):
            raise TypeError(f"descriptor '__rmod__' requires a 'float' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int) and not is_instance(other, float):
            return NotImplemented
        return _convert_float(other) % _convert_float(self)

    def __pow__(self, other, mod=None):
        if not is_instance(self, float):
            raise TypeError(f"descriptor '__pow__' requires a 'float' object but received a '{type_of(self).__name__}'")
        if mod is not None:
            raise TypeError("pow() 3rd argument not allowed unless all arguments are integers")
        if not is_instance(other, int) and not is_instance(other, float):
            return NotImplemented
        return _convert_float(self) ** _convert_float(other)

    def __rpow__(self, other, mod=None):
        if not is_instance(self, float):
            raise TypeError(f"descriptor '__rpow__' requires a 'float' object but received a '{type_of(self).__name__}'")
        if mod is not None:
            return NotImplemented
        if not is_instance(other, int) and not is_instance(other, float):
            return NotImplemented
        return _convert_float(other) ** _convert_float(self)

    def __divmod__(self, other):
        if not is_instance(self, float):
            raise TypeError(f"descriptor '__divmod__' requires a 'float' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int) and not is_instance(other, float):
            return NotImplemented
        left, right = _convert_float(self), _convert_float(other)
        if right == 0.0:
            raise ZeroDivisionError('division by zero')
        return (left // right, left % right)

    def __rdivmod__(self, other):
        if not is_instance(self, float):
            raise TypeError(f"descriptor '__rdivmod__' requires a 'float' object but received a '{type_of(self).__name__}'")
        if not is_instance(other, int) and not is_instance(other, float):
            return NotImplemented
        left, right = _convert_float(other), _convert_float(self)
        if right == 0.0:
            raise ZeroDivisionError('division by zero')
        return (left // right, left % right)

    def conjugate(self):
        if not is_instance(self, float):
            raise TypeError(f"descriptor 'conjugate' for 'float' objects doesn't apply to a '{type_of(self).__name__}' object")
        return _convert_float(self)

    @property
    def real(self):
        if not is_instance(self, float):
            raise TypeError(f"descriptor 'real' for 'float' objects doesn't apply to a '{type_of(self).__name__}' object")
        return _convert_float(self)

    @property
    def imag(self):
        if not is_instance(self, float):
            raise TypeError(f"descriptor 'imag' for 'float' objects doesn't apply to a '{type_of(self).__name__}' object")
        return 0.0


class code(object):
    """A function's compiled-code metadata."""

    __module__ = "builtins"

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
    cached = code_cache(receiver)
    if cached is not None:
        return cached
    metadata = code_metadata(receiver)
    if metadata is None:
        if fallback is None:
            raise AttributeError("native callable has no Python code metadata")
        cached = code(fallback[0], fallback[2], fallback[1], fallback[3])
    else:
        cached = code(*metadata)
    set_code_cache(receiver, cached)
    return cached


class function(object):
    """The class of functions defined in compiled Python."""

    __module__ = "builtins"

    # The runtime supplies a function's real globals and closure cells
    # before class lookup (see function_attribute in the runtime), so these
    # getters only serve values with no native function behind them. Their
    # presence on the class is what types.py reads, as it reads __code__,
    # to name the descriptor types.
    @property
    def __globals__(self):
        attrs = function_attributes(self)
        if attrs is not None:
            for key, value in namespace_items(attrs):
                if key == "__globals__":
                    return value
        return _globals_dict(function_globals(self))

    @property
    def __builtins__(self):
        globals_dict = self.__globals__
        if "__builtins__" in globals_dict:
            builtins_val = globals_dict["__builtins__"]
            if is_instance(builtins_val, dict):
                return builtins_val
            return builtins_val.__dict__
        import builtins
        return builtins.__dict__

    @property
    def __type_params__(self):
        return ()

    @__type_params__.setter
    def __type_params__(self, value):
        if not is_instance(value, tuple):
            raise TypeError(f"__type_params__ must be set to a tuple, not {type_of(value).__name__}")
        namespace_bind(function_attributes(self), "__type_params__", value)

    @property
    def __closure__(self):
        return generic_get_attribute(self, "__closure__")

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
        set_code_cache(self, value)
        namespace_bind(function_attributes(self), "__code__", value)

    @property
    def __defaults__(self):
        defaults = function_positional_defaults(self)
        if defaults is not None:
            return defaults
        count = self.__code__._positional_defaults
        if not count:
            return None
        captured = function_captured_defaults(self)
        defaults = tuple(captured[index] for index in range(count))
        set_function_positional_defaults(self, defaults)
        return defaults

    @__defaults__.setter
    def __defaults__(self, value):
        if value is not None and not is_instance(value, tuple):
            raise TypeError(f"__defaults__ must be set to a tuple object")
        set_function_positional_defaults(self, value)

    @__defaults__.deleter
    def __defaults__(self):
        set_function_positional_defaults(self, None)

    @property
    def __kwdefaults__(self):
        defaults = function_keyword_defaults(self)
        if defaults is not None:
            return defaults
        metadata = self.__code__
        names = metadata._keyword_defaults
        if not names:
            return None
        captured = function_captured_defaults(self)
        defaults = {name: captured[metadata._positional_defaults + index]
                    for index, name in enumerate(names)}
        set_function_keyword_defaults(self, defaults)
        return defaults

    @__kwdefaults__.setter
    def __kwdefaults__(self, value):
        if value is not None and not is_instance(value, dict):
            raise TypeError(f"__kwdefaults__ must be set to a dict object")
        set_function_keyword_defaults(self, value)

    @__kwdefaults__.deleter
    def __kwdefaults__(self):
        set_function_keyword_defaults(self, None)

    @property
    def __annotations__(self):
        annotations = function_annotations(self)
        if annotations is None:
            annotate = function_annotate(self)
            annotations = {} if annotate is None else annotate(1).__mule_annotation_mapping__()
            set_function_annotations(self, annotations)
        return annotations

    @__annotations__.setter
    def __annotations__(self, value):
        if value is not None:
            value = value.__mule_annotation_mapping__()
        set_function_annotations(self, value)
        set_function_annotate(self, None)

    @__annotations__.deleter
    def __annotations__(self):
        set_function_annotations(self, None)

    @property
    def __annotate__(self):
        return function_annotate(self)

    @__annotate__.setter
    def __annotate__(self, value):
        if value is not None and not callable(value):
            raise TypeError("__annotate__ must be callable or None")
        set_function_annotate(self, value)
        if value is not None:
            set_function_annotations(self, None)

    @property
    def __dict__(self):
        return _GlobalsDict(function_attributes(self))

    @__dict__.setter
    def __dict__(self, value):
        _replace_dictionary(_GlobalsDict(function_attributes(self)), value)

    def __get__(self, instance, owner=None):
        # Functions are descriptors: reading one through an instance binds
        # it, and through a class returns the function itself. Attribute
        # lookup applies this natively; this method exists for code that
        # looks the descriptor protocol up by name.
        if instance is None:
            return self
        return bound_method(self, instance)

    def __call__(self, *args, **kwargs):
        return self(*args, **kwargs)


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

    __module__ = "builtins"

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
        if not is_instance(instance, self.__objclass__):
            raise TypeError(f"descriptor '{self.__name__}' for '{self.__objclass__.__name__}' objects doesn't apply to a '{type_of(instance).__name__}' object")


class member_descriptor(getset_descriptor):
    """A builtin object's exposed stored field."""

    __module__ = "builtins"


class _SlotAccessor:
    def __init__(self, name):
        self.name = name

    def get(self, instance):
        return instance_field(instance, self.name)

    def set(self, instance, value):
        set_instance_field(instance, self.name, value)

    def delete(self, instance):
        delete_instance_field(instance, self.name)


def _install_builtin_descriptors():
    global _slot_descriptors_ready
    _slot_descriptors_ready = True
    from _descriptors import _enable_metadata
    _enable_metadata()
    for owner in (object, type, int, bool, float, method, str, bytes, bytearray, tuple, list, dict, set, frozenset, memoryview):
        if "__new__" in class_own_names(owner):
            value = class_lookup(owner, "__new__")
            if is_instance(value, function):
                setattr(owner, "__new__", staticmethod(value))
    for owner, names in (
        (object, ("__class__", "__dict__")),
        (type, ("__name__", "__qualname__", "__base__", "__bases__", "__mro__", "__dict__", "__annotations__")),
        (function, ("__code__", "__defaults__", "__kwdefaults__", "__annotations__", "__annotate__", "__dict__")),
    ):
        for name in names:
            class_bind(owner, name, getset_descriptor(owner, name, class_lookup(owner, name)))
    for owner, names in ((function, ("__globals__", "__closure__")), (method, ("__self__", "__func__"))):
        for name in names:
            class_bind(owner, name, member_descriptor(owner, name, class_lookup(owner, name)))
    for name in (
        "__init__",
        "__repr__",
        "__str__",
        "__format__",
        "__hash__",
        "__eq__",
        "__ne__",
        "__lt__",
        "__le__",
        "__gt__",
        "__ge__",
        "__getattribute__",
        "__setattr__",
        "__delattr__",
        "__dir__",
    ):
        if name in class_own_names(object):
            mark_wrapper_descriptor(class_lookup(object, name))


class wrapper_descriptor(object):
    """The class of slot wrapper descriptors on builtin types such as ``object.__init__``."""

    __module__ = "builtins"

    def __get__(self, instance, owner=None):
        if instance is None:
            return self
        return bound_method(self, instance)

    def __call__(self, *args, **kwargs):
        return self(*args, **kwargs)


class builtin_function_or_method(object):
    """The class of functions the runtime implements natively."""

    __module__ = "builtins"

    def __get__(self, instance, owner=None):
        if instance is None:
            return self
        return bound_method(self, instance)

    def __call__(self, *args, **kwargs):
        return self(*args, **kwargs)


class method(object):
    """A function bound to the instance it was read from."""

    __module__ = "builtins"

    def __new__(cls, function, instance, /):
        if not callable(function):
            raise TypeError('first argument must be callable')
        if instance is None:
            raise TypeError('instance must not be None')
        return bound_method(function, instance)

    def __init__(self, function, instance, /):
        pass

    @property
    def __self__(self):
        return bound_method_receiver(self)

    @property
    def __func__(self):
        return bound_method_function(self)

    @property
    def __doc__(self):
        return bound_method_function(self).__doc__

    def __getattribute__(self, name):
        try:
            return generic_get_attribute(self, name)
        except AttributeError:
            return getattr(bound_method_function(self), name)

    def __call__(self, *args, **kwargs):
        return self(*args, **kwargs)

    def __eq__(self, other):
        if type_of(other) is not method:
            return NotImplemented
        return (
            bound_method_function(self) == bound_method_function(other)
            and bound_method_receiver(self) == bound_method_receiver(other)
        )

    def __ne__(self, other):
        if type_of(other) is not method:
            return NotImplemented
        return not (
            bound_method_function(self) == bound_method_function(other)
            and bound_method_receiver(self) == bound_method_receiver(other)
        )

    def __hash__(self):
        return hash(bound_method_function(self)) ^ hash(bound_method_receiver(self))


class module(object):
    """A module namespace."""

    __module__ = "builtins"

    def __init__(self, name, doc=None):
        self.__name__ = name
        self.__doc__ = doc
        self.__package__ = None
        self.__loader__ = None
        self.__spec__ = None

    def __getattribute__(self, name):
        try:
            return generic_get_attribute(self, name)
        except AttributeError:
            hook = namespace_binding(self, "__getattr__")
            if hook is None:
                raise
            return hook[0](name)

    @property
    def __dict__(self):
        if namespace_items(self) is None:
            return _instance_dict(self)
        return _globals_dict(self)

    def __dir__(self):
        namespace = self.__dict__
        if "__dir__" in namespace:
            return namespace["__dir__"]()
        return list(namespace)


class traceback(object):
    """One entry of an exception's traceback: a frame the exception passed
    through, the line it was running, and the entry nearer the raise."""

    __module__ = "builtins"

    @property
    def tb_frame(self):
        return traceback_field(self, 0)

    @property
    def tb_lineno(self):
        return traceback_field(self, 1)

    @property
    def tb_next(self):
        return traceback_field(self, 2)

    @property
    def tb_lasti(self):
        # Compiled code has no bytecode offsets.
        return -1


class frame(object):
    """A function call, live or one an exception passed through. Compiled
    frames do not keep their local variables; ``f_locals`` is the frame's
    globals under the framelocalsproxy type, as it is for a module frame."""

    __module__ = "builtins"

    @property
    def f_code(self):
        info = traceback_field(self, 0)
        return _code_for(self, info)

    @property
    def f_lineno(self):
        return traceback_field(self, 1)

    @property
    def f_globals(self):
        return _globals_dict(traceback_field(self, 2))

    @property
    def f_back(self):
        return traceback_field(self, 3)

    @property
    def f_locals(self):
        result = {}
        for name, value in namespace_items(traceback_field(self, 2)):
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
register_type("NoneType", NoneType)
register_type("int", int)
register_type("bool", bool)
register_type("float", float)
register_type("function", function)
register_type("builtin_function_or_method", builtin_function_or_method)
register_type("wrapper_descriptor", wrapper_descriptor)
register_type("method", method)
register_type("module", module)
register_type("traceback", traceback)
register_type("frame", frame)


def isinstance(obj, classinfo, /):
    if type_of(classinfo) is type:
        if type_of(obj) is classinfo:
            return True
        return _raw_is_instance(obj, classinfo)
    if is_subclass(type_of(classinfo), tuple):
        n = len(classinfo)
        if n == 0:
            return False
        if n == 1:
            return isinstance(obj, classinfo[0])
        if n == 2:
            if isinstance(obj, classinfo[0]):
                return True
            return isinstance(obj, classinfo[1])
        if n == 3:
            if isinstance(obj, classinfo[0]):
                return True
            if isinstance(obj, classinfo[1]):
                return True
            return isinstance(obj, classinfo[2])
        for candidate in classinfo:
            if isinstance(obj, candidate):
                return True
        return False
    check = getattr(type_of(classinfo), "__instancecheck__", None)
    if check is None:
        bases = getattr(classinfo, "__bases__", None)
        if bases is not None and is_subclass(type_of(bases), tuple):
            cls_attr = getattr(obj, "__class__", None)
            if cls_attr is not None:
                if cls_attr is classinfo or issubclass(cls_attr, classinfo):
                    return True
            return False
        raise TypeError(
            "isinstance() arg 2 must be a type, a tuple of types, or a union"
        )
    return True if check(classinfo, obj) else False


def issubclass(cls, classinfo, /):
    if type_of(classinfo) is type:
        if is_class(cls):
            return is_subclass(cls, classinfo)
        bases = getattr(cls, "__bases__", None)
        if bases is not None and is_subclass(type_of(bases), tuple):
            for base in bases:
                if base is classinfo or issubclass(base, classinfo):
                    return True
            return False
        raise TypeError("issubclass() arg 1 must be a class")
    if is_subclass(type_of(classinfo), tuple):
        n = len(classinfo)
        if n == 0:
            return False
        if n == 1:
            return issubclass(cls, classinfo[0])
        if n == 2:
            if issubclass(cls, classinfo[0]):
                return True
            return issubclass(cls, classinfo[1])
        if n == 3:
            if issubclass(cls, classinfo[0]):
                return True
            if issubclass(cls, classinfo[1]):
                return True
            return issubclass(cls, classinfo[2])
        for candidate in classinfo:
            if issubclass(cls, candidate):
                return True
        return False
    check = getattr(type_of(classinfo), "__subclasscheck__", None)
    if check is None:
        info_bases = getattr(classinfo, "__bases__", None)
        if info_bases is not None and is_subclass(type_of(info_bases), tuple):
            if cls is classinfo:
                return True
            cls_bases = getattr(cls, "__bases__", None)
            if cls_bases is None or not is_subclass(type_of(cls_bases), tuple):
                raise TypeError("issubclass() arg 1 must be a class")
            for base in cls_bases:
                if base is classinfo or issubclass(base, classinfo):
                    return True
            return False
        raise TypeError(
            "issubclass() arg 2 must be a class, a tuple of classes, or a union"
        )
    return True if check(classinfo, cls) else False
