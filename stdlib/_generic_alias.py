"""``types.GenericAlias`` and ``typing.Union``, compiled by Mule.

``list[int]`` produces a ``GenericAlias`` and ``int | str`` a ``Union``,
following CPython's ``Objects/genericaliasobject.c`` and
``Objects/unionobject.c``. Builtin classes subscript and combine through
these from ``__class_getitem__`` and ``type.__or__``, so they belong with the
other builtins.
"""

from _attributes import getattr, hasattr
from _descriptors import classmethod, property
from mule_types import final

__speed__ = 3
__mule_builtin_implementation__ = True


@final
def _type_repr(value):
    """How generic aliases and unions show their arguments (``_Py_typing_type_repr``)."""
    if value is ...:
        return "..."
    if value is type(None):
        return "None"
    if isinstance(value, GenericAlias) or isinstance(value, Union):
        return repr(value)
    if isinstance(value, type):
        module = getattr(value, "__module__", None)
        qualname = getattr(value, "__qualname__", value.__name__)
        if module == "builtins" or module is None:
            return qualname
        return f"{module}.{qualname}"
    if isinstance(value, (list, tuple)):
        return "[" + ", ".join(_type_repr(item) for item in value) + "]"
    return repr(value)


@final
def _collect_parameters(args):
    """The type parameters among ``args``, each once, in order of appearance."""
    parameters = []
    for arg in args:
        if isinstance(arg, type):
            continue
        if hasattr(arg, "__typing_subst__"):
            if arg not in parameters:
                parameters.append(arg)
            continue
        if isinstance(arg, (list, tuple)):
            nested = _collect_parameters(arg)
        else:
            nested = getattr(arg, "__parameters__", ())
            if not isinstance(nested, tuple):
                nested = ()
        for parameter in nested:
            if parameter not in parameters:
                parameters.append(parameter)
    return tuple(parameters)


@final
def _substitute(args, parameters, items):
    """Replace ``parameters`` in ``args`` with ``items`` (``_Py_subs_parameters``)."""
    if not isinstance(items, tuple):
        items = (items,)
    for parameter in parameters:
        prepare = getattr(parameter, "__typing_prepare_subst__", None)
        if prepare is not None:
            items = prepare(_AliasForSubstitution(parameters), items)
    if len(items) != len(parameters):
        raise TypeError(
            f"Too {'many' if len(items) > len(parameters) else 'few'} arguments; "
            f"actual {len(items)}, expected {len(parameters)}"
        )
    result = []
    for arg in args:
        if isinstance(arg, type):
            result.append(arg)
            continue
        substitute = getattr(arg, "__typing_subst__", None)
        if substitute is not None:
            value = substitute(items[parameters.index(arg)])
            unpacked = getattr(arg, "__typing_is_unpacked_typevartuple__", False)
            if unpacked and isinstance(value, tuple):
                result.extend(value)
            else:
                result.append(value)
            continue
        nested = getattr(arg, "__parameters__", ())
        if isinstance(nested, tuple) and nested:
            arg = arg[tuple(items[parameters.index(parameter)] for parameter in nested)]
        elif isinstance(arg, (list, tuple)) and _collect_parameters(arg):
            arg = type(arg)(_substitute(arg, parameters, items))
        result.append(arg)
    return tuple(result)


class _AliasForSubstitution:
    """What ``__typing_prepare_subst__`` hooks read: the alias's parameters."""

    def __init__(self, parameters):
        self.__parameters__ = parameters


class GenericAlias:
    """A class parameterized with arguments, such as ``list[int]``."""

    def __init__(self, origin, args):
        if not isinstance(args, tuple):
            args = (args,)
        self.__origin__ = origin
        self.__args__ = args
        self._parameters = None
        self.__unpacked__ = False

    @property
    def __parameters__(self):
        if self._parameters is None:
            self._parameters = _collect_parameters(self.__args__)
        return self._parameters

    def __getitem__(self, items):
        parameters = self.__parameters__
        if not parameters:
            raise TypeError(f"{self!r} is not a generic class")
        return GenericAlias(self.__origin__, _substitute(self.__args__, parameters, items))

    def __call__(self, *args, **keywords):
        result = self.__origin__(*args, **keywords)
        try:
            result.__orig_class__ = self
        except AttributeError, TypeError:
            pass
        return result

    def __mro_entries__(self, bases):
        return (self.__origin__,)

    def __eq__(self, other):
        if not isinstance(other, GenericAlias):
            return False
        return (
            self.__origin__ == other.__origin__
            and self.__args__ == other.__args__
            and self.__unpacked__ == other.__unpacked__
        )

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        return hash(self.__origin__) ^ hash(self.__args__)

    def __or__(self, other):
        return _union_of(self, other)

    def __ror__(self, other):
        return _union_of(other, self)

    def __instancecheck__(self, instance):
        raise TypeError("isinstance() argument 2 cannot be a parameterized generic")

    def __subclasscheck__(self, subclass):
        raise TypeError("issubclass() argument 2 cannot be a parameterized generic")

    def __iter__(self):
        unpacked = GenericAlias(self.__origin__, self.__args__)
        unpacked.__unpacked__ = True
        yield unpacked

    def __reduce__(self):
        return GenericAlias, (self.__origin__, self.__args__)

    def __getattr__(self, name):
        if name.startswith("__") and name.endswith("__"):
            raise AttributeError(name)
        return getattr(self.__origin__, name)

    def __repr__(self):
        prefix = "*" if self.__unpacked__ else ""
        if not self.__args__:
            arguments = "()"
        else:
            arguments = ", ".join(_type_repr(arg) for arg in self.__args__)
        return f"{prefix}{_type_repr(self.__origin__)}[{arguments}]"


@final
def _union_member(value):
    if value is None:
        return type(None)
    return value


@final
def _unionable(value):
    return (
        value is None
        or isinstance(value, type)
        or isinstance(value, GenericAlias)
        or isinstance(value, Union)
        or hasattr(value, "__typing_subst__")
        or type(value).__name__ in ("TypeAliasType", "_GenericAlias", "_SpecialForm")
    )


@final
def _union_of(left, right):
    if not (_unionable(left) and _unionable(right)):
        return NotImplemented
    return Union._from_args((left, right))


class Union:
    """``X | Y``: a value of any of several types (``typing.Union``)."""

    def __init__(self, args):
        self.__args__ = args
        self._parameters = None

    @classmethod
    def _from_args(cls, args):
        members = []
        for arg in args:
            arg = _union_member(arg)
            nested = arg.__args__ if isinstance(arg, Union) else (arg,)
            for member in nested:
                if member not in members:
                    members.append(member)
        if len(members) == 1:
            return members[0]
        return cls(tuple(members))

    def __class_getitem__(cls, parameters):
        if not isinstance(parameters, tuple):
            parameters = (parameters,)
        if not parameters:
            raise TypeError("Cannot take a Union of no types.")
        return cls._from_args(parameters)

    @property
    def __origin__(self):
        return Union

    @property
    def __parameters__(self):
        if self._parameters is None:
            self._parameters = _collect_parameters(self.__args__)
        return self._parameters

    def __getitem__(self, items):
        parameters = self.__parameters__
        if not parameters:
            raise TypeError(f"{self!r} is not a generic class")
        return Union._from_args(_substitute(self.__args__, parameters, items))

    def __or__(self, other):
        return _union_of(self, other)

    def __ror__(self, other):
        return _union_of(other, self)

    def __eq__(self, other):
        if not isinstance(other, Union):
            return False
        return set(self.__args__) == set(other.__args__)

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        return hash(frozenset(self.__args__))

    def __instancecheck__(self, instance):
        for member in self.__args__:
            if isinstance(instance, member):
                return True
        return False

    def __subclasscheck__(self, subclass):
        for member in self.__args__:
            if issubclass(subclass, member):
                return True
        return False

    def __mro_entries__(self, bases):
        raise TypeError(f"Cannot subclass {self!r}")

    def __repr__(self):
        return " | ".join(_type_repr(member) for member in self.__args__)


Union.__module__ = "typing"


def class_getitem(cls, items):
    """``__class_getitem__`` of the builtin generic classes."""
    return GenericAlias(cls, items)


def type_or(left, right):
    return _union_of(left, right)


def type_ror(right, left):
    return _union_of(left, right)
