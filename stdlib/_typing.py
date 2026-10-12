"""Type parameter primitives, compiled by Mule.

This provides the interface of CPython's ``_typing`` extension module:
``TypeVar``, ``ParamSpec``, ``TypeVarTuple``, ``TypeAliasType`` and
``Generic`` follow ``Objects/typevarobject.c``. As there, substitution,
generic subscription and generic subclassing call back into ``typing``'s own
helper functions, imported when first needed because ``typing`` imports this
module.
"""

Union = type(int | str)


class _NoDefaultType:
    """The marker of a type parameter without a default."""

    def __repr__(self):
        return "typing.NoDefault"

    def __reduce__(self):
        return "NoDefault"


NoDefault = _NoDefaultType()


def _idfunc(_, x):
    return x


def _typing(name):
    import typing

    return getattr(typing, name)


def _check_bound(bound, message):
    if bound is None:
        return None
    return _typing("_type_check")(bound, message)


class _Unset:
    """Marks a lazily evaluated attribute that has not been computed."""


_UNSET = _Unset()


class _TypeParameter:
    """Behavior shared by the type parameter kinds."""

    __module__ = "typing"

    @property
    def __default__(self):
        evaluate = getattr(self, "_lazy_default", None)
        if evaluate is not None:
            return evaluate()
        return self._default

    def evaluate_default(self):
        return self.__default__

    def __or__(self, other):
        return Union[self, other]

    def __ror__(self, other):
        return Union[other, self]

    def has_default(self):
        return self.__default__ is not NoDefault

    def __reduce__(self):
        return self.__name__

    def __mro_entries__(self, bases):
        raise TypeError(f"Cannot subclass an instance of {type(self).__name__}")

    def __copy__(self):
        return self

    def __deepcopy__(self, memo):
        return self


class TypeVar(_TypeParameter):
    """A type variable: ``T = TypeVar("T")`` or ``class C[T]``."""

    def __init__(
        self,
        name,
        *constraints,
        bound=None,
        covariant=False,
        contravariant=False,
        infer_variance=False,
        default=NoDefault,
    ):
        if covariant and contravariant:
            raise ValueError("Bivariant type variables are not supported.")
        if infer_variance and (covariant or contravariant):
            raise ValueError("Variance cannot be specified with infer_variance.")
        if constraints and bound is not None:
            raise TypeError("Constraints cannot be combined with bound=...")
        if len(constraints) == 1:
            raise TypeError("A single constraint is not allowed")
        self.__name__ = name
        self.__covariant__ = bool(covariant)
        self.__contravariant__ = bool(contravariant)
        self.__infer_variance__ = bool(infer_variance)
        self._lazy_bound = None
        self._lazy_constraints = None
        self._lazy_default = None
        self._bound = _UNSET if bound is None else _check_bound(bound, "Bound must be a type.")
        if constraints:
            checked = []
            for constraint in constraints:
                checked.append(
                    _typing("_type_check")(
                        constraint, "TypeVar(name, constraint, ...): constraints must be types."
                    )
                )
            self._constraints = tuple(checked)
        else:
            self._constraints = _UNSET
        self._default = default

    def __typing_subst__(self, arg):
        return _typing("_typevar_subst")(self, arg)

    def __typing_prepare_subst__(self, alias, args):
        parameters = alias.__parameters__
        index = parameters.index(self)
        if index < len(args):
            return args
        if index == len(args) and self.has_default():
            return (*args, self.__default__)
        raise TypeError(f"Too few arguments for {alias}")

    def __repr__(self):
        if self.__infer_variance__:
            prefix = ""
        elif self.__covariant__:
            prefix = "+"
        elif self.__contravariant__:
            prefix = "-"
        else:
            prefix = "~"
        return prefix + self.__name__


class TypeVarTuple(_TypeParameter):
    """A variadic type variable: ``Ts = TypeVarTuple("Ts")``."""

    def __init__(self, name, *, default=NoDefault):
        self.__name__ = name
        self._lazy_default = None
        self._default = default

    def __iter__(self):
        yield _typing("Unpack")[self]

    def __typing_subst__(self, arg):
        raise TypeError("Substitution of bare TypeVarTuple is not supported")

    def __typing_prepare_subst__(self, alias, args):
        return _typing("_typevartuple_prepare_subst")(self, alias, args)

    def __repr__(self):
        return self.__name__


class ParamSpecArgs:
    """``P.args`` of a ParamSpec ``P``."""

    __module__ = "typing"

    def __init__(self, origin):
        self.__origin__ = origin

    def __repr__(self):
        return f"{self.__origin__.__name__}.args"

    def __eq__(self, other):
        return isinstance(other, ParamSpecArgs) and self.__origin__ == other.__origin__

    def __hash__(self):
        return hash((ParamSpecArgs, self.__origin__))

    def __mro_entries__(self, bases):
        raise TypeError("Cannot subclass an instance of ParamSpecArgs")


class ParamSpecKwargs:
    """``P.kwargs`` of a ParamSpec ``P``."""

    __module__ = "typing"

    def __init__(self, origin):
        self.__origin__ = origin

    def __repr__(self):
        return f"{self.__origin__.__name__}.kwargs"

    def __eq__(self, other):
        return isinstance(other, ParamSpecKwargs) and self.__origin__ == other.__origin__

    def __hash__(self):
        return hash((ParamSpecKwargs, self.__origin__))

    def __mro_entries__(self, bases):
        raise TypeError("Cannot subclass an instance of ParamSpecKwargs")


class ParamSpec(_TypeParameter):
    """A parameter specification variable: ``P = ParamSpec("P")``."""

    def __init__(
        self,
        name,
        *,
        bound=None,
        covariant=False,
        contravariant=False,
        infer_variance=False,
        default=NoDefault,
    ):
        self.__name__ = name
        self.__bound__ = _check_bound(bound, "Bound must be a type.")
        self.__covariant__ = bool(covariant)
        self.__contravariant__ = bool(contravariant)
        self.__infer_variance__ = bool(infer_variance)
        self._lazy_default = None
        self._default = default

    @property
    def args(self):
        return ParamSpecArgs(self)

    @property
    def kwargs(self):
        return ParamSpecKwargs(self)

    def __typing_subst__(self, arg):
        return _typing("_paramspec_subst")(self, arg)

    def __typing_prepare_subst__(self, alias, args):
        return _typing("_paramspec_prepare_subst")(self, alias, args)

    def __repr__(self):
        if self.__infer_variance__:
            prefix = ""
        elif self.__covariant__:
            prefix = "+"
        elif self.__contravariant__:
            prefix = "-"
        else:
            prefix = "~"
        return prefix + self.__name__


def _type_parameter(kind, name, bound, constraints, default):
    """A PEP 695 type parameter. ``bound``, ``constraints`` and ``default``
    are functions evaluating them, or None; they run on first access."""
    if kind == "TypeVar":
        parameter = TypeVar(name, infer_variance=True)
        parameter._lazy_bound = bound
        parameter._lazy_constraints = constraints
    elif kind == "ParamSpec":
        parameter = ParamSpec(name, infer_variance=True)
    else:
        parameter = TypeVarTuple(name)
    parameter._lazy_default = default
    return parameter


def _lazy_attribute(parameter, name, missing):
    evaluate = getattr(parameter, name, None)
    if evaluate is None:
        return missing
    return evaluate()


TypeVar.__bound__ = property(
    lambda self: _lazy_attribute(self, "_lazy_bound", None)
    if self._bound is _UNSET
    else self._bound
)
TypeVar.__constraints__ = property(
    lambda self: tuple(_lazy_attribute(self, "_lazy_constraints", ()))
    if self._constraints is _UNSET
    else self._constraints
)


class TypeAliasType:
    """A ``type Name[...] = value`` alias, whose value is evaluated lazily."""

    __module__ = "typing"

    def __init__(self, name, value, *, type_params=()):
        self.__name__ = name
        self.__qualname__ = name
        self._value = value
        self._evaluate = None
        self.__type_params__ = tuple(type_params)
        self.__module__ = None

    @classmethod
    def _lazy(cls, name, evaluate, type_params, module):
        alias = cls(name, None, type_params=type_params)
        alias._evaluate = evaluate
        alias.__module__ = module
        return alias

    @property
    def __value__(self):
        if self._evaluate is not None:
            self._value = self._evaluate()
            self._evaluate = None
        return self._value

    @property
    def __parameters__(self):
        return self.__type_params__

    def __getitem__(self, parameters):
        if not self.__type_params__:
            raise TypeError("Only generic type aliases are subscriptable")
        return type(list[int])(self, parameters)

    def __or__(self, other):
        return Union[self, other]

    def __ror__(self, other):
        return Union[other, self]

    def __reduce__(self):
        return self.__name__

    def __mro_entries__(self, bases):
        raise TypeError("Cannot subclass an instance of TypeAliasType")

    def __repr__(self):
        return self.__name__


class Generic:
    """The base of user-defined generic classes."""

    __module__ = "typing"
    __slots__ = ()
    _is_protocol = False

    def __class_getitem__(cls, parameters):
        return _typing("_generic_class_getitem")(cls, parameters)

    def __init_subclass__(cls, *args, **kwargs):
        return _typing("_generic_init_subclass")(cls, *args, **kwargs)
