"""Known-function metadata and registration, following ``fun-info.lisp`` and
``knownfun.lisp``.

A ``FunInfo`` records what the compiler knows about one operation: its effect
attributes, how to derive its result type, and later its optimizers,
transforms, and backend templates. ``defknown`` registers one; ``fndb``
populates the database for Python's operations and Mule's runtime protocols.
"""

from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass, field
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from .ctype import ValueType
    from .node import Combination


@dataclass(frozen=True, slots=True)
class Transform:
    fun: Callable[[Combination], bool]
    arg_types: tuple[ValueType, ...] | None = None
    policy: Callable[[Combination], bool] | None = None


@dataclass(frozen=True, slots=True)
class FunInfo:
    name: str
    # Effect attributes, as in SBCL's DEFKNOWN attribute list:
    #   foldable: may be evaluated at compile time on constant arguments.
    #   flushable: may be deleted when its result is unused.
    #   call: may run user code (dunder methods, callbacks).
    #   unsafe: mutates visible state (namespaces, objects, exception state).
    #   never_returns: control does not continue past the call.
    attributes: frozenset[str] = frozenset()
    # Whether the operation produces a Python object the caller can consume.
    result: bool = True
    # The compiled stdlib protocol name this operation invokes, if any.
    protocol: str | None = None
    derive_type: Callable[[Combination], ValueType | None] | None = field(
        default=None, compare=False
    )
    optimizer: Callable[[Combination], bool] | None = field(default=None, compare=False)
    transforms: tuple[Transform, ...] = field(default_factory=tuple, compare=False)
    propagate: Callable[..., object] | None = field(default=None, compare=False)
    constrain_branch: Callable[..., object] | None = field(default=None, compare=False)
    numeric: bool = False
    non_integer_first_arg: bool = False
    index_second_arg: bool = False

    @property
    def foldable(self) -> bool:
        return "foldable" in self.attributes

    @property
    def flushable(self) -> bool:
        return "flushable" in self.attributes

    @property
    def may_call(self) -> bool:
        return "call" in self.attributes

    @property
    def unsafe(self) -> bool:
        return "unsafe" in self.attributes

    @property
    def never_returns(self) -> bool:
        return "never_returns" in self.attributes


KNOWN: dict[str, FunInfo] = {}


def defknown(
    name: str,
    *attributes: str,
    result: bool = True,
    protocol=None,
    derive_type=None,
    propagate=None,
    constrain_branch=None,
    numeric: bool = False,
    non_integer_first_arg: bool = False,
    index_second_arg: bool = False,
) -> FunInfo:
    info = FunInfo(
        name,
        frozenset(attributes),
        result=result,
        protocol=protocol,
        derive_type=derive_type,
        propagate=propagate,
        constrain_branch=constrain_branch,
        numeric=numeric,
        non_integer_first_arg=non_integer_first_arg,
        index_second_arg=index_second_arg,
    )
    if name in KNOWN:
        raise ValueError(f"known function {name} is already defined")
    KNOWN[name] = info
    return info


def fun_info(name: str) -> FunInfo:
    try:
        return KNOWN[name]
    except KeyError:
        raise KeyError(f"no known function named {name}") from None


def deftransform(
    name: str,
    arg_types: tuple[ValueType, ...] | None = None,
    fun: Callable[[Combination], bool] | None = None,
    *,
    policy: Callable[[Combination], bool] | None = None,
):
    """Register an IR1 transform on the known function ``name``."""
    info = fun_info(name)

    def register(fn: Callable[[Combination], bool]) -> Callable[[Combination], bool]:
        transform = Transform(fn, arg_types, policy)
        object.__setattr__(info, "transforms", (*info.transforms, transform))
        return fn

    return register(fun) if fun is not None else register


def defoptimizer(
    name: str,
    fun: Callable[[Combination], bool] | None = None,
):
    """Register a custom IR1 optimizer on the known function ``name``."""
    info = fun_info(name)

    def register(fn: Callable[[Combination], bool]) -> Callable[[Combination], bool]:
        object.__setattr__(info, "optimizer", fn)
        return fn

    return register(fun) if fun is not None else register


def defderive(
    name: str,
    fun: Callable[[Combination], ValueType | None] | None = None,
):
    """Register a result-type derivation function on the known function ``name``."""
    info = fun_info(name)

    def register(
        fn: Callable[[Combination], ValueType | None],
    ) -> Callable[[Combination], ValueType | None]:
        object.__setattr__(info, "derive_type", fn)
        return fn

    return register(fun) if fun is not None else register


def defpropagate(
    name: str,
    fun: Callable[..., object] | None = None,
):
    """Register a flow-propagation handler on the known function ``name``."""
    info = fun_info(name)

    def register(fn: Callable[..., object]) -> Callable[..., object]:
        object.__setattr__(info, "propagate", fn)
        return fn

    return register(fun) if fun is not None else register


def defconstrain(
    name: str,
    fun: Callable[..., object] | None = None,
):
    """Register a branch-constraint handler on the known function ``name``."""
    info = fun_info(name)

    def register(fn: Callable[..., object]) -> Callable[..., object]:
        object.__setattr__(info, "constrain_branch", fn)
        return fn

    return register(fun) if fun is not None else register


__all__ = [
    "KNOWN",
    "FunInfo",
    "Transform",
    "defconstrain",
    "defderive",
    "defknown",
    "defoptimizer",
    "defpropagate",
    "deftransform",
    "fun_info",
]
