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
    derive_type: Callable[[Combination], ValueType | None] | None = None
    optimizer: object = None
    transforms: tuple = field(default_factory=tuple)

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
    name: str, *attributes: str, result: bool = True, protocol=None, derive_type=None
) -> FunInfo:
    info = FunInfo(
        name,
        frozenset(attributes),
        result=result,
        protocol=protocol,
        derive_type=derive_type,
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
