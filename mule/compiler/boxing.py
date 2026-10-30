"""LLVM-visible payload construction and destruction for private boxes.

SBCL's word/descriptor coercions separate fixnum tagging, bignum allocation,
and digit access (``x86-64/move.lisp``). These codecs expose the corresponding
operations from ``mule/procedures/boxing/`` while preserving Mule's existing
object and GMP layouts. They describe storage, not Python type dispatch or
box/unbox cancellation.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING

from mule import procedures

if TYPE_CHECKING:
    from .vop import BoxLayout


class _Payload:
    def helpers(self, layout: BoxLayout) -> str:
        name = layout.name
        return "\n\n".join(
            procedures.load_procedure(f"boxing/box_{name}{suffix}.ll").strip()
            for suffix in ("", "_load", "_private_retain", "_private_release")
        )


@dataclass(frozen=True, slots=True)
class ScalarPayload(_Payload):
    """One trivially droppable scalar in an otherwise ordinary object."""


@dataclass(frozen=True, slots=True)
class IntegerPayload(_Payload):
    """Tagged words or an existing GMP integer with one native-width limb."""
