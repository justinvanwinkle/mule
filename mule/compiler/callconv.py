"""Internal argument and return locations, independent of whole-body lowering.

SBCL's ``return-info`` and known local-call conventions select machine value
locations without requiring every operation in the callee to be unboxed
(``ltn-analyze-return`` and ``ir2-convert-local-known-call``). Mule expresses
those locations as LLVM storage classes. Generic object entries retain their
object convention; a numeric entry can still use ordinary object operations.
"""

from __future__ import annotations

from dataclasses import dataclass

from .vop import DESCRIPTOR, DOUBLE, IMMEDIATE, SIGNED, StorageClass


@dataclass(frozen=True, slots=True)
class CallConvention:
    parameters: tuple[StorageClass, ...]
    returned: StorageClass

    def operand_restriction(self, vop, ref):
        """Representations this entry's boundary generator can consume or emit.

        A numeric return can extract a boxed value after object-sensitive uses.
        A raw float parameter must instead reach an entirely compatible local
        group: reconstructing its original object identity would not be safe.
        Fixnum parameters may use their existing tagged representation inside
        an object body, so their initialization retains the usual restriction.
        """
        if vop.info.name == "return" and not ref.write_p:
            if self.returned is DOUBLE:
                return (DESCRIPTOR, DOUBLE)
            if self.returned is SIGNED:
                return (IMMEDIATE, SIGNED)
        elif vop.info.name == "move-argument" and ref.write_p:
            if self.parameters[vop.codegen_info] is DOUBLE:
                return (DOUBLE,)
        return None
