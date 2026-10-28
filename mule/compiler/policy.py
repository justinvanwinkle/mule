"""Optimization policy, following SBCL's ``policy.lisp`` qualities.

Mule exposes three qualities through the ``mule_types.optimize`` directive. Derived
decisions (whether a cast becomes a check, whether a body may inline) are
methods here so that every phase consults one authority.
"""

from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class OptimizationPolicy:
    speed: int = 1
    # Safety 3 preserves dynamic builtin lookup. Safety 2 adds recursion-depth
    # and strict return/local checks; safety 1 is the default; safety 0 drops
    # optional runtime contract and unbound checks.
    safety: int = 1
    debug: int = 1
    # Compiler-private builtin implementations retain native debug metadata,
    # while contributing no Python source frames to exception tracebacks.
    builtin_implementation: bool = False

    @property
    def records_traceback(self) -> bool:
        return self.debug >= 1 and not self.builtin_implementation

    @property
    def records_frame(self) -> bool:
        return self.debug >= 2 or self.builtin_implementation

    @property
    def checks_contracts(self) -> bool:
        """Positive safety validates unknown values against declared contracts."""
        return self.safety > 0

    @property
    def prevents_inlining(self) -> bool:
        return self.debug >= 2

    @property
    def disables_optimization(self) -> bool:
        return self.speed == 0

    @property
    def infers_return_contracts(self) -> bool:
        """Use checked definition-derived results unless optimization is disabled."""
        return not self.disables_optimization and not self.prevents_inlining
