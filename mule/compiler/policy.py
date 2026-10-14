"""Optimization policy, following SBCL's ``policy.lisp`` qualities.

Mule exposes three qualities through the ``mule.optimize`` directive. Derived
decisions (whether a cast becomes a check, whether a body may inline) are
methods here so that every phase consults one authority.
"""

from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class OptimizationPolicy:
    speed: int = 3
    # Safety 3 is exact Python. Safety 2, the default, also assumes builtins
    # are never rebound once installed; safety 0 drops unbound-name checks.
    safety: int = 2
    debug: int = 1
    # Compiler-private builtin implementations retain native debug metadata,
    # while contributing no Python source frames to exception tracebacks.
    builtin_implementation: bool = False

    @property
    def records_traceback(self) -> bool:
        return not self.builtin_implementation

    @property
    def checks_contracts(self) -> bool:
        """Positive safety validates unknown values against declared contracts."""
        return self.safety > 0

    @property
    def prevents_inlining(self) -> bool:
        return self.debug == 3

    @property
    def disables_optimization(self) -> bool:
        return self.speed == 0
