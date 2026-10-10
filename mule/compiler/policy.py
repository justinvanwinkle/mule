"""Optimization policy, following SBCL's ``policy.lisp`` qualities.

Mule exposes three qualities through the ``mule.optimize`` directive. Derived
decisions (whether a cast becomes a check, whether a body may inline) are
methods here so that every phase consults one authority.
"""

from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class OptimizationPolicy:
    speed: int = 3
    safety: int = 0
    debug: int = 1

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
