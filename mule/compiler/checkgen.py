"""Type-check generation, following ``src/compiler/checkgen.lisp``.

After propagation has settled, each ``Cast`` is marked as needing a runtime
check or not: a check is emitted only under a policy that checks contracts and
only when the derived type of the value does not already satisfy the contract.
Statically contradictory casts were already rejected during propagation.
"""

from __future__ import annotations

from .node import Cast, Lambda


def generate_type_checks(lambda_: Lambda, types) -> bool:
    """Decide ``type_check`` for every cast in ``lambda_``. Returns whether
    any cast changed state, which would require another optimization pass."""
    changed = False
    checks = lambda_.policy.checks_contracts
    for block in lambda_.component.blocks():
        for node in block.nodes():
            if not isinstance(node, Cast):
                continue
            actual = types.get(node.value.use())
            needed = checks and (actual is None or not actual.fits(node.contract.fact))
            if node.type_check != needed:
                node.type_check = needed
                changed = True
    return changed
