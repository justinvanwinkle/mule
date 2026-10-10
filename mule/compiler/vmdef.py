"""Template definition and lookup, following ``src/compiler/vmdef.lisp`` and
``meta-vmdef.lisp``.

``define_vop`` registers a ``Template`` and, when it translates a known
function, records it under that function's name so local template selection
(``ltn``) finds the candidates in cost order. SBCL stores the list on the
``fun-info``; here the registry lives beside the templates so the known
function database stays free of backend data.
"""

from __future__ import annotations

from collections import defaultdict

from .vop import Template

TEMPLATES: dict[str, list[Template]] = defaultdict(list)


def define_vop(
    name: str,
    *,
    translate=None,
    args=(),
    results=(),
    arg_types=(),
    result_types=(),
    cost: int = 0,
    policy=None,
    guard=None,
    ltn_policy: str | None = None,
):
    """Decorate a generator function into a registered ``Template``.

    ``translate`` names the known function(s) the template implements, as
    ``FunInfo`` objects or names. ``args``/``results`` are operand
    restrictions (a tuple of storage classes, or ``None`` for any);
    ``arg_types``/``result_types`` are primitive-type requirements that the
    IR1 derived types must satisfy for the template to apply. ``ltn_policy``
    restricts the template to native (``"raw"``) or object bodies.
    """

    def register(generator):
        template = Template(
            name,
            generator,
            args=tuple(args),
            results=tuple(results),
            arg_types=tuple(arg_types),
            result_types=tuple(result_types),
            cost=cost,
            policy=policy,
            guard=guard,
            translate=translate,
            ltn_policy=ltn_policy,
        )
        for known in _translated(translate):
            candidates = TEMPLATES[known]
            candidates.append(template)
            candidates.sort(key=lambda candidate: candidate.cost)
        return template

    return register


def _translated(translate) -> tuple[str, ...]:
    if translate is None:
        return ()
    if isinstance(translate, (list, tuple)):
        return tuple(_name(known) for known in translate)
    return (_name(translate),)


def _name(known) -> str:
    return known if isinstance(known, str) else known.name


def templates_for(fun_info) -> list[Template]:
    """The templates translating ``fun_info``, cheapest first."""
    return TEMPLATES.get(fun_info.name, [])


def operand_restriction_ok(restriction, primitive_type) -> bool:
    """Whether a value of ``primitive_type`` can satisfy ``restriction``
    (``operand-restriction-ok``): any restriction, or one admitting a storage
    class the primitive type can occupy."""
    if restriction is None:
        return True
    return any(primitive_type.admits(sc) for sc in restriction)


def primitive_type_ok(required, primitive_type) -> bool:
    """Whether ``primitive_type`` satisfies a primitive-type requirement:
    ``None`` accepts anything; otherwise one of the listed types."""
    if required is None:
        return True
    if isinstance(required, (list, tuple, frozenset, set)):
        return primitive_type in required
    return primitive_type is required


__all__ = [
    "TEMPLATES",
    "define_vop",
    "operand_restriction_ok",
    "primitive_type_ok",
    "templates_for",
]
