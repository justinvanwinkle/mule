"""Pooled constants retain their roots without a counted copy per comparison."""

import re

import pytest

from mule.compiler.codegen import compile_component
from mule.compiler.ctype import UNKNOWN
from mule.compiler.ltn import EntryContext
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case

TAG = "route-with-pooled-heap-text-not-a-character"


def _comparison_constants(expression):
    compilation = compile_source(
        f"def matches(value):\n    return value == {expression}\n"
    )
    function = compilation.lambdas[1]
    facts = compilation.facts[function]
    context = EntryContext(
        None,
        facts.types,
        facts.targets,
        facts.parameters or (UNKNOWN,) * len(function.vars),
        facts.return_type,
    )
    component = compile_component(function, context)
    constants = [
        vop
        for block in component.blocks
        for vop in block.vops
        if vop.info.name == "constant"
    ]
    assert constants, "the runtime comparison must still consume its literal"
    return constants


@pytest.mark.parametrize(
    "expression",
    [
        repr(TAG),
        repr(TAG.encode()),
        repr((TAG, 64)),
        repr(((TAG, 64), b"route")),
    ],
    ids=["heap-text", "heap-bytes", "tuple", "nested-tuple"],
)
def test_pooled_comparison_constants_borrow_the_unit_root(expression):
    constants = _comparison_constants(expression)
    assert len(constants) == 1
    result = constants[0].results[0].tn
    assert result.borrowed_from is result


@pytest.mark.parametrize(
    "expression",
    [repr((TAG, None)), repr((TAG, True))],
    ids=["none-in-tuple", "boolean-in-tuple"],
)
def test_unsupported_tuple_shapes_keep_an_owned_comparison_operand(expression):
    constants = _comparison_constants(expression)
    assert len(constants) == 1
    result = constants[0].results[0].tn
    assert result.borrowed_from is None


def test_returning_a_pooled_tuple_acquires_an_owned_reference():
    compilation = compile_source(f"def saved_route():\n    return {(TAG, 64)!r}\n")
    function = compilation.lambdas[1]
    facts = compilation.facts[function]
    component = compile_component(
        function,
        EntryContext(None, facts.types, facts.targets, (), facts.return_type),
    )
    constants = [
        vop
        for block in component.blocks
        for vop in block.vops
        if vop.info.name == "constant"
    ]
    assert constants
    assert all(vop.results[0].tn.borrowed_from is None for vop in constants)


ROUTE_SOURCE = f"""
def text_route(value):
    return value == {TAG!r}

def tuple_route(value):
    return value == {(TAG, 64)!r}

def optional_route(value):
    return value == {(TAG, None)!r}

def boolean_route(value):
    return value == {(TAG, True)!r}

def saved_route():
    return {(TAG, 64)!r}

class RaisingComparison:
    def __eq__(self, other):
        print("comparison", other)
        raise ValueError("route comparison failed")

key = input()
number = int(input())
saved = saved_route()
for index in range(5):
    print(text_route(key), tuple_route((key, number)))
    print(optional_route((key, None)), boolean_route((key, number)))
    try:
        text_route(RaisingComparison())
    except ValueError as error:
        print(type(error).__name__, str(error))
    try:
        tuple_route((RaisingComparison(), number))
    except ValueError as error:
        print(type(error).__name__, str(error))
    print(saved, saved is saved_route())
print(text_route(key), tuple_route(saved), saved)
"""


@pytest.fixture(scope="module")
def pooled_routes(tmp_path_factory):
    return build_case(tmp_path_factory.mktemp("pooled-routes"), ROUTE_SOURCE)


_SSA = re.compile(r"%[-a-zA-Z$._0-9]+")
_CACHE_LOAD = re.compile(
    r"^\s*(%[-a-zA-Z$._0-9]+) = load ptr, ptr "
    r"@mule\.literal\.(?:text\.)?slot\.\d+\b",
    re.MULTILINE,
)


def _warm_cache_lines(function):
    """Keep blocks reachable when every literal cache already contains a root."""
    blocks = {}
    for line in function.splitlines()[1:]:
        label = re.match(r"^([-a-zA-Z$._0-9]+):", line)
        if label:
            current = blocks.setdefault(label.group(1), [])
        elif line != "}":
            current.append(line)
    cached_values = {match.group(1) for match in _CACHE_LOAD.finditer(function)}
    warm_conditions = {}
    for line in function.splitlines():
        comparison = re.match(
            r"\s*(%[-a-zA-Z$._0-9]+) = icmp (eq|ne) ptr "
            r"(%[-a-zA-Z$._0-9]+), null",
            line,
        )
        if comparison and comparison.group(3) in cached_values:
            condition, predicate, _value = comparison.groups()
            warm_conditions[condition] = predicate == "ne"
    reachable = set()
    pending = [next(iter(blocks))]
    while pending:
        label = pending.pop()
        if label in reachable:
            continue
        reachable.add(label)
        body = "\n".join(blocks[label])
        branch = re.search(
            r"br i1 (%[-a-zA-Z$._0-9]+), label %([-a-zA-Z$._0-9]+), "
            r"label %([-a-zA-Z$._0-9]+)",
            body,
        )
        if branch and branch.group(1) in warm_conditions:
            condition, yes, no = branch.groups()
            pending.append(yes if warm_conditions[condition] else no)
        else:
            pending.extend(re.findall(r"label %([-a-zA-Z$._0-9]+)", body))
    return [
        line for label, lines in blocks.items() if label in reachable for line in lines
    ]


def _assert_cached_operand_has_no_counted_copy(llvm):
    """Follow each warm cache load to its comparison, not cold tuple builders.

    The cache owns its pointer before this load. A phi may merge that pointer
    with first-use construction, whose dependency ownership remains legitimate.
    Exclude cache-miss edges before following aliases. Nested cache loads in a
    cold tuple builder legitimately retain dependencies and the newly built root.
    """
    compared = False
    for function in re.split(r"(?=^define\b)", llvm, flags=re.MULTILINE):
        if not function.startswith("define"):
            continue
        lines = _warm_cache_lines(function)
        for cached in _CACHE_LOAD.finditer("\n".join(lines)):
            aliases = {cached.group(1)}
            changed = True
            while changed:
                changed = False
                for line in lines:
                    assignment = re.match(
                        r"\s*(%[-a-zA-Z$._0-9]+) = "
                        r"(?:phi ptr|select i1|bitcast ptr|addrspacecast ptr|"
                        r"getelementptr|ptrtoint ptr|inttoptr)\b(.*)",
                        line,
                    )
                    if assignment is None:
                        continue
                    result, operands = assignment.groups()
                    if result not in aliases and aliases.intersection(
                        _SSA.findall(operands)
                    ):
                        aliases.add(result)
                        changed = True
            uses = [line for line in lines if aliases.intersection(_SSA.findall(line))]
            consumers = [
                line
                for line in uses
                if re.search(r"\bcall\b.*@mule_(?:text_eq|compare(?:_bool)?)\(", line)
            ]
            if not consumers:
                continue
            compared = True
            for line in uses:
                assert "atomicrmw" not in line and "cmpxchg" not in line, line
                assert not re.search(
                    r"\bcall\b.*@mule(?:[._][a-z]+)*[._](?:retain|release|destroy)\(",
                    line,
                ), line
    assert compared, "no warm cached literal reached the runtime comparison"


@pytest.mark.integration
def test_cached_text_and_tuple_comparisons_omit_counted_literal_copies(pooled_routes):
    # alwaysinline literal helpers must expose their warm load and its consumers.
    # This follows ownership dataflow after optimization instead of checking only
    # that a helper with 'borrowed' in its identifier was selected.
    for function in ("text_route", "tuple_route"):
        body = pooled_routes.function_ir(rf"^mule_fn_{function}\.")
        _assert_cached_operand_has_no_counted_copy(body)


@pytest.mark.integration
def test_pooled_roots_survive_repeated_comparisons_errors_and_owned_escape(
    pooled_routes,
):
    for key, number in ((TAG, 64), (TAG, 1), ("other-route", 64), ("routé🚀", -7)):
        pooled_routes.compare(f"{key}\n{number}\n")
