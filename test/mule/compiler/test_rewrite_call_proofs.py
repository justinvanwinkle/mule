"""Call-shape proofs preserve roots, binding, and guarded fallback effects."""

import re

import pytest

from test.mule.compiler.rewrite_support import build_case

pytestmark = pytest.mark.integration


def _direct_helper(case, target, *, borrowed):
    helpers = case.function_ir(rf"^mule\.direct\.mule_fn_{target}\.", optimized=False)
    definitions = re.findall(r"^define .*?^}", helpers, re.MULTILINE | re.DOTALL)
    selected = [
        body
        for body in definitions
        if bool(re.search(rf"@\"mule_fn_{target}\.borrow\.", body)) == borrowed
    ]
    assert selected, f"No {'borrowed' if borrowed else 'owning'} call to {target}"
    return "\n".join(selected)


def _assert_optimized_identity_path(case, caller):
    # Follow the real dispatch helper if LLVM leaves it outlined. Otherwise the
    # wrapper/body contains its identity operation after inlining.
    pending = [rf"^mule_fn_{caller}(?:$|\.body$)"]
    seen = set()
    reached = []
    while pending:
        pattern = pending.pop()
        if pattern in seen:
            continue
        seen.add(pattern)
        body = case.function_ir(pattern)
        reached.append(body)
        for quoted, plain in re.findall(
            r'\bcall\b[^\n]*@(?:"([^"]+)"|([\w.$]+))\(', body
        ):
            symbol = quoted or plain
            if symbol.startswith(("mule.direct.", "mule.positional.", "mule_fn_")):
                pending.append(rf"^{re.escape(symbol)}$")
    path = "\n".join(reached)
    assert "@mule_function_matches(" in path
    assert re.search(r"icmp eq ptr", path), (
        "The matched path must compare identity directly"
    )
    return path


def _assert_matched_path_has_no_argument_acquisition(case, caller, target):
    # This proof starts at the caller's owned parameter, not an independently
    # selected helper. Follow that operand into outlined callees until the
    # actual identity comparison, excluding only the target's guarded fallback.
    functions = {}
    for function in re.findall(
        r"^define .*?^}", case.optimized_llvm, re.MULTILINE | re.DOTALL
    ):
        header = re.search(r'@(?:"([^"]+)"|([\w.$]+))\(([^\n]*)\)', function)
        functions[header[1] or header[2]] = (header[3], function)
    matched_target = False
    compared_argument = False
    active = set()

    def operand(text):
        values = re.findall(r"%[\w.$-]+", text)
        return values[-1] if values else None

    def follow(symbol, rooted_indices):
        nonlocal matched_target, compared_argument
        state = symbol, frozenset(rooted_indices)
        assert state not in active, f"Unexpected recursive root forwarding in {symbol}"
        active.add(state)
        parameters, function = functions[symbol]
        formals = [operand(parameter) for parameter in parameters.split(",")]
        aliases = {formals[index] for index in rooted_indices}
        sections = re.split(r"^([\w.$-]+):[^\n]*\n", function, flags=re.MULTILINE)
        blocks = dict(zip(sections[1::2], sections[2::2]))
        pending = [sections[1]]
        reachable = []
        seen = set()
        while pending:
            label = pending.pop()
            if label in seen:
                continue
            seen.add(label)
            block = blocks[label]
            reachable.append(block)
            matched = re.search(
                rf"(%[\w.$-]+) = .*@mule_function_matches\([^\n]*mule_fn_{target}",
                block,
            )
            if matched is None:
                pending.extend(re.findall(r"label %([\w.$-]+)", block))
                continue
            matched_target = True
            condition = re.search(
                rf"(%[\w.$-]+) = icmp (eq|ne) i32 {re.escape(matched[1])}, 0",
                block,
            )
            assert condition, block
            branch = re.search(
                rf"br i1 {re.escape(condition[1])}, label %([\w.$-]+), label %([\w.$-]+)",
                block,
            )
            assert branch, block
            direct, fallback = (branch[1], branch[2])
            if condition[2] == "eq":
                direct, fallback = fallback, direct
            seen.add(fallback)
            pending.append(direct)

        # LLVM can copy the root through PHIs, pointer casts, or a spill slot.
        # Track these aliases before inspecting calls and reference increments.
        instructions = "\n".join(reachable).splitlines()
        slots = set()
        changed = True
        while changed:
            previous = aliases.copy(), slots.copy()
            for line in instructions:
                stored = re.search(r"store ptr ([^,]+), ptr ([^,]+)", line)
                if stored and operand(stored[1]) in aliases:
                    slots.add(operand(stored[2]))
                assigned = re.search(r"(%[\w.$-]+) = (.*)", line)
                if assigned:
                    value, expression = assigned.groups()
                    uses = set(re.findall(r"%[\w.$-]+", expression))
                    if (
                        expression.startswith(
                            (
                                "phi ptr",
                                "select i1",
                                "bitcast",
                                "addrspacecast",
                                "getelementptr",
                                "freeze ptr",
                            )
                        )
                        and uses & aliases
                    ):
                        aliases.add(value)
                    if expression.startswith("load ptr") and uses & slots:
                        aliases.add(value)
                call = re.search(
                    r'\bcall\b[^\n]*@(?:"([^"]+)"|([\w.$]+))\((.*)\)', line
                )
                if call is None:
                    assert not (
                        re.search(r"\bcall\b", line)
                        and set(re.findall(r"%[\w.$-]+", line)) & aliases
                    ), f"Indirect call receives the borrowed root: {line}"
                    continue
                callee = call[1] or call[2]
                arguments = [operand(argument) for argument in call[3].split(",")]
                rooted = {
                    index
                    for index, argument in enumerate(arguments)
                    if argument in aliases
                }
                if not rooted:
                    continue
                assert callee not in {"mule.fast.retain", "mule_retain"}, (symbol, line)
                if callee in {"mule.fast.release", "mule_release", "mule_destroy"}:
                    continue
                assert callee in functions, (
                    f"Opaque call receives the borrowed root: {line}"
                )
                if follow(callee, rooted) and assigned:
                    aliases.add(assigned[1])
            changed = previous != (aliases, slots)

        for line in instructions:
            increment = re.search(r"atomicrmw (?:volatile )?add ptr ([^,]+),", line)
            assert not (increment and operand(increment[1]) in aliases), (symbol, line)
            if "icmp eq ptr" in line and "@mule.none" in line:
                compared_argument |= bool(set(re.findall(r"%[\w.$-]+", line)) & aliases)
        active.remove(state)
        return any(
            line.strip().startswith("ret ptr") and operand(line) in aliases
            for line in instructions
        )

    follow(f"mule_fn_{caller}.body", {1})
    assert matched_target, "The rooted operand must reach the target identity guard"
    assert compared_argument, (
        "The matched callee must compare the actual rooted operand"
    )


def test_rooted_positional_call_borrows_argument_and_keeps_direct_identity(tmp_path):
    case = build_case(
        tmp_path,
        """
def probe(value):
    return value is None
def caller(value):
    result = probe(value)
    return result, value
value = [input()]
print(caller(value), value)
print(caller(None))
""",
    )
    helper = _direct_helper(case, "probe", borrowed=True)
    direct, fallback = helper.split("fallback:\n", 1)
    # The helper receives the caller's actual rooted operand, not a retained
    # copy. Frame rejection must not release that caller-owned reference.
    assert "@mule_function_matches(" in direct
    assert "@mule.fast.retain" not in direct
    assert "@mule.fast.release(ptr %arg.0)" not in direct
    assert "@mule.fast.retain(ptr %arg.0)" in fallback
    caller = case.function_ir(r"^mule_fn_caller\.body$", optimized=False)
    assert "@mule.fast.retain" not in caller
    _assert_matched_path_has_no_argument_acquisition(case, "caller", "probe")
    case.compare(stdin="owned payload\n")


@pytest.mark.parametrize(
    ("definition", "call"),
    [
        ("def probe(value):\n    return value is None", "probe(value=value)"),
        (
            "def probe(value=None):\n    return value is None",
            "probe(value)",
        ),
    ],
    ids=["keyword", "default"],
)
def test_nonborrowable_call_shapes_keep_owned_entry(tmp_path, definition, call):
    case = build_case(
        tmp_path,
        definition
        + f"""
def caller(value):
    result = {call}
    return result, value
value = [input()]
print(caller(value), value)
print(caller(None))
""",
    )
    helper = _direct_helper(case, "probe", borrowed=False)
    assert "@mule.fast.release(ptr %arg.0)" in helper
    assert not re.search(r'@"mule_fn_probe\.borrow\.', helper)
    _assert_optimized_identity_path(case, "caller")
    case.compare(stdin="still rooted\n")


def test_guarded_borrowing_preserves_rebound_mutation_and_exception_cleanup(tmp_path):
    case = build_case(
        tmp_path,
        """
def choose(owner, token):
    return token if owner is None else False
def caller(owner):
    token = [input()]
    try:
        result = choose(owner, token)
    except ValueError:
        result = 'caught'
    return result, owner
marker = []
print(caller(marker), marker)
original = choose
events = []
def replacement(owner, token):
    events.append(token[0])
    owner.append(token)
    raise ValueError('expected')
choose = replacement
result, same = caller(marker)
print(result, same is marker, marker, events)
choose = original
result, same = caller(None)
print(result, same is None)
""",
    )
    helper = _direct_helper(case, "choose", borrowed=True)
    failed = helper.split("failed:\n", 1)[1].split("fallback:\n", 1)[0]
    assert "release(ptr %arg.1)" in failed
    assert "release(ptr %arg.0)" not in failed
    fallback = helper.split("fallback:\n", 1)[1]
    assert "retain(ptr %arg.0)" in fallback
    assert "retain(ptr %arg.1)" not in fallback
    _assert_optimized_identity_path(case, "caller")
    case.compare(stdin="initial\nreplacement\nrestored\n")


def test_omitted_default_uses_actual_callable_storage_after_global_rebinding(tmp_path):
    case = build_case(
        tmp_path,
        """
seed = [input()]
def probe(value=seed):
    return value is seed
def caller():
    return probe()
print(caller())
seed.append('mutated default')
print(caller())
seed = [input()]
print(caller())
""",
    )
    helper = _direct_helper(case, "probe", borrowed=False)
    direct, _fallback = helper.split("fallback:\n", 1)
    assert "@mule_function_default(ptr %callee" in direct
    assert "@mule.fast.retain(ptr %def." in direct
    path = _assert_optimized_identity_path(case, "caller")
    assert "@mule_function_default(" in path
    case.compare(stdin="captured default\nnew global\n")


def test_callable_loaded_before_argument_rebinding_keeps_original_target(tmp_path):
    case = build_case(
        tmp_path,
        """
events = []
def probe(value):
    return value is None
original = probe
def replacement(value):
    events.append('replacement')
    return False
def argument(value):
    global probe
    events.append('argument')
    probe = replacement
    return value
def caller(value):
    return probe(argument(value))
value = [input()]
print(caller(value), events)
print(caller(value), events)
probe = original
print(caller(None), events)
""",
    )
    # Argument callbacks can invalidate a root proof. The loaded callable still
    # determines this call even though its global slot changes during evaluation.
    _direct_helper(case, "probe", borrowed=False)
    _assert_optimized_identity_path(case, "caller")
    case.compare(stdin="callback input\n")


def test_captured_parameter_keeps_owner_after_factory_returns(tmp_path):
    case = build_case(
        tmp_path,
        """
def capture(value):
    def read():
        return value
    return read
def caller(value):
    read = capture(value)
    return read
value = [input()]
read = caller(value)
print(read() is value)
value.append('changed')
print(read())
del value
print(read())
""",
    )
    helper = _direct_helper(case, "capture", borrowed=False)
    assert "release(ptr %arg.0)" in helper
    optimized = case.function_ir(r"^mule_fn_capture(?:$|\.body$)")
    assert re.search(r"\bcall\b[^\n]*@[^\n]*(?:cell|capture)", optimized)
    case.compare(stdin="captured payload\n")


def test_augmented_parameter_is_not_borrowed_and_preserves_float_alias(tmp_path):
    case = build_case(
        tmp_path,
        """
def increase(value):
    value += 1.25
    return value
def caller(value):
    result = increase(value)
    return result, value
value = float(input())
alias = value
result, same = caller(value)
print(result, value, same is alias, result is alias)
""",
    )
    # Parameters with stores cannot receive borrowed entries. This exercises
    # the source-level boundary protecting local box reuse from borrowed roots.
    helper = _direct_helper(case, "increase", borrowed=False)
    assert "release(ptr %arg.0)" in helper
    optimized = case.function_ir(
        r"^mule_fn_increase(?:\.fn$|\.spec\.\d+$|\.body(?:\.fallback\.[\w.]+)?$)"
    )
    # The unconstrained object entry must retain the real in-place protocol.
    # Float-box exclusivity is checked by that protocol, not assumed here.
    assert "@mule_binary_reuse(" in optimized
    case.compare(stdin="3.5\n")
