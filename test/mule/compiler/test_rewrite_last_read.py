"""Last reads transfer local ownership without invalidating live CFG paths."""

import re
import textwrap

import pytest

from test.mule.compiler.rewrite_support import build_case
from test.mule.compiler.test_borrowed_lifetimes import component_for


def item_reads(body):
    component = component_for(
        "def consume(pending, output, flag, divisor):\n"
        + textwrap.indent(textwrap.dedent(body), "    ")
    )
    reads = [
        vop
        for block in component.blocks
        for vop in block.vops
        if vop.info.name == "load-local" and vop.codegen_info.name == "item"
    ]
    assert reads, "The normal source pipeline must retain the item reads under test"
    return reads


@pytest.mark.parametrize(
    "body",
    [
        "item = pending.pop()\nreturn item\n",
        ("item = pending.pop()\nif flag:\n    item = pending.pop()\nreturn item\n"),
        (
            "item = pending.pop()\noutput.append(item)\n"
            "item = pending.pop()\nreturn item\n"
        ),
    ],
    ids=["single-block", "branch-join", "overwrite-kills-old-generation"],
)
def test_last_reads_move_before_exit_or_overwrite(body):
    reads = item_reads(body)
    assert all(read.codegen_info.move for read in reads)
    assert all(read.results[0].tn.sc.owned for read in reads)


@pytest.mark.parametrize(
    "body",
    [
        ("item = pending.pop()\nif flag:\n    output.append(item)\nreturn item\n"),
        (
            "item = pending.pop()\n"
            "while flag:\n    output.append(item)\n    flag -= 1\n"
            "return item\n"
        ),
        (
            "item = pending.pop()\n"
            "try:\n    output.append(item)\n    flag = 10 // divisor\n"
            "except ZeroDivisionError:\n    return item\n"
            "return item\n"
        ),
    ],
    ids=["join-live", "backedge-live", "handler-live"],
)
def test_reads_keep_local_ownership_when_another_cfg_path_needs_it(body):
    reads = item_reads(body)
    # A read before a join, backedge, or raising operation cannot empty the slot.
    assert any(not read.codegen_info.move for read in reads)
    assert any(read.codegen_info.move for read in reads)


WORKLIST = """
def take_last(pending):
    item = pending.pop()
    return item

def branch_join(pending, output, flag):
    item = pending.pop()
    if flag:
        output.append(item)
    output.append(item)
    return item

def repeat_live(pending, output, count):
    item = pending.pop()
    while count:
        output.append(item)
        count -= 1
    return item

def overwrite(pending, output):
    item = pending.pop()
    output.append(item)
    item = pending.pop()
    return item

def handle_live(pending, output, divisor):
    item = pending.pop()
    try:
        output.append(item)
        print(10 // divisor)
    except ZeroDivisionError:
        item.append("recovered")
        print("caught", output[0] is item)
        return item
    return item

def run(raw, mode, flag):
    pending = []
    for word in raw.split(":"):
        pending.append([word])
    output = []
    if mode == "single":
        while pending:
            output.append(take_last(pending))
        item = output[-1]
    elif mode == "join":
        item = branch_join(pending, output, flag)
    elif mode == "loop":
        item = repeat_live(pending, output, flag)
    elif mode == "overwrite":
        item = overwrite(pending, output)
    else:
        item = handle_live(pending, output, flag)
    item.append("live")
    # Allocate fresh payloads after ownership transfers, before reading aliases.
    churn = []
    for index in range(16):
        churn.append([raw, index])
    print(item, output, pending)
    for saved in output:
        print(saved is item, saved)
    try:
        while True:
            take_last(pending)
    except IndexError:
        print("empty", item[-1], len(churn))

run(input(), input(), int(input()))
"""


@pytest.fixture(scope="module")
def worklist_case(tmp_path_factory):
    return build_case(tmp_path_factory.mktemp("last-read"), WORKLIST)


@pytest.mark.integration
@pytest.mark.parametrize(
    ("mode", "flag"),
    [
        ("single", 0),
        ("join", 0),
        ("join", 1),
        ("loop", 0),
        ("loop", 3),
        ("overwrite", 0),
        ("handler", 0),
        ("handler", 2),
    ],
)
def test_worklist_moves_preserve_identity_mutation_and_error_liveness(
    worklist_case, mode, flag
):
    worklist_case.compare(stdin=f"alpha:βeta:tail\n{mode}\n{flag}\n")


def assert_popped_value_has_no_retain(body):
    popped = re.findall(r"(%[\w.]+) = (?:tail )?call ptr @mule_storage_pop\(", body)
    assert popped, "Inspect the actual exact-list worklist path, not an unused helper"
    for value in popped:
        derived = {value}
        # Follow pointer arithmetic as well as calls: an inlined retain increments
        # the object's reference-count field rather than calling a retain helper.
        changed = True
        while changed:
            changed = False
            for line in body.splitlines():
                if not any(
                    re.search(re.escape(item) + r"(?![\w.])", line) for item in derived
                ):
                    continue
                assert not re.search(r"call\b.*@[^\s(]*retain", line), line
                assert not re.search(r"atomicrmw\s+(?:\w+\s+)?add\b", line), line
                result = re.match(
                    r"\s*(%[\w.]+) = (?:getelementptr|bitcast|phi|select|freeze)\b",
                    line,
                )
                if result and result[1] not in derived:
                    derived.add(result[1])
                    changed = True


@pytest.mark.integration
def test_optimized_worklist_transfers_popped_item_without_retain(worklist_case):
    # LLVM can inline take_last into run. Inspect the real callers as well as
    # any surviving take_last definitions, with runtime-dependent list contents.
    body = worklist_case.function_ir(r"mule_fn_(?:take_last|run)(?:\.|$)")
    paths = [
        definition
        for definition in re.split(r"(?m)(?=^define\b)", body)
        if "@mule_storage_pop(" in definition
    ]
    assert paths, "The real worklist must reach the exact-list pop operation"
    for path in paths:
        assert_popped_value_has_no_retain(path)
    helper = worklist_case.function_assembly(r"^mule_storage_pop$")
    assert re.search(r"\bmov\w*\b", helper)
    assert re.search(r"\b(?:sub|dec)\w*\b", helper)
    assert not re.search(r"\b(?:call|jmp)\w*\s+[^\n]*<[^>]*retain", helper)
    assert not re.search(r"\block\s+(?:add|inc)\b", helper)
