"""Pattern conversion requests only the helpers its generated tests use."""

import pytest

from mule.compiler.driver import cached_builtins_interface, compile_unit, emit_llvm
from mule.compiler.modules import STDLIB
from test.mule.compiler.test_exception_groups import compare


@pytest.mark.parametrize(
    "pattern",
    ["0", "None", "True", "0 | 1", "0 as captured", "captured", "_", "Constants.value"],
)
def test_direct_patterns_do_not_import_runtime_helpers(tmp_path, pattern):
    _, modules = compile_unit(
        "class Constants:\n value = 1\n"
        "def choose(subject):\n match subject:\n"
        f"  case {pattern}: return 1\n",
        "direct_pattern.py",
        cached_builtins_interface(),
        roots=(tmp_path, STDLIB),
    )
    assert not modules


@pytest.mark.parametrize(
    "pattern", ["[item]", "{'key': item}", "Constants(item)", "[0] | [1]"]
)
def test_structural_patterns_import_runtime_helpers(tmp_path, pattern):
    _, modules = compile_unit(
        "class Constants:\n __match_args__ = ('value',)\n"
        "def choose(subject):\n match subject:\n"
        f"  case {pattern}: return 1\n",
        "structural_pattern.py",
        cached_builtins_interface(),
        roots=(tmp_path, STDLIB),
    )
    assert [module.name for module in modules] == ["_match"]


def test_direct_patterns_preserve_subject_guard_and_capture_order(tmp_path):
    compare(
        tmp_path,
        """
def subject(value):
    print('subject', value)
    return value
def guard(value):
    print('guard', value)
    return value != 0
for value in (0, 1, 2, None):
    match subject(value):
        case 0 | 1 as captured if guard(captured): print('matched', captured)
        case None: print('none')
        case other: print('fallback', other)
""",
    )


def test_declared_class_and_tuple_subject_patterns_lower_directly_without_match_helper(
    tmp_path,
):
    source = """
class Pair:
    __match_args__ = ('left', 'right')
    def __init__(self, left: int, right: int) -> None:
        self.left = left
        self.right = right

def eval_pair(node, tag: int) -> int:
    match node:
        case Pair(left, _):
            match (tag, left):
                case (0, value):
                    return value + node.right
                case (1 | 2, value):
                    return value - node.right
                case _:
                    return left
        case _:
            return -1
"""
    _, modules = compile_unit(
        source,
        "direct_class_match.py",
        cached_builtins_interface(),
        roots=(tmp_path, STDLIB),
    )
    assert not modules
    ir = emit_llvm(source)
    assert "class_attributes" not in ir
    assert "@mule.fast.isinstance.1" in ir
    assert "@mule_tuple(" not in ir
    compare(
        tmp_path,
        source + "\nfor item in (Pair(10, 3), Pair(10, 4), None):\n"
        "    for tag in (0, 1, 2, 5):\n"
        "        print(eval_pair(item, tag))\n",
    )
