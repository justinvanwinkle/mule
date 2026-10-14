"""Pattern conversion requests only the helpers its generated tests use."""

import pytest

from mule.compiler.driver import cached_builtins_interface, compile_unit
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
