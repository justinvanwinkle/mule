"""Compiled min/max reductions preserve Python iteration and key behavior."""

import subprocess
import sys

import pytest

from test.mule.compiler.test_native import run_native


def assert_matches_python(tmp_path, source):
    reference = subprocess.run(
        [sys.executable, "-c", source],
        capture_output=True,
        text=True,
        check=True,
        timeout=10,
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == reference.stdout
    assert result.stderr == ""


def test_minmax_positional_values_match_python(tmp_path):
    assert_matches_python(tmp_path, "print(max(4, 2), min(4, 2))\n")


@pytest.mark.parametrize(
    "expression",
    [
        "max([1, -3, 2])",
        "min((1, -3, 2))",
        "max(2 ** 100, -(2 ** 100), 42)",
        "min(2.5, 2, -3.0)",
        "max('café')",
        "min('beta', 'alpha', 'gamma')",
        "max([None])",
        "min(range(5, -4, -2))",
        "max([], default=None)",
        "min([], default=42, key=99)",
        "max([1, -3, 2], key=abs)",
    ],
)
def test_minmax_supported_values_match_python(tmp_path, expression):
    assert_matches_python(tmp_path, f"print({expression})\n")


def test_minmax_empty_default_preserves_identity_and_skips_key(tmp_path):
    assert_matches_python(
        tmp_path,
        """
class Default:
    pass
default = Default()
def key(value):
    raise ValueError('unexpected key invocation')
print(max([], default=default, key=key) is default)
print(min([], default=default, key=key) is default)
""",
    )


def test_minmax_key_order_and_first_tie_identity_match_python(tmp_path):
    assert_matches_python(
        tmp_path,
        """
class Item:
    def __init__(self, label, value):
        self.label = label
        self.value = value
def key(item):
    print('key', item.label)
    return item.value
first = Item('first', 2)
second = Item('second', 2)
lower = Item('lower', 1)
print(max(first, second, lower, key=key) is first)
print(min(first, second, lower, key=key) is lower)
""",
    )


def test_minmax_streams_custom_iterators_and_indexed_sequences(tmp_path):
    assert_matches_python(
        tmp_path,
        """
class Countdown:
    def __init__(self, value):
        self.value = value
    def __iter__(self):
        print('iter')
        return self
    def __next__(self):
        if self.value == 0:
            raise StopIteration
        self.value -= 1
        print('yield', self.value)
        return self.value
class Indexed:
    def __getitem__(self, index):
        if index == 3:
            raise IndexError
        return index * 2
def key(value):
    print('key', value)
    return -value
print(max(Countdown(3), key=key))
print(min(Indexed()))
""",
    )


@pytest.mark.parametrize(
    ("expression", "exception"),
    [
        ("max()", "TypeError"),
        ("min()", "TypeError"),
        ("max([])", "ValueError"),
        ("min([])", "ValueError"),
        ("max(1, 2, default=None)", "TypeError"),
        ("min(1, 2, default=42)", "TypeError"),
        ("max(42)", "TypeError"),
        ("min([1], key=42)", "TypeError"),
        ("max([1], unknown=42)", "TypeError"),
    ],
)
def test_minmax_argument_errors_recover_like_python(tmp_path, expression, exception):
    assert_matches_python(
        tmp_path,
        f"try:\n {expression}\nexcept {exception}:\n print('caught')\n"
        "print(max([2, 1]), min([2, 1]))\n",
    )


@pytest.mark.parametrize("stop_at", [1, 2])
def test_minmax_key_stopiteration_is_not_iterator_exhaustion(tmp_path, stop_at):
    assert_matches_python(
        tmp_path,
        f"""
def key(value):
    if value == {stop_at}:
        raise StopIteration('key failed')
    return value
try:
    max([1, 2], key=key)
except StopIteration:
    print('max key failed')
try:
    min([1, 2], key=key)
except StopIteration:
    print('min key failed')
""",
    )


def test_minmax_comparison_stopiteration_propagates(tmp_path):
    assert_matches_python(
        tmp_path,
        """
class Item:
    def __gt__(self, other):
        raise StopIteration('comparison failed')
    def __lt__(self, other):
        raise StopIteration('comparison failed')
first = Item()
second = Item()
try:
    max([first, second])
except StopIteration:
    print('max comparison failed')
try:
    min([first, second])
except StopIteration:
    print('min comparison failed')
""",
    )


def test_minmax_nan_signed_zero_and_ties_preserve_first_identity(tmp_path):
    assert_matches_python(
        tmp_path,
        """
nan = float('nan')
finite = 1.0
positive_zero = 0.0
negative_zero = -0.0
print(max(nan, finite) is nan, min(nan, finite) is nan)
print(max(finite, nan) is finite, min(finite, nan) is finite)
print(max(positive_zero, negative_zero) is positive_zero)
print(min(negative_zero, positive_zero) is negative_zero)
""",
    )


def test_minmax_builtin_aliases_and_rebinding_preserve_saved_function(tmp_path):
    assert_matches_python(
        tmp_path,
        """
largest = __builtins__.max
smallest = min
def replacement(*args):
    return 'replaced'
max = replacement
min = replacement
print(max(1, 2), min(1, 2))
print(largest([1, 2], key=None), smallest([1, 2], default=None))
""",
    )
