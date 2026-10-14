"""Compare scoped print ending behavior with the Python reference runtime."""

import subprocess
import sys

import pytest

from test.mule.compiler.test_native import run_native


def assert_print_matches_python(tmp_path, source):
    reference = subprocess.run(
        [sys.executable, "-c", source],
        check=False,
        capture_output=True,
        text=True,
        timeout=10,
    )
    assert reference.returncode == 0, reference.stderr
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == reference.stdout
    assert result.stderr == ""


def test_print_default_ending_and_multiple_objects_match_python(tmp_path):
    assert_print_matches_python(
        tmp_path,
        "print('first')\nprint()\nprint('second', 42, True, None)\n",
    )


@pytest.mark.parametrize("ending", ["''", "'!'", "'🙂\\n終'", "'\\0終'", "None"])
def test_print_custom_ending_matches_python(tmp_path, ending):
    assert_print_matches_python(
        tmp_path,
        f"print('first', 42, end={ending})\nprint('second')\n",
    )


def test_print_without_objects_writes_custom_ending(tmp_path):
    assert_print_matches_python(tmp_path, "print(end='only the ending')\n")


@pytest.mark.parametrize("reference", ["print", "__builtins__.print"])
def test_aliased_print_honors_ending_and_str_hook(tmp_path, reference):
    assert_print_matches_python(
        tmp_path,
        f"""
class Text:
    def __str__(self):
        return 'converted'
writer = {reference}
writer(Text(), end='!')
writer('alive')
""",
    )


@pytest.mark.parametrize("ending", ["42", "b'bytes'"])
def test_print_invalid_ending_prevents_conversion_and_recovers(tmp_path, ending):
    assert_print_matches_python(
        tmp_path,
        f"""
class Text:
    def __str__(self):
        print('unexpected conversion')
        return 'converted'
try:
    print(Text(), end={ending})
except TypeError:
    print('caught')
print('alive')
""",
    )


def test_print_conversion_failure_preserves_partial_output_and_recovers(tmp_path):
    assert_print_matches_python(
        tmp_path,
        """
class BadText:
    def __str__(self):
        raise ValueError('conversion failed')
try:
    print('prefix', BadText(), end='unexpected ending')
except ValueError:
    print('caught')
print('alive')
""",
    )


def test_print_returns_none_with_custom_ending(tmp_path):
    assert_print_matches_python(
        tmp_path,
        "result = print('first', end='')\nprint(result)\n",
    )


@pytest.mark.parametrize("keyword", ["sep=':'", "file=None", "flush=True"])
def test_print_standard_keywords_match_python(tmp_path, keyword):
    assert_print_matches_python(
        tmp_path,
        f"print('first', 'second', {keyword})\n",
    )


def test_print_custom_stream_and_rebound_stdout_match_python(tmp_path):
    assert_print_matches_python(
        tmp_path,
        """
import sys
class Stream:
    def __init__(self):
        self.events = []
    def write(self, text):
        self.events.append(('write', text))
    def flush(self):
        self.events.append(('flush',))
stream = Stream()
print('first', 42, sep='|', end='!', file=stream, flush=True)
print(stream.events)
original = sys.stdout
sys.stdout = stream
print('rebound', sep=None, end=None)
sys.stdout = None
print('ignored', sep=42, end=42)
sys.stdout = original
print(stream.events)
""",
    )
