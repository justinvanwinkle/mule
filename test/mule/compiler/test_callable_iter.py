"""Callable/sentinel iteration uses real compiled callbacks and comparisons."""

import pytest

from test.mule.compiler.test_native import run_native


def test_callable_iter_compares_sentinel_first_preserves_aliases_and_skips_identity(
    tmp_path,
):
    source = """events = []
class Value:
    def __init__(self, label, answer):
        self.label = label
        self.answer = answer
    def __eq__(self, other):
        if self is other:
            raise AssertionError('identity must not invoke equality')
        events.append(self.label + ':' + other.label)
        return self.answer
value = Value('value', True)
sentinel = Value('sentinel', False)
class Producer:
    def __init__(self):
        self.state = 0
    def __call__(self):
        if self.state == 0:
            self.state = 1
            return value
        if self.state == 1:
            self.state = 2
            return sentinel
        raise AssertionError('callback invoked after exhaustion')
producer = Producer()
iterator = iter(producer, sentinel)
assert producer.state == 0
assert iter(iterator) is iterator
assert next(iterator) is value
assert events == ['sentinel:value']
for attempt in range(2):
    try:
        next(iterator)
    except StopIteration:
        pass
    else:
        raise AssertionError('sentinel must exhaust the iterator')
assert events == ['sentinel:value']
print('ok')
"""
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "ok\n"


@pytest.mark.parametrize("exception", ["StopIteration", "Finished"])
def test_callable_iter_callback_stopiteration_is_sticky(tmp_path, exception):
    source = f"""class Finished(StopIteration):
    pass
class Producer:
    def __init__(self):
        self.stopped = False
    def __call__(self):
        if self.stopped:
            raise AssertionError('callback invoked after StopIteration')
        self.stopped = True
        raise {exception}('finished')
producer = Producer()
iterator = iter(producer, None)
assert not producer.stopped
for attempt in range(3):
    try:
        next(iterator)
    except StopIteration:
        pass
    else:
        raise AssertionError('callback StopIteration must exhaust the iterator')
print('ok')
"""
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "ok\n"


@pytest.mark.parametrize(
    "phase,exception",
    [
        ("callback", "ValueError"),
        ("equality", "ValueError"),
        ("equality", "StopIteration"),
    ],
)
def test_callable_iter_nonterminal_errors_propagate_and_iteration_resumes(
    tmp_path, phase, exception
):
    callback_error = (
        f"    if current == 1:\n        raise {exception}('callback failure')\n"
        if phase == "callback"
        else ""
    )
    equality_error = (
        f"        if value == 1:\n            raise {exception}('equality failure')\n"
        if phase == "equality"
        else ""
    )
    source = (
        "current = 0\n"
        "def produce():\n"
        "    global current\n"
        "    current += 1\n" + callback_error + "    return current\n"
        "class Sentinel:\n"
        "    def __eq__(self, value):\n"
        + equality_error
        + "        return value == 3\n"
        "iterator = iter(produce, Sentinel())\n"
        "try:\n"
        "    next(iterator)\n"
        f"except {exception}:\n"
        "    pass\n"
        "else:\n"
        "    raise AssertionError('callback or equality error was suppressed')\n"
        "assert next(iterator) == 2\n"
        "for attempt in range(2):\n"
        "    try:\n"
        "        next(iterator)\n"
        "    except StopIteration:\n"
        "        pass\n"
        "    else:\n"
        "        raise AssertionError('matching sentinel must exhaust the iterator')\n"
        "assert current == 3\n"
        "print('ok')\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "ok\n"


def test_callable_iter_equality_truth_error_propagates_without_exhausting(tmp_path):
    source = """current = 0
def produce():
    global current
    current += 1
    return current
class Truth:
    def __init__(self, value):
        self.value = value
    def __bool__(self):
        if self.value == 1:
            raise ValueError('comparison truth failed')
        return self.value == 3
class Sentinel:
    def __eq__(self, value):
        return Truth(value)
iterator = iter(produce, Sentinel())
try:
    next(iterator)
except ValueError:
    pass
else:
    raise AssertionError('comparison truth error was suppressed')
assert next(iterator) == 2
try:
    next(iterator)
except StopIteration:
    pass
else:
    raise AssertionError('truthy comparison must stop iteration')
assert next(iterator, None) is None
assert current == 3
print('ok')
"""
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "ok\n"


def test_callable_iter_validates_callable_and_arity_without_invoking_callback(tmp_path):
    source = """class Producer:
    def __call__(self):
        raise AssertionError('construction invoked the callback')
    def __bool__(self):
        return False
producer = Producer()
iterator = iter(producer, None)
assert iter(iterator) is iterator
for value in (None, 1, []):
    try:
        iter(value, None)
    except TypeError:
        pass
    else:
        raise AssertionError('construction accepted a non-callable')
try:
    iter()
except TypeError:
    pass
else:
    raise AssertionError('iter accepted zero arguments')
try:
    iter(producer, None, 0)
except TypeError:
    pass
else:
    raise AssertionError('iter accepted three arguments')
try:
    iter(producer, sentinel=None)
except TypeError:
    pass
else:
    raise AssertionError('iter accepted a keyword argument')
class InvalidCall:
    __call__ = None
invalid = iter(InvalidCall(), None)
try:
    next(invalid)
except TypeError:
    pass
else:
    raise AssertionError('non-callable __call__ value executed successfully')
assert list(iter([1, 2])) == [1, 2]
print('ok')
"""
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "ok\n"
