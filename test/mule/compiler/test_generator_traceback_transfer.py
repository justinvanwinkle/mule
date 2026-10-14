"""An existing traceback chain accompanies an exception thrown into a generator."""

import pytest

from test.mule.compiler.test_exception_groups import compare


def test_context_manager_receives_original_traceback_and_preserves_it(tmp_path):
    compare(
        tmp_path,
        """
from contextlib import contextmanager
import sys
def fail():
    assert False
class Manager:
    def __enter__(self):
        return self
    def __exit__(self, kind, value, traceback):
        print(kind is AssertionError, traceback is value.__traceback__)
        print(sys.exception() is value)
        return False
@contextmanager
def generator_manager():
    yield
for manager in [Manager(), generator_manager()]:
    try:
        with manager:
            fail()
    except AssertionError as error:
        traceback = error.__traceback__
        names = []
        while traceback is not None:
            names.append(traceback.tb_frame.f_code.co_name)
            traceback = traceback.tb_next
        print('fail' in names)
    print(sys.exception())
""",
    )


@pytest.mark.parametrize(
    "action", ["return False", "return True", "raise ValueError('replacement')"]
)
def test_context_exit_restores_outer_handler_on_all_paths(tmp_path, action):
    compare(
        tmp_path,
        """
import sys
class Manager:
    def __enter__(self):
        pass
    def __exit__(self, kind, value, traceback):
        print(sys.exception() is value, traceback is value.__traceback__)
        EXIT_ACTION
try:
    raise KeyError('outer')
except KeyError as outer:
    try:
        with Manager():
            raise AssertionError('inner')
    except BaseException as error:
        print(type(error).__name__, type(error.__context__).__name__)
    print(sys.exception() is outer)
print(sys.exception())
""".replace("EXIT_ACTION", action),
    )


@pytest.mark.parametrize("suite", ["pass", "...", "'ignored'"])
def test_caught_attribute_error_does_not_leak_into_later_assertion(tmp_path, suite):
    result = compare(
        tmp_path,
        """
import sys
def fail():
    assert False
def run():
    try:
        del sys.never_defined
    except AttributeError:
        HANDLER_SUITE
    print(sys.exception())
    try:
        fail()
    except AssertionError as error:
        print(error.__context__)
        traceback = error.__traceback__
        names = []
        while traceback is not None:
            names.append(traceback.tb_frame.f_code.co_name)
            traceback = traceback.tb_next
        print('fail' in names)
run()
""".replace("HANDLER_SUITE", suite),
    )
    assert result.stderr == ""


@pytest.mark.parametrize(
    "hook_body",
    [
        "    try:\n        yield\n    except BaseException as error:\n        raise error\n",
        "    try:\n        yield\n    finally:\n        pass\n",
        "    yield\n",
    ],
)
def test_assertion_traceback_survives_hook_relays_after_handler_exit(
    tmp_path, hook_body
):
    result = compare(
        tmp_path,
        """
def fail():
    assert False
def hook():
HOOK_BODY
def run():
    wrappers = [hook(), hook()]
    for wrapper in wrappers:
        next(wrapper)
    exception = None
    previous = None
    try:
        fail()
    except BaseException as error:
        exception = error
        previous = error.__traceback__
    finally:
        for wrapper in reversed(wrappers):
            try:
                wrapper.throw(exception)
            except BaseException as error:
                exception = error
    traceback = exception.__traceback__
    found_previous, found_fail = False, False
    while traceback is not None:
        found_previous = found_previous or traceback is previous
        found_fail = found_fail or traceback.tb_frame.f_code.co_name == 'fail'
        traceback = traceback.tb_next
    print(type(exception).__name__, found_previous, found_fail)
run()
""".replace("HOOK_BODY\n", hook_body),
    )
    assert result.stderr == ""


def test_suspended_handler_state_is_private_and_close_preserves_generator_exit(
    tmp_path,
):
    native = compare(
        tmp_path,
        """
import sys
def generator():
    try:
        raise ValueError('inside')
    except ValueError:
        yield str(sys.exception())
        print('resumed', str(sys.exception()))
        yield 2
        print('finished')
first = generator()
print(next(first), sys.exception())
try:
    raise KeyError('outside')
except KeyError:
    print(next(first), type(sys.exception()).__name__)
print(sys.exception())
first.close()
second = generator()
print(next(second), sys.exception())
second.close()
print('closed', sys.exception())
""",
    )
    assert native.stderr == ""


def test_suspended_generators_do_not_remain_in_live_frame_chain(tmp_path):
    compare(
        tmp_path,
        """
import sys
def paused():
    yield 1
def active():
    frame = sys._getframe()
    names = []
    while frame is not None:
        names.append(frame.f_code.co_name)
        frame = frame.f_back
    print('paused' in names, 'active' in names, 'delegating' in names)
    return 42
def delegating():
    yield active()
first = paused()
print(next(first))
active()
second = delegating()
print(next(second))
first.close()
second.close()
""",
    )


def test_thrown_exception_retains_original_traceback_and_identity(tmp_path):
    compare(
        tmp_path,
        """
def fail():
    return missing
def wrapper():
    yield
generator = wrapper()
next(generator)
try:
    fail()
except NameError as original:
    previous = original.__traceback__
    try:
        generator.throw(original)
    except NameError as caught:
        chain = caught.__traceback__
        while chain is not None and chain is not previous:
            chain = chain.tb_next
        print(caught is original, chain is previous)
        print(previous.tb_next.tb_frame.f_code.co_name)
""",
    )
