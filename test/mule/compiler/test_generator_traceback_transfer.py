"""An existing traceback chain accompanies an exception thrown into a generator."""

from test.mule.compiler.test_exception_groups import compare


def test_assertion_traceback_survives_hook_relays_after_handler_exit(tmp_path):
    result = compare(
        tmp_path,
        """
def fail():
    assert False
def hook():
    try:
        yield
    except BaseException as error:
        raise error
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
""",
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
