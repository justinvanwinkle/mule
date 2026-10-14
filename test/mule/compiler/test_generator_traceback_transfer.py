"""An existing traceback chain accompanies an exception thrown into a generator."""

from test.mule.compiler.test_exception_groups import compare


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
