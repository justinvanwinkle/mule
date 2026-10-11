"""Nested try, bare raise after an inner exception was handled, raise with no active exception, classes vs instances, non-exceptions."""


# Bare raise re-raises the exception being handled by the *current* handler,
# even after a nested exception was raised and handled inside it.
def bare_raise_after_inner():
    try:
        raise ValueError("outer")
    except ValueError:
        try:
            raise KeyError("inner")
        except KeyError:
            pass
        raise


try:
    bare_raise_after_inner()
except Exception as e:
    print("bare raise after inner handled:", type(e).__name__, e)


def bare_raise_inside_inner_handler():
    try:
        raise ValueError("outer")
    except ValueError:
        try:
            raise KeyError("inner")
        except KeyError:
            raise


try:
    bare_raise_inside_inner_handler()
except Exception as e:
    print("bare raise inside inner:", type(e).__name__, "context", type(e.__context__).__name__)


def helper_reraise():
    raise


def bare_raise_from_called_function():
    try:
        raise IndexError("from caller")
    except IndexError:
        helper_reraise()


try:
    bare_raise_from_called_function()
except IndexError as e:
    print("bare raise in callee sees caller's exception:", e)

try:
    raise
except RuntimeError as e:
    print("raise with nothing active:", type(e).__name__)

try:
    helper_reraise()
except RuntimeError as e:
    print("callee raise with nothing active:", type(e).__name__)


def bare_raise_after_handler_finished():
    try:
        raise ValueError
    except ValueError:
        pass
    raise


try:
    bare_raise_after_handler_finished()
except RuntimeError as e:
    print("after handler finished:", type(e).__name__)
except ValueError:
    print("wrong: stale exception re-raised")


# raise e inside its own handler vs bare raise
try:
    try:
        raise ValueError("v")
    except ValueError as e:
        raise e
except ValueError as e:
    print("raise e keeps identity, context:", e.__context__)

# Classes vs instances.
try:
    raise ZeroDivisionError
except ZeroDivisionError as e:
    print("raised class:", type(e).__name__, e.args)

try:
    raise ZeroDivisionError()
except ZeroDivisionError as e:
    print("raised instance:", type(e).__name__, e.args)

saved = KeyError("same")
for i in range(2):
    try:
        raise saved
    except KeyError as e:
        print("reraised same object", i, e is saved)

# Raising non-exceptions.
for bad in [42, "text", None, object, int, [ValueError]]:
    try:
        raise bad
    except TypeError as e:
        print("raise", repr(bad), "->", type(e).__name__)


class NotExc:
    pass


try:
    raise NotExc()
except TypeError as e:
    print("raise plain object instance ->", type(e).__name__)

try:
    raise ValueError from 5
except TypeError as e:
    print("raise from non-exception ->", type(e).__name__)


# Class whose __init__ fails while being raised.
class BadInit(Exception):
    def __init__(self):
        raise ArithmeticError("init failed")


try:
    raise BadInit
except ArithmeticError as e:
    print("init failure surfaces:", e)


# Deeply nested try statements; each level handles a different type.
def nested(kind):
    log = []
    try:
        try:
            try:
                raise kind("deep")
            except KeyError:
                log.append("L3 KeyError")
                raise
            finally:
                log.append("L3 finally")
        except ValueError:
            log.append("L2 ValueError")
        finally:
            log.append("L2 finally")
    except LookupError as e:
        log.append("L1 " + type(e).__name__)
    return log


print("nested KeyError", nested(KeyError))
print("nested ValueError", nested(ValueError))
print("nested IndexError", nested(IndexError))
