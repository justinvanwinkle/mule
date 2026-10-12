"""Builtin exception hierarchy: catching subclasses through their base classes."""


def catch_with(base, exc):
    try:
        raise exc
    except base as e:
        return "caught " + type(e).__name__ + " via " + base.__name__
    except BaseException as e:
        return "missed " + type(e).__name__ + " by " + base.__name__


print(catch_with(LookupError, KeyError("k")))
print(catch_with(LookupError, IndexError(0)))
print(catch_with(LookupError, ValueError("v")))
print(catch_with(ArithmeticError, ZeroDivisionError("z")))
print(catch_with(ArithmeticError, OverflowError("o")))
print(catch_with(ArithmeticError, FloatingPointError("f")))
print(catch_with(Exception, KeyboardInterrupt()))
print(catch_with(Exception, SystemExit(0)))
print(catch_with(Exception, GeneratorExit()))
print(catch_with(BaseException, KeyboardInterrupt()))
print(catch_with(BaseException, SystemExit(5)))
print(catch_with(BaseException, GeneratorExit()))
print(catch_with(Exception, StopIteration()))
print(catch_with(NameError, UnboundLocalError("u")))
print(catch_with(RuntimeError, RecursionError("r")))
print(catch_with(RuntimeError, NotImplementedError("n")))
print(catch_with(ValueError, UnicodeDecodeError("utf-8", b"\xff", 0, 1, "bad")))
print(catch_with(OSError, FileNotFoundError("f")))
print(catch_with(OSError, OSError("io")))
print(catch_with(Exception, AssertionError("a")))
print(catch_with(ImportError, ModuleNotFoundError("m")))

print("IOError is OSError", IOError is OSError)
print("EnvironmentError is OSError", EnvironmentError is OSError)

chains = [KeyError, IndexError, ZeroDivisionError, OverflowError,
          UnboundLocalError, RecursionError, StopIteration, KeyboardInterrupt,
          SystemExit, GeneratorExit, ExceptionGroup, BaseExceptionGroup,
          ModuleNotFoundError, FileNotFoundError, StopAsyncIteration]
for cls in chains:
    names = [c.__name__ for c in cls.__mro__ if c is not object]
    print("mro", " -> ".join(names))

print("issubclass KeyError LookupError", issubclass(KeyError, LookupError))
print("issubclass KeyboardInterrupt Exception", issubclass(KeyboardInterrupt, Exception))
print("issubclass bool tuple", issubclass(ZeroDivisionError, (TypeError, ArithmeticError)))
print("isinstance SystemExit BaseException", isinstance(SystemExit(), BaseException))

# Manually raised SystemExit passes through an `except Exception` and is caught outside.
log = []
try:
    try:
        raise SystemExit(7)
    except Exception:
        log.append("inner Exception handler")
    finally:
        log.append("inner finally")
except SystemExit as e:
    log.append("outer SystemExit code=" + repr(e.code))
print("system exit flow", log)

log = []
try:
    try:
        raise KeyboardInterrupt
    except Exception:
        log.append("wrong")
except BaseException as e:
    log.append("base got " + type(e).__name__)
print("keyboard interrupt flow", log)

# The first matching handler wins even when a later one is more specific.
try:
    {}["missing"]
except LookupError:
    print("first matching handler: LookupError")
except KeyError:
    print("first matching handler: KeyError")
