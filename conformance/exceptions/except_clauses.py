"""Except clause ordering, tuples of types, PEP 758 unparenthesized lists, dynamic types."""


def classify(exc):
    try:
        raise exc
    except ZeroDivisionError:
        return "ZeroDivisionError clause"
    except (KeyError, IndexError) as e:
        return "tuple clause " + type(e).__name__
    except TypeError, AttributeError:
        return "pep758 clause"
    except ArithmeticError as e:
        return "ArithmeticError clause " + type(e).__name__
    except Exception:
        return "Exception clause"


for exc in [ZeroDivisionError(), KeyError(), IndexError(), TypeError(),
            AttributeError(), OverflowError(), ValueError()]:
    print(type(exc).__name__, "->", classify(exc))


# PEP 758 with three names and no `as`.
def pep758(exc):
    try:
        raise exc
    except ValueError, KeyError, NameError:
        return "three-name list"
    except BaseException:
        return "fallback"


for exc in [ValueError(), KeyError(), UnboundLocalError(), RuntimeError()]:
    print("pep758", type(exc).__name__, pep758(exc))


# Exception types computed at runtime.
handled = (OSError, LookupError)
try:
    [][1]
except handled as e:
    print("runtime tuple caught", type(e).__name__)

nested_tuple = (ValueError, (KeyError, (ZeroDivisionError,)))
try:
    try:
        1 / 0
    except nested_tuple as e:
        print("wrong: nested tuple caught", type(e).__name__)
except TypeError as e:
    print("nested tuple in except clause ->", type(e).__name__,
          "context", type(e.__context__).__name__)

empty = ()
try:
    try:
        raise ValueError("x")
    except empty:
        print("wrong: empty tuple matched")
except ValueError:
    print("empty tuple matches nothing")


# Handler expressions are evaluated lazily, in order, only until one matches.
order = []


def typ(name, t):
    order.append(name)
    return t


try:
    raise KeyError("k")
except typ("first", ValueError):
    pass
except typ("second", KeyError):
    pass
except typ("third", Exception):
    pass
print("evaluated handler expressions", order)

# A non-exception class in an except clause only errors if it is reached.
try:
    try:
        raise ValueError
    except ValueError:
        print("matched before bad clause")
    except 42:
        pass
except TypeError:
    print("wrong")

try:
    try:
        raise ValueError
    except "not a type":
        pass
except TypeError as e:
    print("bad except clause raises", type(e).__name__)


# Subclass listed after its base is unreachable but legal.
def shadowed():
    try:
        raise FileNotFoundError("f")
    except OSError:
        return "OSError first"
    except FileNotFoundError:
        return "unreachable"


print("shadowed", shadowed())
