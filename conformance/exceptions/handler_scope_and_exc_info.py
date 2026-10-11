"""Exception variable deletion after a handler, sys.exc_info()/sys.exception(), add_note/__notes__, with_traceback(None)."""
import sys

e = "outer value"
try:
    raise ValueError("bound")
except ValueError as e:
    print("inside handler:", e)
try:
    print(e)
except NameError as err:
    print("after handler, name deleted:", type(err).__name__)


def local_deleted():
    err = "before"
    try:
        1 / 0
    except ZeroDivisionError as err:
        pass
    try:
        return err
    except UnboundLocalError as u:
        return "UnboundLocalError: " + type(u).__name__


print("function local:", local_deleted())


def keep_reference():
    saved = None
    try:
        raise KeyError("kept")
    except KeyError as exc:
        saved = exc
    return saved


print("saved reference survives:", repr(keep_reference()))


def handler_del_then_rebind():
    try:
        raise ValueError
    except ValueError as v:
        del v
        v = "rebound in handler"
    return "v" in locals()


print("rebind then implicit delete leaves v:", handler_del_then_rebind())

print("exc_info outside:", sys.exc_info())
print("exception outside:", sys.exception())

try:
    raise TypeError("t")
except TypeError:
    t, v, tb = sys.exc_info()
    print("exc_info inside:", t.__name__, v, tb is not None)
    print("sys.exception inside:", repr(sys.exception()))
    try:
        raise KeyError("nested")
    except KeyError:
        print("nested exc_info:", sys.exc_info()[0].__name__)
    print("restored exc_info:", sys.exc_info()[0].__name__)
print("cleared after:", sys.exc_info())


def peek():
    return sys.exception()


try:
    raise IndexError("visible to callee")
except IndexError:
    print("callee sees:", repr(peek()))

try:
    pass
finally:
    print("finally without exception sees:", sys.exception())

try:
    try:
        raise ValueError("in finally")
    finally:
        print("finally during propagation sees:", repr(sys.exception()))
except ValueError:
    pass

# add_note and __notes__
err = ValueError("noted")
print("no notes yet:", hasattr(err, "__notes__"))
err.add_note("first note")
err.add_note("second note")
print("notes:", err.__notes__)
try:
    err.add_note(42)
except TypeError as te:
    print("non-str note ->", type(te).__name__)

try:
    try:
        raise KeyError("k")
    except KeyError as k:
        k.add_note("added in handler")
        raise
except KeyError as k:
    print("notes survive reraise:", k.__notes__)

err.__notes__ = ["replaced"]
err.add_note("appended")
print("after replacing list:", err.__notes__)

# with_traceback
try:
    raise RuntimeError("tb")
except RuntimeError as r:
    print("has traceback:", r.__traceback__ is not None)
    same = r.with_traceback(None)
    print("with_traceback returns self:", same is r, "tb now:", r.__traceback__)

fresh = OSError("never raised")
print("unraised traceback:", fresh.__traceback__)
try:
    raise fresh.with_traceback(None)
except OSError as o:
    print("raised after with_traceback(None):", o is fresh, o.__traceback__ is not None)
