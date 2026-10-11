"""except ... as e unbinds the name afterwards, even when it shadowed an outer binding."""

try:
    raise ValueError("first")
except ValueError as err:
    print("inside handler", err)

try:
    print(err)
except NameError as e:
    print("after handler", type(e).__name__)

# shadowing an outer module name: the outer binding is deleted too
shadow = "outer value"
try:
    raise KeyError("k")
except KeyError as shadow:
    print("handler sees exception", type(shadow).__name__)
print("shadowed name deleted", "shadow" in globals())


def local_shadow():
    e = "local before"
    try:
        1 / 0
    except ZeroDivisionError as e:
        pass
    try:
        return e
    except UnboundLocalError as err:
        return type(err).__name__


print("local shadow unbound", local_shadow())


# keeping a reference by rebinding inside the handler
def keep_reference():
    saved = None
    try:
        raise RuntimeError("kept")
    except RuntimeError as e:
        saved = e
    return str(saved)


print("saved reference", keep_reference())


# handler not taken: the name is untouched
untouched = "still here"
try:
    pass
except Exception as untouched:
    pass
print("handler not taken", untouched)

# unbinding happens even if the handler returns, breaks, or re-raises
def return_from_handler():
    try:
        raise TypeError
    except TypeError as e:
        return "returned " + type(e).__name__


print(return_from_handler())

for attempt in range(2):
    try:
        raise IndexError
    except IndexError as loop_err:
        break
print("after break", "loop_err" in globals())

# deleting the name inside the handler does not cause an error at handler exit
try:
    raise ValueError
except ValueError as gone:
    del gone
print("deleted inside handler", "gone" in globals())

# rebinding inside the handler is still unbound afterwards
try:
    raise ValueError
except ValueError as rebound:
    rebound = "rebound string"
print("rebound in handler then deleted", "rebound" in globals())


# closure capturing the except variable
def closure_capture():
    try:
        raise LookupError("captured")
    except LookupError as e:
        f = lambda: e
    try:
        return f()
    except NameError as err:
        return type(err).__name__


print("closure over except var", closure_capture())

# except* also unbinds
try:
    raise ExceptionGroup("grp", [ValueError(1), TypeError(2)])
except* ValueError as eg:
    print("except* value", [type(x).__name__ for x in eg.exceptions])
except* TypeError as eg:
    print("except* type", [type(x).__name__ for x in eg.exceptions])
print("except* var unbound", "eg" in globals())

# nested handlers with the same name
try:
    raise ValueError("outer")
except ValueError as e:
    try:
        raise KeyError("inner")
    except KeyError as e:
        print("inner handler", type(e).__name__)
    try:
        print("outer e after inner", e)
    except NameError as ne:
        print("outer e after inner", type(ne).__name__)

# exception variable in a class body
class Body:
    try:
        raise ValueError
    except ValueError as cls_err:
        caught = type(cls_err).__name__


print("class body handler", Body.caught, hasattr(Body, "cls_err"))

# PEP 758 unparenthesized except list without as
try:
    raise KeyError
except KeyError, IndexError:
    print("pep 758 except list")
