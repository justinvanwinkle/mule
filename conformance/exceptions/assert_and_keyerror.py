"""assert statements (messages, lazily evaluated side-effecting messages) and KeyError with tuple keys."""

calls = []


def message(text):
    calls.append(text)
    return text


assert True, message("never evaluated")
print("message not evaluated when assert passes:", calls)

try:
    assert 1 == 2, message("evaluated once")
except AssertionError as e:
    print("assert message:", e.args, "calls", calls)

try:
    assert []
except AssertionError as e:
    print("no message args:", e.args, repr(str(e)))

try:
    assert 0, ("tuple", "message")
except AssertionError as e:
    print("tuple message is single arg:", e.args)

try:
    assert None, 42
except AssertionError as e:
    print("non-str message:", e.args, str(e))

try:
    assert False, ValueError("an exception object as message")
except AssertionError as e:
    print("exception as message:", type(e.args[0]).__name__, e.args[0])


def checked(n):
    assert n > 0, f"n must be positive, got {n}"
    return n * 2


for n in (3, -1):
    try:
        print("checked", checked(n))
    except AssertionError as e:
        print("checked failed:", e)


# The message expression can itself raise; that exception replaces AssertionError.
try:
    assert False, 1 / 0
except ZeroDivisionError as e:
    print("message raised:", type(e).__name__)


# The assertion condition is evaluated exactly once.
counter = [0]


def cond():
    counter[0] += 1
    return counter[0] > 5


try:
    assert cond(), counter[0]
except AssertionError as e:
    print("condition calls:", counter[0], "message saw", e.args[0])

# AssertionError is an ordinary Exception subclass.
try:
    assert False
except Exception as e:
    print("caught as Exception:", type(e).__name__)

# KeyError with tuple keys: the key is the single argument, str shows its repr.
d = {(1, 2): "pair", ("a",): "single"}
for key in [(1, 3), ("b",), (), ("x", (1, 2))]:
    try:
        d[key]
    except KeyError as e:
        print("KeyError tuple key:", e.args, "str", str(e), "args[0] is key", e.args[0] == key)

for key in ["plain", 'quote"s', "it's", 3.5, None, b"by"]:
    try:
        {}[key]
    except KeyError as e:
        print("KeyError str:", str(e))

try:
    {}.pop(("p", 1))
except KeyError as e:
    print("pop tuple key:", str(e))

try:
    del d[(9, 9)]
except KeyError as e:
    print("del tuple key:", str(e))


class Missing(dict):
    def __missing__(self, key):
        raise KeyError(("custom", key))


try:
    Missing()["z"]
except KeyError as e:
    print("__missing__ raised:", str(e), e.args)

try:
    set().remove((1, 2))
except KeyError as e:
    print("set remove tuple:", str(e))
