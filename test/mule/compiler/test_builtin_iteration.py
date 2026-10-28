"""Builtin sequence iterators bypass subclass indexing overrides."""

from test.mule.compiler.test_exception_groups import compare


def test_list_and_tuple_iterators_use_storage_and_stay_exhausted(tmp_path):
    compare(
        tmp_path,
        """
class Tuple(tuple):
    def __getitem__(self, key):
        raise AssertionError('index override called')
class List(list):
    def __getitem__(self, key):
        raise AssertionError('index override called')
for sequence in (Tuple((1, 2, 3)), List((4, 5, 6))):
    print(list(sequence), tuple(sequence))
    iterator = iter(sequence)
    print(next(iterator), next(iterator), next(iterator))
    print(next(iterator, 'finished'), next(iterator, 'still finished'))
sequence = List([1])
iterator = iter(sequence)
print(next(iterator))
sequence.append(2)
print(next(iterator))
print(next(iterator, 'finished'))
sequence.append(3)
print(next(iterator, 'still finished'))
""",
    )


def test_generator_and_coroutine_fast_paths_and_stack_recycling(tmp_path):
    compare(
        tmp_path,
        """
def pairs(n):
    for i in range(n):
        yield (i, i * 10)
    return "done"

acc = []
gen = pairs(4)
for a, b in gen:
    acc.append((a, b))
for a, b in gen:
    acc.append((a, b))
print(acc)

def raiser():
    yield 1
    raise StopIteration("boom")

seen = []
try:
    for item in raiser():
        seen.append(item)
except RuntimeError as err:
    print(seen, type(err.__cause__).__name__, err.__cause__.value)

def subgen(n):
    total = 0
    for i in range(n):
        received = yield i
        if received is not None:
            total += received
        else:
            total += i
    return total

def delegator(n):
    res = yield from subgen(n)
    yield ("result", res)
    return res * 2

d = delegator(3)
print(next(d))
print(d.send(10))
print(d.send(20))
try:
    next(d)
except StopIteration as stop:
    print("delegator return", stop.value)

def delegator_pep479():
    yield from raiser()

try:
    list(delegator_pep479())
except RuntimeError as err:
    print("yield_from pep479", type(err.__cause__).__name__)

def inner_catch():
    try:
        yield "wait"
    except ValueError as err:
        yield f"caught:{err}"
    return "inner_done"

def outer_catch():
    res = yield from inner_catch()
    yield f"outer:{res}"

g = outer_catch()
print(next(g))
print(g.throw(ValueError("ping")))
print(next(g))
g2 = outer_catch()
print(next(g2))
print("closed", g2.close())

async def fib(n):
    if n < 2:
        return n
    return await fib(n - 1) + await fib(n - 2)

coro = fib(10)
try:
    coro.send(None)
except StopIteration as stop:
    print("fib(10)", stop.value)

try:
    coro.send(None)
except RuntimeError as err:
    print("reuse", err)

total = 0
for i in range(500):
    unstarted = pairs(2)
    unstarted.close()
    partial = pairs(3)
    total += next(partial)[0]
    for x, y in pairs(2):
        total += x + y
print("total", total)
""",
    )
