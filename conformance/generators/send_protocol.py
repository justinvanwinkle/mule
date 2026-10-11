"""Generator send(): yield as expression, priming, send-before-start TypeError, coroutine-style accumulators."""


def echo():
    received = []
    while True:
        value = yield len(received)
        if value is None:
            break
        received.append(value)
    return received


g = echo()
print("prime with next", next(g))
print("send a", g.send("a"))
print("send b", g.send("b"))
try:
    g.send(None)
except StopIteration as e:
    print("return via StopIteration", e.value)

g = echo()
try:
    g.send("too early")
except TypeError as e:
    print("send non-None before start", type(e).__name__)
print("send(None) to start", g.send(None))


def averager():
    total = 0.0
    count = 0
    avg = None
    while True:
        term = yield avg
        total += term
        count += 1
        avg = total / count


avg = averager()
next(avg)
print("running average", [avg.send(v) for v in (10, 20, 30, 40)])


def yield_expression_value():
    x = yield 1
    y = yield x * 2
    z = (yield) or "default"
    yield (x, y, z)


g = yield_expression_value()
print("yield expr 1", next(g))
print("yield expr 2", g.send(5))
print("yield expr 3", g.send("why"))
print("yield expr 4", g.send(None))


def next_sends_none():
    got = []
    for _ in range(3):
        got.append((yield))
    yield got


g = next_sends_none()
next(g)
next(g)
g.send("x")
print("next sends None", g.send("y"))


def yield_in_expressions():
    a = [(yield "first"), (yield "second")]
    b = {"k": (yield "third")}
    c = (yield "fourth") + (yield "fifth")
    yield a, b, c


g = yield_in_expressions()
outputs = [next(g)]
for v in ("A", "B", "C", 10):
    outputs.append(g.send(v))
outputs.append(g.send(20))
print("yield in expressions", outputs)


def yield_in_call_args():
    result = max((yield "x?"), (yield "y?"))
    yield result


g = yield_in_call_args()
next(g)
g.send(3)
print("yield as call args", g.send(8))


def running():
    yield "running state send"


g = running()
next(g)
try:
    g.send("value")
except StopIteration as e:
    print("send into finished body", type(e).__name__, e.value)

try:
    g.send(1)
except StopIteration:
    print("send to exhausted raises StopIteration")


def reentrant():
    yield gen_ref.send(None)


gen_ref = reentrant()
try:
    next(gen_ref)
except ValueError as e:
    print("generator already executing", type(e).__name__)


def state_machine():
    state = "idle"
    while True:
        cmd = yield state
        if cmd == "start" and state == "idle":
            state = "running"
        elif cmd == "pause" and state == "running":
            state = "paused"
        elif cmd == "resume" and state == "paused":
            state = "running"
        elif cmd == "stop":
            return "stopped from " + state


sm = state_machine()
seen = [next(sm)]
for c in ("start", "pause", "bogus", "resume"):
    seen.append(sm.send(c))
try:
    sm.send("stop")
except StopIteration as e:
    seen.append(e.value)
print("state machine", seen)
