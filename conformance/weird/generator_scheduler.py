"""A tiny round-robin coroutine scheduler using generators and send(): spawning, message passing, joining, and failures."""

from collections import deque
from dataclasses import dataclass


@dataclass
class Spawn:
    gen: object
    name: str


@dataclass
class Send:
    to: str
    msg: object


@dataclass
class Recv:
    pass


@dataclass
class Join:
    name: str


class Scheduler:
    def __init__(self):
        self.ready = deque()
        self.mailboxes = {}
        self.waiting_recv = {}
        self.joiners = {}
        self.results = {}
        self.trace = []

    def spawn(self, name, gen):
        self.mailboxes[name] = deque()
        self.ready.append((name, gen, None))

    def run(self):
        while self.ready:
            name, gen, value = self.ready.popleft()
            try:
                request = gen.send(value)
            except StopIteration as stop:
                self.finish(name, ("ok", stop.value))
                continue
            except Exception as e:
                self.finish(name, ("error", f"{type(e).__name__}: {e}"))
                continue
            match request:
                case Spawn(gen=child, name=child_name):
                    self.trace.append(f"{name} spawns {child_name}")
                    self.spawn(child_name, child)
                    self.ready.append((name, gen, child_name))
                case Send(to=target, msg=msg) if target in self.mailboxes:
                    self.mailboxes[target].append((name, msg))
                    if target in self.waiting_recv:
                        tgen = self.waiting_recv.pop(target)
                        self.ready.append((target, tgen, self.mailboxes[target].popleft()))
                    self.ready.append((name, gen, True))
                case Send(to=target):
                    self.ready.append((name, gen, False))
                case Recv():
                    if self.mailboxes[name]:
                        self.ready.append((name, gen, self.mailboxes[name].popleft()))
                    else:
                        self.waiting_recv[name] = gen
                case Join(name=other):
                    if other in self.results:
                        self.ready.append((name, gen, self.results[other]))
                    else:
                        self.joiners.setdefault(other, []).append((name, gen))
                case None:
                    self.ready.append((name, gen, None))
                case bad:
                    gen.close()
                    self.finish(name, ("error", f"bad request {bad!r}"))

    def finish(self, name, result):
        self.results[name] = result
        self.trace.append(f"{name} finished {result}")
        for jname, jgen in self.joiners.pop(name, []):
            self.ready.append((jname, jgen, result))


def worker(label, n):
    total = 0
    for i in range(n):
        total += i
        yield
    return f"{label}:{total}"


def echo():
    count = 0
    while True:
        sender, msg = yield Recv()
        if msg == "stop":
            return count
        count += 1
        yield Send(sender, ("echo", msg))


def client(name, msgs):
    replies = []
    for m in msgs:
        delivered = yield Send("echo", m)
        if not delivered:
            replies.append("undelivered")
            continue
        _, reply = yield Recv()
        replies.append(reply)
    return replies


def boss():
    yield Spawn(worker("w1", 3), "w1")
    yield Spawn(worker("w2", 5), "w2")
    yield Spawn(echo(), "echo")
    yield Spawn(client("c1", ["a", "b"]), "c1")
    yield Spawn(client("c2", ["x"]), "c2")
    r1 = yield Join("w1")
    r2 = yield Join("w2")
    c1 = yield Join("c1")
    c2 = yield Join("c2")
    ok = yield Send("nobody", "hello")
    yield Send("echo", "stop")
    e = yield Join("echo")
    yield Spawn(failing(), "bad")
    bad = yield Join("bad")
    yield Spawn(weird_request(), "weird")
    weird = yield Join("weird")
    return {"w": [r1, r2], "c": [c1, c2], "nobody": ok, "echo": e, "bad": bad, "weird": weird}


def failing():
    yield
    raise ValueError("task blew up")


def weird_request():
    try:
        yield 42
    finally:
        print("weird task closed")


s = Scheduler()
s.spawn("boss", boss())
s.run()
for line in s.trace:
    print(line)
print("boss result", s.results["boss"])
print("all finished", sorted(s.results))
