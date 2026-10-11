"""A tiny round-robin event loop: tasks interleaving at explicit yield points, sleeping, joining, and errors."""

from collections import deque

trace = []


class Yield:
    def __await__(self):
        yield ("yield",)


class Sleep:
    def __init__(self, ticks):
        self.ticks = ticks

    def __await__(self):
        yield ("sleep", self.ticks)


class Join:
    def __init__(self, task):
        self.task = task

    def __await__(self):
        while not self.task.done:
            yield ("yield",)
        if self.task.error is not None:
            raise self.task.error
        return self.task.result


class Task:
    def __init__(self, name, coro):
        self.name = name
        self.coro = coro
        self.done = False
        self.result = None
        self.error = None


class Loop:
    def __init__(self):
        self.ready = deque()
        self.sleeping = []
        self.tick = 0

    def spawn(self, name, coro):
        task = Task(name, coro)
        self.ready.append(task)
        return task

    def run(self):
        while self.ready or self.sleeping:
            if not self.ready:
                self.tick += 1
                still = []
                for wake_at, order, task in sorted(self.sleeping, key=lambda s: (s[0], s[1])):
                    if wake_at <= self.tick:
                        self.ready.append(task)
                    else:
                        still.append((wake_at, order, task))
                self.sleeping = still
                continue
            task = self.ready.popleft()
            try:
                request = task.coro.send(None)
            except StopIteration as e:
                task.done = True
                task.result = e.value
                trace.append(("done", task.name, e.value))
                continue
            except Exception as e:
                task.done = True
                task.error = e
                trace.append(("failed", task.name, type(e).__name__))
                continue
            if request[0] == "sleep":
                self.sleeping.append((self.tick + request[1], len(trace), task))
            else:
                self.ready.append(task)


async def worker(name, steps):
    for i in range(steps):
        trace.append((name, i))
        await Yield()
    return name + " finished"


loop = Loop()
loop.spawn("A", worker("A", 3))
loop.spawn("B", worker("B", 2))
loop.spawn("C", worker("C", 1))
loop.run()
print("round robin", trace)

trace.clear()


async def sleeper(name, ticks):
    trace.append((name, "sleep", ticks))
    await Sleep(ticks)
    trace.append((name, "woke"))
    return ticks


loop = Loop()
for name, ticks in (("slow", 3), ("fast", 1), ("mid", 2)):
    loop.spawn(name, sleeper(name, ticks))
loop.run()
print("sleep ordering", trace)

trace.clear()


async def child(v):
    await Yield()
    return v * 2


async def parent():
    t1 = loop.spawn("c1", child(5))
    t2 = loop.spawn("c2", child(7))
    r1 = await Join(t1)
    r2 = await Join(t2)
    return r1 + r2


loop = Loop()
p = loop.spawn("parent", parent())
loop.run()
print("join children", p.result, trace)

trace.clear()


async def failing():
    await Yield()
    raise ValueError("task failed")


async def supervisor():
    t = loop.spawn("failing", failing())
    try:
        await Join(t)
    except ValueError as e:
        return "supervisor saw " + str(e)


loop = Loop()
s = loop.spawn("supervisor", supervisor())
loop.run()
print("error propagates through join", s.result, trace)

trace.clear()


async def producer(queue, n):
    for i in range(n):
        queue.append(i)
        trace.append(("put", i))
        await Yield()
    queue.append(None)


async def consumer(queue):
    total = 0
    while True:
        while not queue:
            await Yield()
        item = queue.popleft()
        if item is None:
            return total
        trace.append(("got", item))
        total += item


loop = Loop()
q = deque()
cons = loop.spawn("consumer", consumer(q))
loop.spawn("producer", producer(q, 4))
loop.run()
print("producer/consumer", cons.result, trace)
