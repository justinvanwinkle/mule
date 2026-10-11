"""Generator method on a metaclass-built class: match inside try/finally inside with, driven by send()."""


class Registry(type):
    created = []

    def __new__(mcls, name, bases, ns, **kw):
        cls = super().__new__(mcls, name, bases, ns)
        cls.tag = kw.get("tag", "none")
        Registry.created.append(name)
        return cls

    def __call__(cls, *args):
        print("metaclass __call__", cls.__name__, args)
        return super().__call__(*args)


class Ctx:
    def __init__(self, log):
        self.log = log

    def __enter__(self):
        self.log.append("enter")
        return self

    def __exit__(self, et, ev, tb):
        self.log.append(("exit", et.__name__ if et else None))
        return et is KeyError


class Machine(metaclass=Registry, tag="m1"):
    def __init__(self, name):
        self.name = name
        self.log = []

    def run(self):
        state = "start"
        with Ctx(self.log):
            while True:
                try:
                    cmd = yield state
                    match cmd:
                        case ("go", int(n)) if n > 0:
                            state = f"go{n}"
                        case ("go", n):
                            state = f"bad-go:{n!r}"
                        case {"set": str(s)}:
                            state = s
                        case [*_, "stop"]:
                            return f"{self.name} stopped from {state}"
                        case "boom":
                            raise KeyError("boom")
                        case None:
                            state = "idle"
                        case _:
                            state = f"?{cmd!r}"
                finally:
                    self.log.append(("finally", state))


print("registry", Registry.created, Machine.tag)
m = Machine("alpha")
g = m.run()
print("prime", next(g))
for cmd in [("go", 3), ("go", -1), ("go", "x"), {"set": "custom"}, None, 3.5]:
    print("send", cmd, "->", g.send(cmd))
try:
    g.send(["a", "b", "stop"])
except StopIteration as e:
    print("returned", e.value)
print("log", m.log)

m2 = Machine("beta")
g2 = m2.run()
next(g2)
g2.send(("go", 1))
try:
    g2.send("boom")
except StopIteration as e:
    print("boom suppressed by with, return value", e.value)
print("log2", m2.log)

m3 = Machine("gamma")
g3 = m3.run()
next(g3)
g3.close()
print("closed log", m3.log)

m4 = Machine("delta")
g4 = m4.run()
next(g4)
try:
    g4.throw(ValueError("thrown"))
except ValueError as e:
    print("throw propagated", e)
print("throw log", m4.log)


class Sub(Machine, tag="sub"):
    def run(self):
        result = yield from super().run()
        yield ("sub-done", result)


s = Sub("eps")
gs = s.run()
print("sub prime", next(gs))
print("sub send", gs.send(("go", 2)))
print("sub final", gs.send(["stop"]))
print("sub tag", Sub.tag, "registry", Registry.created)
