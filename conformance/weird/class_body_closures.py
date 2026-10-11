"""Closures created in loops inside class bodies inside functions, early vs late binding, invoked from generators."""


def factory(n):
    class Holder:
        late = []
        early = []
        for i in range(n):
            late.append(lambda: i)          # class-body i: lambda can't see it, looks up global/enclosing
            early.append(lambda i=i: i * 10)
        tag = "cls"
        # comprehension in class body sees function locals (n) but not class names like tag except first iterable
        squares = [k * k for k in range(n)]

        def method(self):
            return [f() for f in self.early]

    return Holder


i = "global-i"
H = factory(3)
print("late binding sees global", [f() for f in H.late])
print("early binding", [f() for f in H.early])
print("squares", H.squares)
print("method", H().method())


def outer():
    i = "enclosing-i"

    class C:
        i = "class-i"
        fs = [lambda: i]
        gs = []
        for j in range(2):
            gs.append(lambda j=j: (i, j))

    return C


C = outer()
print("lambda in class sees enclosing not class", C.fs[0]())
print("defaults captured", [g() for g in C.gs])


def counter_maker():
    funcs = []
    for k in range(4):
        def f():
            return k
        funcs.append(f)
    k = 100
    return funcs


print("late binding rebinding after loop", [f() for f in counter_maker()])


def gen_invoker(funcs):
    for f in funcs:
        yield f()


def mixed():
    out = []
    for m in range(3):
        class Inner:
            val = m
            def get(self, m=m):
                return (self.val, m)
        out.append(Inner)
    return out


classes = mixed()
print("classes from loop", list(gen_invoker([c().get for c in classes])))


def nonlocal_through_class():
    x = 0

    class K:
        def bump(self):
            nonlocal x
            x += 1
            return x

    k = K()
    g = (k.bump() for _ in range(3))
    vals = list(g)
    return vals, x


print("nonlocal via class method from genexpr", nonlocal_through_class())


def cells_shared():
    fs = []
    for q in range(3):
        def make(q=q):
            acc = []
            def push(v):
                acc.append((q, v))
                return list(acc)
            return push
        fs.append(make())
    return fs


ps = cells_shared()
print("independent cells", ps[0]("a"), ps[0]("b"), ps[2]("c"))


def gen_with_closures():
    hooks = []
    for n in range(3):
        hooks.append(lambda: n)
        yield [h() for h in hooks]


print("closures observed mid-generator", list(gen_with_closures()))

# class body name not visible to nested function
class Outer:
    secret = 42

    def get(self):
        try:
            return secret
        except NameError as e:
            return type(e).__name__


print("method cannot see class name", Outer().get())

# __class__ cell in closures created inside methods
class WithSuper:
    def make(self):
        return [lambda: __class__.__name__ for _ in range(2)]


print("__class__ cell", [f() for f in WithSuper().make()])
