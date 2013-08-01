
a = 1


class A:
    __slots__ = ('a', 'b')

    def __init__(self, a):
        self.a = a

    def meth(self):
        return self.a + 1

a = A(1)

assert(a.a == 1)
assert(a.meth() == 2)
