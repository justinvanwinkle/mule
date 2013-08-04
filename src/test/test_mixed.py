

class CounterX:
    __slots__ = ('count', )

    def __init__(self):
        self.count = 0

    def incr(self):
        self.count = self.count + 1


def foo():
    c = CounterX()
    while c.count < 10000000:
        c.incr()
    return c.count

x = foo()

print x
