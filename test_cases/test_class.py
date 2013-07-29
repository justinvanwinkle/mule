
class Fuck:
    __slots__ = ('a', 'b')

    def __init__(self, a):
        self.a = a

f = Fuck(1)

assert(f.a == 1)
