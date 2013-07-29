

class Class1:
    def meth(self):
        pass


class AttrTest(Class1):
    def __init__(self, x, y):
        self.x = 5
        self.y = 6

    def foo(self):
        self.x = 5
        return self.x


def not_method():
    return 1


at = AttrTest()
assert(at.foo() == 5)
