

class Test:
    pass


class SubTest(Test, object):
    attr = 44

    def foo(self):
        x = 5
        print(x)


def not_method():
    pass
