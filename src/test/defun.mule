def test(x):
    return x + 1


def test2(x, y):
    z = x + y

    return z


def test3(x):
    test(1)
    test2(5, 7)
    return test(1) + 55


def test_outer(x):
    def test_inner(y):
        return y + 1
    return test_inner(x + 1)

ASSERT(test(5) == 6)
ASSERT(test2(1, 2) == 3)
ASSERT(test3(1) == 57)
ASSERT(test_outer(5) == 7)
