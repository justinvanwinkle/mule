def test(x):
    return x


def test2(x, y):
    z = x + y

    return z


def test3(x):
    test(1)
    test2(5, 7)
    return 55

assert(test(5) == 5)
assert(test2(1, 2) == 3)
