x = 1


def test(y):
    x = 2
    return x + y


def test2(x):
    x = 3
    return x


def test3():
    y = 10
    for x in range(100):
        y = x

    return y
