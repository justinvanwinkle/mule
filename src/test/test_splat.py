

def f(a, *args):
    x = a
    for arg in args:
        x += arg

    return x

ASSERT(f(1, 1, 2) == 4)
ASSERT(f(1) == 1)
