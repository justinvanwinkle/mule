import sys


def increment(value):
    return value + 1


def run(count):
    value = 0
    for index in range(count):
        value = increment(value)
    return value


print(run(int(sys.argv[1])))
