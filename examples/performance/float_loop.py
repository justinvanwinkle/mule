import sys


def run(count):
    total = 0.0
    value = 0.5
    for index in range(count):
        total += value * 0.125
        value += 0.5
    return total


print(run(int(sys.argv[1])))
