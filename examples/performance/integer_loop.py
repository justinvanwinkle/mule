import sys


def run(count):
    total = 0
    for index in range(count):
        total += index & 255
    return total


print(run(int(sys.argv[1])))
