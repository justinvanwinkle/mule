import sys


def run(count):
    values = [1, 2, 3, 4, 5, 6, 7, 8]
    total = 0
    for index in range(count):
        total += values[index & 7]
    return total


print(run(int(sys.argv[1])))
