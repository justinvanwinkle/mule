import sys


class Counter:
    def __init__(self):
        self.value = 0

    def increment(self):
        self.value += 1


def run(count):
    counter = Counter()
    for index in range(count):
        counter.increment()
    return counter.value


print(run(int(sys.argv[1])))
