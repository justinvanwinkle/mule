"""Exercise native method calls and fixed instance fields without annotations."""


class Counter:
    def __init__(self):
        self.value = 0

    def add(self, amount):
        self.value += amount


def count(limit):
    counter = Counter()
    index = 0
    while index < limit:
        counter.add(3)
        index += 1
    return counter.value


print(count(200_000_000))
