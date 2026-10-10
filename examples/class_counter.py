"""Exercise native method calls with annotated numeric arguments and loop state."""

import mule


class Counter:
    def __init__(self):
        self.value = 0

    def add(self, amount: mule.int64):
        self.value += amount


def count(limit: mule.int64) -> mule.int64:
    counter = Counter()
    index: mule.int64 = 0
    while index < limit:
        counter.add(3)
        index += 1
    return counter.value


print(count(200_000_000))
