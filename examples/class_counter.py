"""Exercise native method calls with annotated numeric arguments and loop state."""

import mule_types


@mule_types.optimize(speed=3, safety=0)
class Counter:
    value: mule_types.int64

    def __init__(self):
        self.value = 0

    def add(self, amount: mule_types.int64):
        self.value += amount


@mule_types.optimize(speed=3, safety=3)
def count(limit: mule_types.int64) -> mule_types.int64:
    counter = Counter()
    index: mule_types.int64 = 0
    while index < limit:
        counter.add(3)
        index += 1
    return counter.value


print(count(200_000_000))
