r'''
schema_version = 1
name = "Queue enqueue and dequeue"
source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/python-deque/custom_queue.py"
source_sha256 = "df5b847978a1f19177da61099d082cf590d035f384ca0777e1b612aff44f22f2"
expectation_source = "https://github.com/realpython/materials/blob/518d392755b2c993fd1e7362274dbd3d9e0f3876/python-deque/custom_queue.py"
license = "MIT"
license_file = "licenses/RealPython-MIT.txt"
adaptation = "Prepended TOML module docstring; upstream implementation unchanged."

[test]
stdout = "Enqueueing items with '.enqueue()'...\nQueue([1, 2, 3])\nDequeueing one item with '.dequeue()'...\n1\nQueue([2, 3])\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0

[mule]
status = "pending"
phase = "compile"
reason = "only absolute compiler imports from mule are supported"
'''

from collections import deque


class Queue:
    def __init__(self):
        self._items = deque()

    def enqueue(self, item):
        self._items.append(item)

    def dequeue(self):
        try:
            return self._items.popleft()
        except IndexError:
            # Break exception chain to hide implementation details
            raise IndexError("dequeue from an empty queue") from None

    def __len__(self):
        return len(self._items)

    def __contains__(self, item):
        return item in self._items

    def __iter__(self):
        yield from self._items

    def __reversed__(self):
        yield from reversed(self._items)

    def __repr__(self):
        return f"Queue({list(self._items)})"


if __name__ == "__main__":
    queue = Queue()
    print("Enqueueing items with '.enqueue()'...")
    queue.enqueue(1)
    queue.enqueue(2)
    queue.enqueue(3)
    print(queue)
    print("Dequeueing one item with '.dequeue()'...")
    print(queue.dequeue())
    print(queue)
