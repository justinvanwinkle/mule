"""Receiver type validation on explicit dict, set, and frozenset descriptor calls."""

for bad_call in (
    lambda: dict.__len__([]),
    lambda: dict.__getitem__([], 0),
    lambda: dict.__setitem__([], 0, 1),
    lambda: dict.get([], 0),
    lambda: set.__len__([]),
    lambda: set.add([], 1),
    lambda: frozenset.__len__([]),
):
    try:
        bad_call()
    except TypeError as e:
        print("receiver_error:", type(e).__name__)
