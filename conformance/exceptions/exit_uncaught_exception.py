"""Program that prints some lines then terminates with an uncaught ValueError (exit code 1)."""


def validate(values):
    for v in values:
        print("checking", v)
        if v < 0:
            raise ValueError(f"negative value: {v}")
    return True


print("start")
try:
    validate([1, 2])
finally:
    print("first validation finally")
print("first batch ok")
try:
    validate([3, -4, 5])
finally:
    print("second validation finally")
print("unreachable")
