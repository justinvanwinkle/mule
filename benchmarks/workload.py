"""Scalar integer/search workload shared by Mule and CPython.

Read board size, search repetitions, and integer iterations from stdin (one per
line). Print the N-Queens solution total and a deterministic integer checksum.
Use only syntax supported by the initial native Mule compiler.
"""


def queens(mask, columns, left, right):
    if columns == mask:
        return 1
    available = mask & ~(columns | left | right)
    total = 0
    while available:
        bit = available & -available
        available ^= bit
        total += queens(
            mask, columns | bit, ((left | bit) << 1) & mask, (right | bit) >> 1
        )
    return total


def gcd(a, b):
    while b:
        remainder = a % b
        a = b
        b = remainder
    return a


def is_prime(n):
    if n < 2:
        return False
    if n % 2 == 0:
        return n == 2
    if n % 3 == 0:
        return n == 3
    divisor = 5
    while divisor * divisor <= n:
        if n % divisor == 0 or n % (divisor + 2) == 0:
            return False
        divisor += 6
    return True


def integer_kernel(iterations):
    state = 123456789
    checksum = 0
    index = 0
    while index < iterations:
        state = (state * 1664525 + 1013904223) & 2147483647
        candidate = 10000 + state % 90000
        common = gcd(candidate, (state >> 8) % 10000 + 1)
        if is_prime(candidate):
            checksum = (checksum * 33 + candidate + common) % 1000000007
        else:
            checksum = (checksum * 33 + (candidate ^ common)) % 1000000007
        index += 1
    return checksum


def main():
    n = int(input())
    repetitions = int(input())
    iterations = int(input())
    if n < 1 or n > 15 or repetitions < 0 or iterations < 0:
        print("invalid parameters")
        return
    mask = (1 << n) - 1
    solutions = 0
    repeat = 0
    while repeat < repetitions:
        solutions += queens(mask, 0, 0, 0)
        repeat += 1
    print(solutions, integer_kernel(iterations))


main()
