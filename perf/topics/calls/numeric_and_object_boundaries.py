"""Numeric tower boundaries (fixnum/bigint/float/dunder) and heterogeneous callable dispatch.

Covers arithmetic crossing 62-bit fixnum boundaries into 128-bit and 256-bit
integers, mixed `int`/`float`/`bool` arithmetic, custom `FixedDecimal` and
`Rational` numeric dunders, and polymorphic callable dispatch (`__call__`,
bound methods, `functools.partial`, `getattr`, and plain functions).
"""

import functools
import math
import sys
import time

DEFAULT_SIZE = 200
DEFAULT_LOOPS = 1

_PRIME_127 = (1 << 127) - 1
_MASK_128 = (1 << 128) - 1
_MASK_256 = (1 << 256) - 1


class FixedDecimal:
    """Fixed-point decimal with 4 decimal places (scale = 10_000)."""

    def __init__(self, scaled: int) -> None:
        self.scaled = scaled

    def __add__(self, other: "FixedDecimal | int") -> "FixedDecimal":
        if isinstance(other, FixedDecimal):
            return FixedDecimal(self.scaled + other.scaled)
        return FixedDecimal(self.scaled + other * 10_000)

    def __sub__(self, other: "FixedDecimal | int") -> "FixedDecimal":
        if isinstance(other, FixedDecimal):
            return FixedDecimal(self.scaled - other.scaled)
        return FixedDecimal(self.scaled - other * 10_000)

    def __mul__(self, other: "FixedDecimal | int") -> "FixedDecimal":
        if isinstance(other, FixedDecimal):
            return FixedDecimal((self.scaled * other.scaled) // 10_000)
        return FixedDecimal(self.scaled * other)

    def __truediv__(self, other: "FixedDecimal | int") -> "FixedDecimal":
        if isinstance(other, FixedDecimal):
            denom = other.scaled if other.scaled != 0 else 10_000
            return FixedDecimal((self.scaled * 10_000) // denom)
        denom_i = other if other != 0 else 1
        return FixedDecimal(self.scaled // denom_i)

    def __lt__(self, other: "FixedDecimal") -> bool:
        return self.scaled < other.scaled


class Rational:
    """Small exact rational number reduced by gcd."""

    def __init__(self, num: int, den: int) -> None:
        if den < 0:
            num = -num
            den = -den
        g = math.gcd(num, den)
        self.num = num // g
        self.den = (den // g) if g != 0 else 1

    def __add__(self, other: "Rational") -> "Rational":
        return Rational(self.num * other.den + other.num * self.den, self.den * other.den)

    def __sub__(self, other: "Rational") -> "Rational":
        return Rational(self.num * other.den - other.num * self.den, self.den * other.den)

    def __mul__(self, other: "Rational") -> "Rational":
        return Rational(self.num * other.num, self.den * other.den)

    def __truediv__(self, other: "Rational") -> "Rational":
        onum = other.num if other.num != 0 else 1
        return Rational(self.num * other.den, self.den * onum)

    def __lt__(self, other: "Rational") -> bool:
        return self.num * other.den < other.num * self.den


def _plain_linear_step(a: int, b: int, scale: int, bias: int) -> int:
    return (a * scale + b - bias) ^ (scale << 2)


class StatefulAccumulator:
    def __init__(self, seed: int) -> None:
        self.state = seed

    def apply_delta(self, x: int, y: int, weight: int) -> int:
        self.state = (self.state + x * weight - y) & 0x7FFFFFFF
        return self.state

    def combine_pair(self, x: int, y: int, weight: int) -> int:
        self.state = (self.state ^ ((x + y) * weight)) & 0x7FFFFFFF
        return self.state + x

    def rotate_mix(self, x: int, y: int, weight: int) -> int:
        rotated = ((x << 3) | (y & 7)) + weight
        self.state = (self.state + rotated) & 0x7FFFFFFF
        return self.state


class PolynomialCallable:
    def __init__(self, c2: int, c1: int, c0: int) -> None:
        self.c2 = c2
        self.c1 = c1
        self.c0 = c0

    def __call__(self, x: int, y: int, weight: int) -> int:
        return (self.c2 * x * x + self.c1 * y + self.c0) * weight


def prepare(
    size: int,
) -> tuple[
    list[tuple[int, int, float, FixedDecimal, Rational]],
    list[tuple[int, int, int, str]],
]:
    numeric_rows: list[tuple[int, int, float, FixedDecimal, Rational]] = []
    call_inputs: list[tuple[int, int, int, str]] = []
    state = 314159265
    method_names = ("apply_delta", "combine_pair", "rotate_mix")

    for i in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        small_int = ((state >> 3) % 4000) + 17
        big_seed = ((state << 65) | (small_int << 33) | (i + 99)) & _MASK_128
        flt = (small_int * 0.125) + ((state & 0xFF) * 0.03125)
        fdec = FixedDecimal(small_int * 250 + (state % 9000))
        rat = Rational((small_int % 29) + 1, ((state >> 9) % 19) + 2)
        numeric_rows.append((small_int, big_seed, flt, fdec, rat))

        x = (state >> 5) % 300 - 50
        y = (state >> 12) % 300 - 50
        w = 1 + ((state >> 19) % 11)
        mname = method_names[i % len(method_names)]
        call_inputs.append((x, y, w, mname))

    return numeric_rows, call_inputs


def scenario_bigint_and_float_tower(
    numeric_rows: list[tuple[int, int, float, FixedDecimal, Rational]],
) -> tuple[int, int, int, int]:
    """Cross 62-bit fixnum boundaries into 128/256-bit ints, floats, and numeric dunders."""
    bigint_digest = 0
    float_mix_sum = 0
    decimal_total = 0
    rational_total = 0

    unit_dec = FixedDecimal(12_500)
    step_rat = Rational(3, 7)

    for round_idx in range(21800):
        shift_amt = 60 + (round_idx & 7)
        for small, big128, flt, fdec, rat in numeric_rows:
            # Cross from 62-bit fixnum into 128-bit and 256-bit bigint arithmetic
            promoted = (small << shift_amt) + big128
            wide256 = (promoted * ((small << 64) | 0x5A5A5A5A3C3C3C3C)) & _MASK_256
            quot, rem = divmod(wide256, big128 | 1)
            mod_exp = pow(promoted & 0xFFFFFFFFFFFFFFFFFFFF, 3, _PRIME_127)
            folded = ((quot ^ rem ^ mod_exp) >> 48) & 0x3FFFFFFF
            bigint_digest = (bigint_digest + folded) & 0x7FFFFFFF

            # Mixed int / bool / float arithmetic across numeric tower
            is_large = rem > (big128 >> 1)
            mixed_f = (flt * (small + is_large)) / (1.5 + (round_idx & 3) * 0.25) + (folded & 0xFF) * 0.5
            float_mix_sum += int(mixed_f)

            # Custom FixedDecimal dunder arithmetic (__add__, __sub__, __mul__, __truediv__, __lt__)
            d_acc = (fdec + unit_dec) * ((small & 7) + 1) - (small & 3)
            d_div = d_acc / unit_dec
            if d_div < fdec:
                decimal_total += d_div.scaled
            else:
                decimal_total += d_acc.scaled // 100

            # Custom Rational dunder arithmetic
            r_next = (rat + step_rat) * Rational(2, 3) - Rational(1, 5)
            r_div = r_next / step_rat
            if r_div < rat:
                rational_total += r_div.num + r_div.den
            else:
                rational_total += r_next.num * 2 + r_next.den

    return bigint_digest, float_mix_sum, decimal_total, rational_total


def scenario_dynamic_callable_objects(
    call_inputs: list[tuple[int, int, int, str]],
) -> tuple[int, int, int, int]:
    """Invoke heterogeneous callables: functions, bound methods, `__call__`, `partial`, `getattr`."""
    table_sum = 0
    getattr_sum = 0
    partial_sum = 0
    acc_state_sum = 0

    acc_a = StatefulAccumulator(101)
    acc_b = StatefulAccumulator(202)
    poly_1 = PolynomialCallable(2, -3, 11)
    poly_2 = PolynomialCallable(1, 5, -7)
    part_fast = functools.partial(_plain_linear_step, scale=5, bias=19)
    part_alt = functools.partial(_plain_linear_step, scale=9, bias=-7)

    callable_table = (
        acc_a.apply_delta,
        poly_1,
        acc_b.combine_pair,
        poly_2,
        acc_a.rotate_mix,
        lambda x, y, w: _plain_linear_step(x + w, y - w, 3, 5),
    )
    n_callables = len(callable_table)

    for round_idx in range(65000):
        for idx, (x, y, w, method_name) in enumerate(call_inputs):
            fn = callable_table[(idx + round_idx) % n_callables]
            table_sum += fn(x, y, w)

            # Dynamic getattr method lookup and invocation
            target_obj = acc_a if (idx & 1) == 0 else acc_b
            bound_m = getattr(target_obj, method_name)
            if len(args_pair := (x + (round_idx & 7), y)) == 2 and (idx & 3) == 0:
                getattr_sum += bound_m(*args_pair, w)
            else:
                getattr_sum += bound_m(x + (round_idx & 7), y, w)

            # functools.partial calls with mixed positional & keyword args
            if (idx & 1) == 0:
                partial_sum += part_fast(x, y + w)
            else:
                partial_sum += part_alt(x - w, y, bias=w * 3)

        acc_state_sum = (acc_state_sum + acc_a.state + acc_b.state) & 0x7FFFFFFF

    return table_sum, getattr_sum, partial_sum, acc_state_sum


def main() -> None:
    size = DEFAULT_SIZE
    loops = DEFAULT_LOOPS
    scenario = "all"
    args = sys.argv[1:]
    i = 0
    while i < len(args):
        if args[i] == "--size" and i + 1 < len(args):
            size = int(args[i + 1])
            i += 2
        elif args[i] == "--loops" and i + 1 < len(args):
            loops = int(args[i + 1])
            i += 2
        elif args[i] == "--scenario" and i + 1 < len(args):
            scenario = args[i + 1]
            i += 2
        else:
            raise ValueError(f"Unknown argument: {args[i]}")

    t0 = time.perf_counter()
    numeric_rows, call_inputs = prepare(size)
    prep_s = time.perf_counter() - t0

    expected = None
    num_s = 0.0
    call_s = 0.0
    for _ in range(loops):
        r1: tuple[int, ...] = ()
        r2: tuple[int, ...] = ()
        if scenario in ("all", "bigint_and_float_tower"):
            s0 = time.perf_counter()
            r1 = scenario_bigint_and_float_tower(numeric_rows)
            num_s += time.perf_counter() - s0
        if scenario in ("all", "dynamic_callable_objects"):
            s1 = time.perf_counter()
            r2 = scenario_dynamic_callable_objects(call_inputs)
            call_s += time.perf_counter() - s1
        combined = r1 + r2
        if expected is not None and combined != expected:
            raise AssertionError("Result mismatch across loops")
        expected = combined

    total_s = (num_s + call_s) / loops
    print("topic\tcalls/numeric_and_object_boundaries")
    print(f"size\t{size}")
    print(f"loops\t{loops}")
    print(f"result\t{expected}")
    print(f"prepare\t{prep_s:.6f}")
    if scenario in ("all", "bigint_and_float_tower"):
        print(f"bigint_and_float_tower\t{num_s / loops:.6f}")
    if scenario in ("all", "dynamic_callable_objects"):
        print(f"dynamic_callable_objects\t{call_s / loops:.6f}")
    print(f"total\t{total_s:.6f}")


if __name__ == "__main__":
    main()
