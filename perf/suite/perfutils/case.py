"""The common CLI and timing protocol used by each standalone scenario."""

import sys
import time

VARIANTS = ("untyped", "partial", "typed")


def run_case(name, description, default_size, default_seed, prepare, kernels):
    """Prepare once, time calls, and consume every result outside the timer.

    Kernels must leave their input unchanged and return a deterministic tuple
    of integers. Warmup and measured calls all contribute to validation. No
    opaque calls or other optimization barriers are inserted inside a kernel.
    """
    options = {
        "--size": str(default_size),
        "--seed": str(default_seed),
        "--variant": "untyped",
        "--loops": "1",
        "--warmups": "1",
    }
    arguments = sys.argv[1:]
    if arguments == ["--help"]:
        print(name + ": " + description)
        print("--size N --seed N --variant untyped|partial|typed --loops N --warmups N")
        return
    if len(arguments) % 2:
        raise ValueError("options require a value; use --help for usage")
    index = 0
    while index < len(arguments):
        option = arguments[index]
        if option not in options:
            raise ValueError("unknown option: " + option)
        options[option] = arguments[index + 1]
        index += 2
    size = int(options["--size"])
    seed = int(options["--seed"])
    loops = int(options["--loops"])
    warmups = int(options["--warmups"])
    variant = options["--variant"]
    if size < 1 or loops < 1 or warmups < 0:
        raise ValueError("size and loops must be positive; warmups must be nonnegative")
    if variant not in VARIANTS:
        raise ValueError("unknown variant: " + variant)
    kernel = kernels[variant]
    started = time.perf_counter()
    data = prepare(size, seed)
    preparation = time.perf_counter() - started
    expected = None
    for _ in range(warmups):
        result = kernel(data)
        if expected is not None and result != expected:
            raise AssertionError("unstable warmup result")
        expected = result
    elapsed = 0.0
    for _ in range(loops):
        started = time.perf_counter()
        result = kernel(data)
        elapsed += time.perf_counter() - started
        if expected is not None and result != expected:
            raise AssertionError(
                "kernel changed its input or returned unstable results"
            )
        expected = result
    print("CASE\t" + name)
    print("SIZE\t" + str(size))
    print("SEED\t" + str(seed))
    print("VARIANT\t" + variant)
    print("LOOPS\t" + str(loops))
    print("RESULT\t" + repr(expected))
    print("PREPARE\t" + str(preparation))
    print("KERNEL\t" + str(elapsed / loops))
