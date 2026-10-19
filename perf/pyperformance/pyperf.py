"""Self-contained pyperf compatibility harness compiled by Mule and run by CPython.

Provides the ``Runner``, ``perf_counter``, and ``python_implementation`` APIs
used by unmodified ``pyperformance`` ``run_benchmark.py`` entry points without
pulling in external PyPI packages or heavy CLI dependencies.
"""

import sys
import time


def perf_counter() -> float:
    return time.perf_counter()


def python_implementation() -> str:
    return "cpython"


class _OptionSpec:
    def __init__(self, flags, dest, action, default, conv, nargs):
        self.flags = flags
        self.dest = dest
        self.action = action
        self.default = default
        self.conv = conv
        self.nargs = nargs


class Namespace:
    pass


class _ArgParser:
    def __init__(self):
        self._options = []
        self._positionals = []
        self.add_argument("-l", "--loops", type=int, default=0, dest="loops")
        self.add_argument("-w", "--warmups", type=int, default=1, dest="warmups")
        self.add_argument("-n", "--values", type=int, default=1, dest="values")
        self.add_argument("-p", "--processes", type=int, default=1, dest="processes")
        self.add_argument("--min-time", type=float, default=0.0, dest="min_time")
        self.add_argument("--worker", action="store_true", default=False, dest="worker")

    def add_argument(
        self,
        *names,
        action="store",
        default=None,
        type=None,
        choices=None,
        nargs=None,
        help=None,
        metavar=None,
        required=False,
        dest=None,
        version=None,
    ):
        flags = []
        for name in names:
            if name.startswith("-"):
                flags.append(name)
        if not flags:
            target = dest if dest is not None else names[0].replace("-", "_")
            if action == "store_true" and default is None:
                default = False
            elif action == "store_false" and default is None:
                default = True
            self._positionals.append(
                _OptionSpec((), target, action, default, type, nargs)
            )
            return
        if dest is None:
            long_names = [f for f in flags if f.startswith("--")]
            chosen = long_names[0] if long_names else flags[0]
            dest = chosen.lstrip("-").replace("-", "_")
        if action == "store_true" and default is None:
            default = False
        elif action == "store_false" and default is None:
            default = True
        self._options.append(
            _OptionSpec(tuple(flags), dest, action, default, type, nargs)
        )

    def parse_args(self, argv=None):
        if argv is None:
            argv = sys.argv[1:]
        ns = Namespace()
        for spec in self._options:
            setattr(ns, spec.dest, spec.default)
        for spec in self._positionals:
            setattr(ns, spec.dest, spec.default)
        pos_idx = 0
        i = 0
        n = len(argv)
        while i < n:
            arg = argv[i]
            if arg.startswith("-"):
                opt_name = arg
                inline_val = None
                if "=" in arg:
                    opt_name, _, inline_val = arg.partition("=")
                matched = None
                for spec in self._options:
                    if opt_name in spec.flags:
                        matched = spec
                        break
                if matched is not None:
                    if matched.action == "store_true":
                        setattr(ns, matched.dest, True)
                        i += 1
                    elif matched.action == "store_false":
                        setattr(ns, matched.dest, False)
                        i += 1
                    else:
                        if inline_val is not None:
                            raw = inline_val
                            i += 1
                        else:
                            raw = argv[i + 1]
                            i += 2
                        val = matched.conv(raw) if matched.conv is not None else raw
                        setattr(ns, matched.dest, val)
                else:
                    i += 1
            else:
                if pos_idx < len(self._positionals):
                    spec = self._positionals[pos_idx]
                    pos_idx += 1
                    val = spec.conv(arg) if spec.conv is not None else arg
                    setattr(ns, spec.dest, val)
                i += 1
        return ns


class Runner:
    def __init__(
        self,
        values=None,
        warmups=None,
        processes=None,
        loops=None,
        min_time=None,
        max_time=None,
        metadata=None,
        show_name=True,
        program_args=None,
        add_cmdline_args=None,
        _argparser=None,
    ):
        self.metadata = dict(metadata) if metadata is not None else {}
        self.argparser = _ArgParser()
        self.args = None
        self._default_loops = loops if loops is not None else 1
        self._default_warmups = warmups if warmups is not None else 1

    def parse_args(self, args=None):
        self.args = self.argparser.parse_args(args)
        if self.args.loops <= 0:
            self.args.loops = self._default_loops if self._default_loops > 0 else 1
        return self.args

    def _ensure_args(self):
        if self.args is None:
            self.parse_args()
        return self.args

    def bench_func(self, name, func, *args, **kwargs):
        ns = self._ensure_args()
        inner_loops = kwargs.pop("inner_loops", None)
        if inner_loops is None or inner_loops < 1:
            inner_loops = 1
        for _ in range(ns.warmups):
            func(*args, **kwargs)
        loops = ns.loops
        t0 = time.perf_counter()
        for _ in range(loops):
            func(*args, **kwargs)
        dt = time.perf_counter() - t0
        total_loops = loops * inner_loops
        per_loop = dt / total_loops
        print("BENCH\t" + str(name) + "\t" + str(total_loops) + "\t" + str(per_loop))
        return per_loop

    def bench_time_func(self, name, time_func, *args, **kwargs):
        ns = self._ensure_args()
        inner_loops = kwargs.pop("inner_loops", None)
        if inner_loops is None or inner_loops < 1:
            inner_loops = 1
        for _ in range(ns.warmups):
            time_func(1, *args, **kwargs)
        loops = ns.loops
        dt = time_func(loops, *args, **kwargs)
        total_loops = loops * inner_loops
        per_loop = dt / total_loops
        print("BENCH\t" + str(name) + "\t" + str(total_loops) + "\t" + str(per_loop))
        return per_loop

    def bench_async_func(self, name, func, *args, **kwargs):
        import asyncio

        ns = self._ensure_args()
        inner_loops = kwargs.pop("inner_loops", None)
        if inner_loops is None or inner_loops < 1:
            inner_loops = 1
        for _ in range(ns.warmups):
            asyncio.run(func(*args, **kwargs))
        loops = ns.loops
        t0 = time.perf_counter()
        for _ in range(loops):
            asyncio.run(func(*args, **kwargs))
        dt = time.perf_counter() - t0
        total_loops = loops * inner_loops
        per_loop = dt / total_loops
        print("BENCH\t" + str(name) + "\t" + str(total_loops) + "\t" + str(per_loop))
        return per_loop

    def bench_command(self, name, command):
        import subprocess

        ns = self._ensure_args()
        for _ in range(ns.warmups):
            subprocess.run(
                command,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
                check=True,
            )
        loops = ns.loops
        t0 = time.perf_counter()
        for _ in range(loops):
            subprocess.run(
                command,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
                check=True,
            )
        dt = time.perf_counter() - t0
        per_loop = dt / loops
        print("BENCH\t" + str(name) + "\t" + str(loops) + "\t" + str(per_loop))
        return per_loop
