"""Large string splitting, heterogeneous template formatting, and large file reads.

Covers straightforward bulk operations (reading a multi-megabyte text file,
splitting a multi-megabyte string) alongside heterogeneous formatting across
int, bigint (>64-bit), float, bool, None, str, and custom __str__/__format__ objects.
"""

import os
import sys
import tempfile
import time

DEFAULT_SIZE = 1000
DEFAULT_LOOPS = 1

_LOG_EVENTS = (
    "2026-10-29T19:00:01.104Z,service=checkout,region=us-east-1,status=200,latency_ms=18.42,bytes=4096,msg=Order completed successfully",
    "2026-10-29T19:00:02.219Z,service=inventory,region=eu-west-1,status=409,latency_ms=64.10,bytes=512,msg=Stock reservation conflict on sku-99182",
    "2026-10-29T19:00:03.008Z,service=payments,region=ap-northeast-1,status=502,latency_ms=310.75,bytes=128,msg=Upstream gateway timeout during settlement",
    "2026-10-29T19:00:04.512Z,service=search,region=us-west-2,status=200,latency_ms=7.05,bytes=16384,msg=Query matched 142 documents in shard-04",
    "2026-10-29T19:00:05.881Z,service=auth,region=us-east-1,status=401,latency_ms=3.90,bytes=256,msg=Expired bearer token presented by client",
)


class PriceTag:
    def __init__(self, cents: int, currency: str) -> None:
        self.cents = cents
        self.currency = currency

    def __str__(self) -> str:
        whole = self.cents // 100
        frac = self.cents % 100
        return f"{self.currency} {whole}.{frac:02d}"

    def __format__(self, spec: str) -> str:
        base = str(self)
        if spec == "upper":
            return base.upper()
        if spec.startswith(">") and spec[1:].isdigit():
            return base.rjust(int(spec[1:]))
        return base


class AuditBadge:
    def __init__(self, label: str, trace_id: int, flagged: bool) -> None:
        self.label = label
        self.trace_id = trace_id
        self.flagged = flagged

    def __str__(self) -> str:
        flag_str = "ALERT" if self.flagged else "OK"
        return f"[{self.label}:{flag_str}:{self.trace_id:x}]"


def prepare(size: int) -> tuple[str, list[dict[str, object]], str]:
    lines: list[str] = []
    records: list[dict[str, object]] = []
    state = 135792468
    # Build a multi-megabyte string (~3.5 MB at size=1000)
    for i in range(size * 25):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        ev = _LOG_EVENTS[state % len(_LOG_EVENTS)]
        lines.append(f"seq={i},{ev},trace={(1 << 72) + (state << 8) + i}")
    large_blob = "\n".join(lines) + "\n"

    for i in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        big_trace = (1 << 80) ^ (state << 24) ^ i
        cents = (state % 500000) + 99
        currency = "usd" if (i & 1) == 0 else "eur"
        ratio = ((state % 10000) + 1) / 137.0
        note = None if (i % 5) == 0 else f"batch-{i & 63}-verified"
        records.append(
            {
                "seq": i,
                "trace_id": big_trace,
                "price": PriceTag(cents, currency),
                "badge": AuditBadge("tx", big_trace, (state & 4) != 0),
                "ratio": ratio,
                "active": (state & 2) == 0,
                "note": note,
            }
        )

    fd, temp_path = tempfile.mkstemp(prefix="mule_topic_str_", suffix=".log")
    with os.fdopen(fd, "w", encoding="utf-8") as f:
        f.write(large_blob)

    return large_blob, records, temp_path


def scenario_large_string_split(large_blob: str) -> tuple[int, int, int, int]:
    """Split, rsplit, search, and replace across a single multi-megabyte string."""
    line_count = 0
    token_count = 0
    tail_bytes = 0
    replaced_len = 0

    for _ in range(485):
        # 1. Full splitlines() on multi-MB string
        lines = large_blob.splitlines()
        line_count += len(lines)

        # 2. rsplit with maxsplit on sampled lines + substring search
        stride = max(1, len(lines) // 500)
        idx = 0
        while idx < len(lines):
            ln = lines[idx]
            parts = ln.rsplit(",", 3)
            if len(parts) == 4:
                tail_bytes += len(parts[1]) + len(parts[2]) + len(parts[3])
            pos = ln.find("latency_ms=")
            if pos != -1:
                comma = ln.find(",", pos + 11)
                if comma != -1:
                    tail_bytes += comma - pos
            idx += stride

        # 3. Whitespace & delimiter split + bulk replace on 256 KB slice
        window = large_blob[:262144]
        words = window.split(",")
        token_count += len(words)
        rewritten = window.replace("service=", "svc=").replace("status=", "code=")
        replaced_len += len(rewritten)

    return line_count, token_count, tail_bytes, replaced_len


def scenario_mixed_template_format(records: list[dict[str, object]]) -> tuple[int, int, int]:
    """Format heterogeneous types (bigint, float, None, bool, custom objects)."""
    total_chars = 0
    alert_count = 0
    checksum = 0

    template_str = (
        "ROW[{seq:05d}] trace={trace} price={price} "
        "ratio={ratio:.3f} active={active} note={note} badge={badge}"
    )

    for _ in range(2700):
        out_chunks: list[str] = []
        for rec in records:
            seq = int(rec["seq"])
            trace = rec["trace_id"]
            price = rec["price"]
            badge = rec["badge"]
            ratio = float(rec["ratio"])
            active = rec["active"]
            note = rec["note"]

            # Mix f-string with custom __format__, .format(**kwargs), and % formatting
            f_line = f"{seq:#x}|{price:upper}|{price:>14}|{ratio:08.4f}|{note!r}|{badge}"
            fmt_line = template_str.format(
                seq=seq,
                trace=trace,
                price=price,
                ratio=ratio,
                active=active,
                note=note if note is not None else "N/A",
                badge=badge,
            )
            pct_line = "summary(%s): seq=%d ratio=%.2f active=%s" % (
                price,
                seq,
                ratio,
                active,
            )

            combined = f_line + "\n" + fmt_line + "\n" + pct_line
            total_chars += len(combined)
            if "ALERT" in f_line:
                alert_count += 1
            out_chunks.append(f_line)
            if len(out_chunks) == 50:
                block = "\n".join(out_chunks)
                checksum += len(block) ^ ord(block[0])
                out_chunks.clear()

    return total_chars, alert_count, checksum


def scenario_large_file_read_and_scan(temp_path: str) -> tuple[int, int, int]:
    """Read a multi-megabyte UTF-8 text file from disk and scan its lines."""
    bytes_read = 0
    error_lines = 0
    trace_tail_sum = 0

    for _ in range(1600):
        with open(temp_path, "r", encoding="utf-8") as f:
            content = f.read()
        bytes_read += len(content)

        with open(temp_path, "r", encoding="utf-8") as f:
            for line in f:
                if "status=502" in line or "status=409" in line:
                    error_lines += 1
                    _, _sep, tail = line.rpartition("trace=")
                    if tail:
                        clean = tail.strip()
                        if len(clean) >= 4:
                            trace_tail_sum += ord(clean[-1])

    return bytes_read, error_lines, trace_tail_sum


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
    large_blob, records, temp_path = prepare(size)
    prep_s = time.perf_counter() - t0

    try:
        expected = None
        split_s = 0.0
        tmpl_s = 0.0
        file_s = 0.0
        for _ in range(loops):
            r1 = ()
            r2 = ()
            r3 = ()
            if scenario in ("all", "large_string_split"):
                s0 = time.perf_counter()
                r1 = scenario_large_string_split(large_blob)
                split_s += time.perf_counter() - s0
            if scenario in ("all", "mixed_template_format"):
                s1 = time.perf_counter()
                r2 = scenario_mixed_template_format(records)
                tmpl_s += time.perf_counter() - s1
            if scenario in ("all", "large_file_read_and_scan"):
                s2 = time.perf_counter()
                r3 = scenario_large_file_read_and_scan(temp_path)
                file_s += time.perf_counter() - s2
            combined = r1 + r2 + r3
            if expected is not None and combined != expected:
                raise AssertionError("Result mismatch across loops")
            expected = combined

        total_s = (split_s + tmpl_s + file_s) / loops
        print("topic\tstring/large_text_and_templates")
        print(f"size\t{size}")
        print(f"loops\t{loops}")
        print(f"result\t{expected}")
        print(f"prepare\t{prep_s:.6f}")
        if scenario in ("all", "large_string_split"):
            print(f"large_string_split\t{split_s / loops:.6f}")
        if scenario in ("all", "mixed_template_format"):
            print(f"mixed_template_format\t{tmpl_s / loops:.6f}")
        if scenario in ("all", "large_file_read_and_scan"):
            print(f"large_file_read_and_scan\t{file_s / loops:.6f}")
        print(f"total\t{total_s:.6f}")
    finally:
        try:
            os.remove(temp_path)
        except OSError:
            pass


if __name__ == "__main__":
    main()
