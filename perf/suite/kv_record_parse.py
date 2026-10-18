"""Parse pipe-delimited key-value records and format normalized summaries.

Exercises str.split, str.partition, str.startswith, string literal comparisons,
and string concatenation.
"""

from perfutils import run_case

NAME = "kv_record_parse"
DESCRIPTION = "Delimited key-value string splitting, partitioning, and formatting"
DEFAULT_SIZE = 50_000
DEFAULT_SEED = 42

_RECORDS = (
    "svc=auth|region=us-east|level=info|ms=12|code=200",
    "svc=billing|region=eu-west|level=warn|ms=85|code=429",
    "svc=search|region=ap-south|level=error|ms=240|code=503",
    "svc=cache|region=us-west|level=info|ms=4|code=200",
    "svc=gateway|region=us-east|level=warn|ms=63|code=502",
    "svc=storage|region=eu-central|level=info|ms=19|code=200",
)


def prepare(size: int, seed: int) -> list[str]:
    if size < 0:
        raise ValueError("size must be nonnegative")
    state = seed & 0x7FFFFFFF
    records: list[str] = []
    for i in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        base = _RECORDS[(state >> 8) % len(_RECORDS)]
        seq = str((state >> 16) % 1000 + (i & 15))
        records.append(base + "|seq=" + seq)
    return records


def run(data: list[str]) -> tuple[int, int, int]:
    rounds = max(1, len(data) // 400)
    total_ms = 0
    alerts = 0
    summary_bytes = 0
    for round_idx in range(rounds):
        mask = (round_idx & 7) + 1
        for rec in data:
            parts = rec.split("|")
            svc = ""
            level = ""
            ms = 0
            seq = 0
            for kv in parts:
                key, _sep, val = kv.partition("=")
                if key == "svc":
                    svc = val
                elif key == "level":
                    level = val
                elif key == "ms":
                    ms = int(val)
                elif key == "seq":
                    seq = int(val)
            if level.startswith(("w", "e")):
                alerts += 1
            total_ms += ms + (seq & mask)
            tag = svc + ":" + level
            summary_bytes += len(tag)
    return total_ms, alerts, summary_bytes


def main():
    run_case(NAME, DESCRIPTION, DEFAULT_SIZE, DEFAULT_SEED, prepare, run)


if __name__ == "__main__":
    main()
