"""Scan and evaluate compact JSON-like metric objects without the json module.

Exercises character indexing, string slicing, literal comparisons, and integer
accumulation over structured text payloads.
"""

from perfutils import run_case

NAME = "json_tokenizer"
DESCRIPTION = "Handwritten scanner and evaluator for JSON-like metric payloads"
DEFAULT_SIZE = 50_000
DEFAULT_SEED = 42

_TAGS = ("cpu", "mem", "disk", "net", "io", "gpu")


def prepare(size: int, seed: int) -> list[str]:
    if size < 0:
        raise ValueError("size must be nonnegative")
    state = seed & 0x7FFFFFFF
    payloads: list[str] = []
    for _ in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        tag = _TAGS[(state >> 4) % len(_TAGS)]
        v1 = (state >> 8) % 500
        v2 = (state >> 15) % 500
        v3 = ((state >> 20) % 200) - 50
        active = "true" if (state & 8) else "false"
        payloads.append(
            '{"tag":"'
            + tag
            + '","vals":['
            + str(v1)
            + ","
            + str(v2)
            + ","
            + str(v3)
            + '],"ok":'
            + active
            + "}"
        )
    return payloads


def scan_payload(text: str) -> tuple[str, int, int]:
    n = len(text)
    i = 0
    tag = ""
    val_sum = 0
    active = 0
    while i < n:
        ch = text[i]
        if ch == '"':
            end = text.find('"', i + 1)
            word = text[i + 1 : end]
            i = end + 1
            if word == "tag":
                q1 = text.find('"', i)
                q2 = text.find('"', q1 + 1)
                tag = text[q1 + 1 : q2]
                i = q2 + 1
        elif ch == "[" or ch == ",":
            i += 1
            while i < n and text[i] == " ":
                i += 1
            if i < n and (text[i] == "-" or ("0" <= text[i] <= "9")):
                start = i
                i += 1
                while i < n and ("0" <= text[i] <= "9"):
                    i += 1
                val_sum += int(text[start:i])
        elif ch == "t" and text[i : i + 4] == "true":
            active = 1
            i += 4
        else:
            i += 1
    return tag, val_sum, active


def run(data: list[str]) -> tuple[int, int, int]:
    by_tag: dict[str, int] = {}
    rounds = max(1, len(data) // 1000)
    active_count = 0
    total_sum = 0
    for round_idx in range(rounds):
        weight = (round_idx & 3) + 1
        for payload in data:
            tag, val_sum, active = scan_payload(payload)
            by_tag[tag] = by_tag.get(tag, 0) + val_sum * weight
            active_count += active
            total_sum += val_sum
    tag_checksum = 0
    for tag, total in by_tag.items():
        tag_checksum = (tag_checksum + len(tag) * total) & 0x3FFFFFFF
    return total_sum, active_count, tag_checksum


def main():
    run_case(NAME, DESCRIPTION, DEFAULT_SIZE, DEFAULT_SEED, prepare, run)


if __name__ == "__main__":
    main()
