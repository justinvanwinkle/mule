"""UTF-8 / ASCII / Latin-1 encoding, decoding, and mixed-width Unicode text pipelines.

Exercises bytes.decode, str.encode, mixed Latin-1 and wide Unicode codepoint
iteration, slicing, case conversion, and multi-line text splitting.
"""

import sys
import time

DEFAULT_SIZE = 1200
DEFAULT_LOOPS = 1

_ASCII_LINES = (
    "Content-Disposition: form-data; name=\"payload\"; filename=\"report_2026_10.txt\"",
    "X-Trace-Context: 4bf92f3577b34da6a3ce929d0e0e4736/00f067aa0ba902b7;o=1",
    "Cache-Key: tenant:8841:region:us-central1:resource:invoice_summary:v4",
    "Authorization: Bearer eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.payload.signature",
)

_LATIN1_TEXTS = (
    "Café résumé naïve façade coopération Zürich Straße München Ångström",
    "Crème brûlée, jalapeño, piñata, señor, mañana, São Paulo, Noël",
    "Österreichische Nationalbank — Prüfungsbericht für Geschäftsführung",
)

_UNICODE_MIXED_TEXTS = (
    "日本語テキスト処理とコンパイラ最適化: λ-calculus → SSA → LLVM IR",
    "Ελληνικά αβγδεζηθ • Кириллица: проверка производительности строк",
    "Status: ✅ Passed | Latency: 12.4µs | Delta: ±0.3% | Rocket: 🚀🔥",
    "Mixed ASCII + Café + 東京 + Δx/Δt = 42.0 — end-of-record",
)


def prepare(size: int) -> tuple[list[bytes], list[bytes], list[str]]:
    ascii_blobs: list[bytes] = []
    utf8_blobs: list[bytes] = []
    mixed_strings: list[str] = []

    state = 314159265
    for i in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        a1 = _ASCII_LINES[state % len(_ASCII_LINES)]
        a2 = _ASCII_LINES[(state >> 3) % len(_ASCII_LINES)]
        ascii_doc = f"{a1}\r\n{a2}\r\nX-Sequence: {i}\r\n"
        ascii_blobs.append(ascii_doc.encode("utf-8"))

        l1 = _LATIN1_TEXTS[(state >> 6) % len(_LATIN1_TEXTS)]
        u1 = _UNICODE_MIXED_TEXTS[(state >> 10) % len(_UNICODE_MIXED_TEXTS)]
        combined = f"[{i}] {l1} | {u1}\n{a1}\n"
        utf8_blobs.append(combined.encode("utf-8"))
        mixed_strings.append(combined)

    return ascii_blobs, utf8_blobs, mixed_strings


def scenario_ascii_wire_decode_encode(ascii_blobs: list[bytes]) -> tuple[int, int, int]:
    """Decode ASCII/UTF-8 protocol frames, parse headers, and re-encode responses."""
    decoded_chars = 0
    header_count = 0
    encoded_bytes = 0

    for _ in range(11500):
        for raw in ascii_blobs:
            text = raw.decode("utf-8")
            decoded_chars += len(text)
            out_lines: list[str] = []
            for line in text.splitlines():
                key, sep, val = line.partition(": ")
                if sep:
                    header_count += 1
                    out_lines.append(key.lower() + "=" + val.strip('"'))
            reply = ";".join(out_lines).encode("utf-8")
            encoded_bytes += len(reply)

    return decoded_chars, header_count, encoded_bytes


def scenario_utf8_multilingual_codec(utf8_blobs: list[bytes]) -> tuple[int, int, int]:
    """Decode multi-byte UTF-8 payloads, inspect codepoint ranges, and re-encode."""
    total_codepoints = 0
    non_ascii_count = 0
    reencoded_len = 0

    for _ in range(2100):
        for raw in utf8_blobs:
            text = raw.decode("utf-8")
            total_codepoints += len(text)
            lines = text.splitlines()
            if lines:
                first = lines[0]
                for ch in first:
                    code = ord(ch)
                    if code >= 0x80:
                        non_ascii_count += 1
                        if code >= 0x800:
                            non_ascii_count += 1
                # Slice and re-encode normalized summary
                bar_pos = first.find(" | ")
                if bar_pos != -1:
                    prefix = first[:bar_pos].strip()
                    suffix = first[bar_pos + 3 :].strip()
                    rebuilt = (suffix + " // " + prefix).encode("utf-8")
                    reencoded_len += len(rebuilt)

    return total_codepoints, non_ascii_count, reencoded_len


def scenario_mixed_width_operations(mixed_strings: list[str]) -> tuple[int, int, int]:
    """Exercise slicing, searching, and case conversion across Latin-1 & wide strings."""
    word_count = 0
    latin1_accents = 0
    slice_checksum = 0

    for _ in range(1620):
        for s in mixed_strings:
            words = s.split()
            word_count += len(words)
            for w in words:
                cleaned = w.strip("[]():,|•—")
                if not cleaned:
                    continue
                if cleaned.isascii():
                    if cleaned.isalpha():
                        slice_checksum += len(cleaned.lower())
                else:
                    # Inspect mixed Latin-1 / wide token
                    for ch in cleaned:
                        cp = ord(ch)
                        if 0x80 <= cp <= 0xFF:
                            latin1_accents += 1
                        elif cp > 0xFF:
                            slice_checksum += cp & 0xFF
                    if len(cleaned) >= 3:
                        sub = cleaned[1:-1]
                        slice_checksum += len(sub)

    return word_count, latin1_accents, slice_checksum


def run_all(
    data: tuple[list[bytes], list[bytes], list[str]],
) -> tuple[int, int, int, int, int, int, int, int, int]:
    ascii_blobs, utf8_blobs, mixed_strings = data
    r1 = scenario_ascii_wire_decode_encode(ascii_blobs)
    r2 = scenario_utf8_multilingual_codec(utf8_blobs)
    r3 = scenario_mixed_width_operations(mixed_strings)
    return r1 + r2 + r3


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
    data = prepare(size)
    prep_s = time.perf_counter() - t0

    expected = None
    ascii_s = 0.0
    utf8_s = 0.0
    mixed_s = 0.0
    ascii_blobs, utf8_blobs, mixed_strings = data
    for _ in range(loops):
        r1 = ()
        r2 = ()
        r3 = ()
        if scenario in ("all", "ascii_codec"):
            s0 = time.perf_counter()
            r1 = scenario_ascii_wire_decode_encode(ascii_blobs)
            ascii_s += time.perf_counter() - s0
        if scenario in ("all", "utf8_codec"):
            s1 = time.perf_counter()
            r2 = scenario_utf8_multilingual_codec(utf8_blobs)
            utf8_s += time.perf_counter() - s1
        if scenario in ("all", "mixed_unicode"):
            s2 = time.perf_counter()
            r3 = scenario_mixed_width_operations(mixed_strings)
            mixed_s += time.perf_counter() - s2
        combined = r1 + r2 + r3
        if expected is not None and combined != expected:
            raise AssertionError("Result mismatch across loops")
        expected = combined

    total_s = (ascii_s + utf8_s + mixed_s) / loops
    print(f"topic\tstring/codecs_and_unicode")
    print(f"size\t{size}")
    print(f"loops\t{loops}")
    print(f"result\t{expected}")
    print(f"prepare\t{prep_s:.6f}")
    if scenario in ("all", "ascii_codec"):
        print(f"ascii_codec\t{ascii_s / loops:.6f}")
    if scenario in ("all", "utf8_codec"):
        print(f"utf8_codec\t{utf8_s / loops:.6f}")
    if scenario in ("all", "mixed_unicode"):
        print(f"mixed_unicode\t{mixed_s / loops:.6f}")
    print(f"total\t{total_s:.6f}")


if __name__ == "__main__":
    main()
