"""File and in-memory stream workloads covering buffered file reading, line
iteration, chunked seeking/reading, StringIO/BytesIO multiplexing, bytearray
in-place mutation, and memoryview slicing.
"""

import atexit
import io
import os
import shutil
import sys
import tempfile
import time

DEFAULT_SIZE = 2000
DEFAULT_LOOPS = 1

_TEMP_DIRS: list[str] = []


def _cleanup_temp_dirs() -> None:
    while _TEMP_DIRS:
        d = _TEMP_DIRS.pop()
        shutil.rmtree(d, ignore_errors=True)


atexit.register(_cleanup_temp_dirs)

_LOG_EVENTS = (
    "GET /api/v2/accounts/summary HTTP/1.1|status=200|bytes=4812|ms=14|node=web-01|msg=ok-utf8-αβγ",
    "POST /api/v2/payments/settle HTTP/1.1|status=201|bytes=1024|ms=89|node=pay-03|msg=settled-€100",
    "GET /static/bundles/vendor.js HTTP/1.1|status=304|bytes=0|ms=2|node=cdn-02|msg=cached-hit",
    "PUT /api/v2/inventory/item/9941 HTTP/1.1|status=500|bytes=512|ms=312|node=inv-01|msg=deadlock-retry-Δ",
    "DELETE /api/v2/sessions/expired HTTP/1.1|status=204|bytes=0|ms=7|node=auth-02|msg=purged",
    "POST /graphql/v1/batch HTTP/1.1|status=429|bytes=256|ms=4|node=gw-04|msg=rate-limit-exceeded",
)

_TEXT_CHUNKS = (
    "alpha_metric=1049;beta_flag=true;region=us-east-1;comment=stream_segment_one\n",
    "alpha_metric=2091;beta_flag=false;region=eu-west-1;comment=stream_segment_two\n",
    "alpha_metric=3190;beta_flag=true;region=ap-northeast-1;comment=stream_segment_three\n",
    "alpha_metric=4012;beta_flag=false;region=us-west-2;comment=stream_segment_four\n",
)


def prepare(size: int) -> tuple[str, list[str], list[bytes]]:
    temp_dir = tempfile.mkdtemp(prefix="mule_perf_io_")
    _TEMP_DIRS.append(temp_dir)
    log_path = os.path.join(temp_dir, "application_stream.log")

    text_chunks: list[str] = []
    byte_chunks: list[bytes] = []
    state = 29018471

    # Scale lines so default size produces a multi-megabyte UTF-8 file while size=5 stays tiny.
    lines_per_unit = 18
    with open(log_path, "w", encoding="utf-8") as f:
        for i in range(size * lines_per_unit):
            state = (state * 1103515245 + 12345) & 0x7FFFFFFF
            ev = _LOG_EVENTS[state % len(_LOG_EVENTS)]
            sec = (i // 100) % 60
            ms = state % 1000
            offset_hint = (i * 131) & 0xFFFFF
            line = (
                f"2026-10-30T12:{(i // 6000) % 60:02d}:{sec:02d}.{ms:03d}Z "
                f"seq={i} off={offset_hint} {ev}\n"
            )
            f.write(line)

    for i in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        t_chunk = _TEXT_CHUNKS[state % len(_TEXT_CHUNKS)] + f"seq_id={i};checksum={(state >> 8) & 0xFFFF}\n"
        text_chunks.append(t_chunk)
        b_header = bytes(
            [
                (i & 0xFF),
                ((state >> 4) & 0xFF),
                ((state >> 12) & 0xFF),
                ((state >> 20) & 0xFF),
            ]
        )
        byte_chunks.append(b_header + t_chunk.encode("utf-8"))

    return log_path, text_chunks, byte_chunks


def scenario_large_file_streaming(log_path: str) -> tuple[int, int, int, int]:
    """Read a UTF-8 log file via full read, line iteration, and chunked read + readline."""
    total_chars = 0
    status_accum = 0
    byte_offset_sum = 0
    chunk_transitions = 0

    for _ in range(390):
        # 1. Full file read and newline/timestamp inspection
        with open(log_path, "r", encoding="utf-8") as f:
            full_text = f.read()
            total_chars += len(full_text)

        # 2. Line-by-line streaming iteration
        with open(log_path, "r", encoding="utf-8") as f:
            for line in f:
                # Parse timestamp seconds + milliseconds: "2026-10-30T12:MM:SS.mmmZ"
                if len(line) > 24 and line[19] == ".":
                    sec_s = line[17:19]
                    ms_s = line[20:23]
                    if sec_s.isdigit() and ms_s.isdigit():
                        byte_offset_sum += int(sec_s) * 1000 + int(ms_s)

                st_pos = line.find("|status=")
                if st_pos != -1:
                    code_s = line[st_pos + 8 : st_pos + 11]
                    if code_s.isdigit():
                        status_accum += int(code_s)

                off_pos = line.find(" off=")
                if off_pos != -1:
                    end_pos = line.find(" ", off_pos + 5)
                    if end_pos != -1:
                        off_s = line[off_pos + 5 : end_pos]
                        if off_s.isdigit():
                            byte_offset_sum += int(off_s) & 0xFFF

        # 3. Interleaved chunked .read(4096) and .readline()
        with open(log_path, "r", encoding="utf-8") as f:
            while True:
                block = f.read(4096)
                if not block:
                    break
                chunk_transitions += block.count("|status=")
                rem = f.readline()
                if rem:
                    total_chars += len(rem)
                    if "status=500" in rem or "status=429" in rem:
                        chunk_transitions += 3

    return total_chars, status_accum, byte_offset_sum, chunk_transitions


def scenario_memory_stream_multiplex(
    text_chunks: list[str], byte_chunks: list[bytes]
) -> tuple[int, int, int, int]:
    """Multiplex StringIO, BytesIO, bytearray in-place mutation, and memoryview slicing."""
    string_metric_sum = 0
    bytes_checksum = 0
    mutated_bytes_sum = 0
    slice_digest = 0

    for _ in range(5980):
        sio = io.StringIO()
        bio = io.BytesIO()

        for idx, t_chunk in enumerate(text_chunks):
            sio.write(t_chunk)
            bio.write(byte_chunks[idx])
            if (idx & 7) == 7:
                pos_s = sio.tell()
                pos_b = bio.tell()
                if pos_s > 64:
                    sio.seek(pos_s - 48)
                    sample_s = sio.read(24)
                    string_metric_sum += len(sample_s)
                    sio.seek(pos_s)
                if pos_b > 64:
                    bio.seek(pos_b - 48)
                    sample_b = bio.read(24)
                    bytes_checksum += sample_b[0] + sample_b[-1]
                    bio.seek(pos_b)

        # Rewind and scan StringIO via readline() and read(n)
        sio.seek(0)
        while True:
            line = sio.readline()
            if not line:
                break
            m_pos = line.find("alpha_metric=")
            if m_pos != -1:
                semi = line.find(";", m_pos + 13)
                if semi != -1:
                    val_s = line[m_pos + 13 : semi]
                    if val_s.isdigit():
                        string_metric_sum += int(val_s)
            peek = sio.read(16)
            if peek:
                string_metric_sum += peek.count("=")

        full_str = sio.getvalue()
        string_metric_sum += len(full_str) & 0xFFFF

        # Extract BytesIO buffer into bytearray, mutate in-place, and inspect via memoryview
        raw_bytes = bio.getvalue()
        buf = bytearray(raw_bytes)
        n_buf = len(buf)
        step = 37
        idx = 0
        while idx < n_buf:
            old_b = buf[idx]
            new_b = (old_b ^ 0x5A) + (idx & 0x0F) & 0xFF
            buf[idx] = new_b
            mutated_bytes_sum += new_b
            idx += step

        mv = memoryview(buf)
        window = 128
        offset = 0
        while offset + window <= n_buf:
            sub = mv[offset : offset + window]
            sub_head = sub[0:16]
            sub_tail = sub[window - 16 : window]
            slice_digest += sub_head[0] + sub_head[7] + sub_tail[0] + sub_tail[15]
            offset += 96
        sub_head = None
        sub_tail = None
        sub = None
        mv.release()

    return string_metric_sum, bytes_checksum, mutated_bytes_sum, slice_digest


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

    try:
        t0 = time.perf_counter()
        log_path, text_chunks, byte_chunks = prepare(size)
        prep_s = time.perf_counter() - t0

        expected = None
        file_s = 0.0
        mem_s = 0.0
        for _ in range(loops):
            r1 = ()
            r2 = ()
            if scenario in ("all", "large_file_streaming"):
                s0 = time.perf_counter()
                r1 = scenario_large_file_streaming(log_path)
                file_s += time.perf_counter() - s0
            if scenario in ("all", "memory_stream_multiplex"):
                s1 = time.perf_counter()
                r2 = scenario_memory_stream_multiplex(text_chunks, byte_chunks)
                mem_s += time.perf_counter() - s1
            combined = r1 + r2
            if expected is not None and combined != expected:
                raise AssertionError("Result mismatch across loops")
            expected = combined

        total_s = (file_s + mem_s) / loops
        print("topic\tio/streams_and_files")
        print(f"size\t{size}")
        print(f"loops\t{loops}")
        print(f"result\t{expected}")
        print(f"prepare\t{prep_s:.6f}")
        if scenario in ("all", "large_file_streaming"):
            print(f"large_file_streaming\t{file_s / loops:.6f}")
        if scenario in ("all", "memory_stream_multiplex"):
            print(f"memory_stream_multiplex\t{mem_s / loops:.6f}")
        print(f"total\t{total_s:.6f}")
    finally:
        _cleanup_temp_dirs()


if __name__ == "__main__":
    main()
