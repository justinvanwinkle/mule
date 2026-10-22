"""Decode, unmask, mutate, and checksum framed binary packets.

Exercises bytes indexing, bytearray allocation, append, indexed in-place
mutation, LEB128 varint decoding, and rolling checksum arithmetic.
"""

from perfutils import run_case

NAME = "bytes_codec"
DESCRIPTION = (
    "Binary packet framing, LEB128 varint decoding, bytearray mutation, and checksums"
)
DEFAULT_SIZE = 35_000
DEFAULT_SEED = 42


def encode_varint(buf: bytearray, value: int) -> None:
    v = value
    while v >= 0x80:
        buf.append((v & 0x7F) | 0x80)
        v = v >> 7
    buf.append(v & 0x7F)


def prepare(size: int, seed: int) -> list[bytes]:
    if size < 1:
        raise ValueError("size must be positive")
    state = seed & 0x7FFFFFFF
    packets: list[bytes] = []
    for _ in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        seq_id = (state >> 4) & 0x3FFFF
        mask = (state >> 12) & 0xFF
        payload_len = 8 + ((state >> 20) % 25)
        buf = bytearray()
        encode_varint(buf, seq_id)
        buf.append(mask)
        encode_varint(buf, payload_len)
        for j in range(payload_len):
            state = (state * 1103515245 + 12345) & 0x7FFFFFFF
            raw = (state >> 16) & 0xFF
            buf.append(raw ^ ((mask + j * 13) & 0xFF))
        packets.append(bytes(buf))
    return packets


def decode_varint(packet: bytes, pos: int) -> tuple[int, int]:
    result = 0
    shift = 0
    while True:
        byte = packet[pos]
        pos += 1
        result = result | ((byte & 0x7F) << shift)
        if (byte & 0x80) == 0:
            return result, pos
        shift += 7


def run(data: list[bytes]) -> tuple[int, int, int, int]:
    packet_count = 0
    seq_sum = 0
    total_bytes = 0
    digest = 0

    for packet in data:
        seq_id, pos = decode_varint(packet, 0)
        mask = packet[pos]
        pos += 1
        payload_len, pos = decode_varint(packet, pos)

        decoded = bytearray()
        for j in range(payload_len):
            b = packet[pos + j] ^ ((mask + j * 13) & 0xFF)
            decoded.append(b)

        k = 0
        limit = payload_len - 1
        while k < limit:
            left = decoded[k]
            right = decoded[k + 1]
            decoded[k] = (right + 3) & 0xFF
            decoded[k + 1] = left ^ 0x5A
            k += 2

        s1 = 1
        s2 = 0
        for b in decoded:
            s1 = (s1 + b) % 65521
            s2 = (s2 + s1) % 65521

        packet_Checksum = (s2 << 16) | s1
        digest = ((digest * 33) + packet_Checksum + seq_id) & 0x3FFFFFFF
        packet_count += 1
        seq_sum += seq_id
        total_bytes += payload_len

    return packet_count, seq_sum, total_bytes, digest


def main():
    run_case(NAME, DESCRIPTION, DEFAULT_SIZE, DEFAULT_SEED, prepare, run)


if __name__ == "__main__":
    main()
