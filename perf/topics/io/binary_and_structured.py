"""Binary framing, LEB128 varint decoding, SHA-256 verification, and structured
JSON/CSV ingestion workloads.
"""

import csv
import hashlib
import io
import json
import struct
import sys
import time
from typing import Any

DEFAULT_SIZE = 2000
DEFAULT_LOOPS = 1

_PAYLOAD_FRAGMENTS = (
    b"rpc:invoke:service=ledger;op=commit;shard=01;flags=sync",
    b"rpc:invoke:service=inventory;op=reserve;shard=04;flags=batch",
    b"rpc:invoke:service=auth;op=verify_token;shard=02;flags=fast",
    b"rpc:invoke:service=analytics;op=flush_events;shard=07;flags=async",
)

_CSV_NOTES = (
    'standard_settlement',
    'quoted,"comma,value",segment',
    'multi_field;"escaped""quote""here";ok',
    'regional_adjustment,tier_2',
)


def _encode_leb128(value: int) -> bytes:
    out = bytearray()
    v = value
    while True:
        byte = v & 0x7F
        v >>= 7
        if v:
            out.append(byte | 0x80)
        else:
            out.append(byte)
            break
    return bytes(out)


class PacketRecord:
    def __version_tag(self) -> int:
        return self.version ^ self.flags

    def __init__(
        self,
        version: int,
        flags: int,
        seq_id: int,
        timestamp_ns: int,
        varint_a: int,
        varint_b: int,
        tail_tag: int,
        digest_prefix: int,
        payload_len: int,
    ) -> None:
        self.version = version
        self.flags = flags
        self.seq_id = seq_id
        self.timestamp_ns = timestamp_ns
        self.varint_a = varint_a
        self.varint_b = varint_b
        self.tail_tag = tail_tag
        self.digest_prefix = digest_prefix
        self.payload_len = payload_len

    def score(self) -> int:
        return (
            self.__version_tag()
            + (self.seq_id & 0xFFFF)
            + (self.timestamp_ns & 0xFFFFFF)
            + (self.varint_a & 0xFFFFF)
            + (self.varint_b & 0xFFFFF)
            + self.tail_tag
            + self.digest_prefix
            + self.payload_len
        )


class DomainNode:
    def __init__(self, kind: str, weight: int, score: float, active: bool, tags: list[str]) -> None:
        self.kind = kind
        self.weight = weight
        self.score = score
        self.active = active
        self.tags = tags


def prepare(size: int) -> tuple[bytes, list[str], str]:
    packet_stream = bytearray()
    json_docs: list[str] = []
    csv_buf = io.StringIO()
    csv_writer = csv.writer(csv_buf, lineterminator="\n")
    csv_writer.writerow(["seq_id", "account_code", "amount", "tax_rate", "active", "note"])

    state = 88172645
    for i in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        version = 1 + (state & 0x03)
        flags = (state >> 2) & 0x3FFF
        seq_id = i + 10000
        timestamp_ns = 1730000000000 + i * 25000 + (state & 0xFFF)
        header = struct.pack(">BHIq", version, flags, seq_id, timestamp_ns)

        varint_a = ((state >> 5) & 0x1FFFFF) + (i * 19)
        varint_b = ((state >> 11) & 0x3FFFF) + (i * 7)
        vb_a = _encode_leb128(varint_a)
        vb_b = _encode_leb128(varint_b)

        base_frag = _PAYLOAD_FRAGMENTS[state % len(_PAYLOAD_FRAGMENTS)]
        tail_bytes = (state ^ (i * 2654435761 & 0xFFFFFFFF)).to_bytes(4, "big")
        payload = base_frag + b"|seq=" + str(i).encode("ascii") + b"|" + tail_bytes
        digest8 = hashlib.sha256(payload).digest()[:8]

        packet_stream.extend(header)
        packet_stream.extend(vb_a)
        packet_stream.extend(vb_b)
        packet_stream.extend(len(payload).to_bytes(2, "big"))
        packet_stream.extend(payload)
        packet_stream.extend(digest8)

        # Build nested JSON document
        is_active = (state & 1) == 0
        opt_reason = None if is_active else f"hold_{(state >> 16) & 0xFF}"
        doc_obj = {
            "__type__": "order_envelope",
            "id": seq_id,
            "active": is_active,
            "reason": opt_reason,
            "metrics": {
                "__type__": "metric_node",
                "kind": f"tier_{version}",
                "weight": (state >> 7) & 0x3FF,
                "score": round(((state & 0xFFFF) / 256.0), 4),
                "active": (state & 4) != 0,
                "tags": [f"shard_{state & 7}", f"v{version}", "verified"],
            },
            "items": [
                {
                    "__type__": "line_item",
                    "sku": f"SKU-{(state >> 9) & 0xFFF:04d}",
                    "qty": 1 + (state % 9),
                    "unit_price": round(12.5 + (i % 40) * 1.25, 2),
                    "discounted": (i & 1) == 1,
                },
                {
                    "__type__": "line_item",
                    "sku": f"SKU-{(state >> 15) & 0xFFF:04d}",
                    "qty": 2 + ((state >> 3) % 5),
                    "unit_price": round(5.0 + (i % 15) * 0.75, 2),
                    "discounted": False,
                },
            ],
        }
        json_docs.append(json.dumps(doc_obj, separators=(",", ":")))

        # Write CSV row with escaping and numeric fields
        acct = f"ACCT-{(state >> 12) & 0xFFFF:04X}"
        amt = f"{100.0 + (state & 0x7FF) * 0.375:.3f}"
        rate = f"{0.05 + (i % 8) * 0.0125:.4f}"
        act_str = "true" if is_active else "false"
        note = _CSV_NOTES[state % len(_CSV_NOTES)]
        csv_writer.writerow([seq_id, acct, amt, rate, act_str, note])

    return bytes(packet_stream), json_docs, csv_buf.getvalue()


def scenario_binary_framing_and_hashing(packet_bytes: bytes) -> tuple[int, int, int, int]:
    """Unpack framed binary messages, decode LEB128 varints, and verify SHA-256 digests."""
    decoded_packets = 0
    verified_digests = 0
    varint_accum = 0
    record_score_sum = 0

    n = len(packet_bytes)
    for _ in range(2850):
        offset = 0
        while offset < n:
            version, flags, seq_id, timestamp_ns = struct.unpack_from(">BHIq", packet_bytes, offset)
            offset += 15

            # Decode varint_a
            va = 0
            shift = 0
            while True:
                b = packet_bytes[offset]
                offset += 1
                va |= (b & 0x7F) << shift
                if (b & 0x80) == 0:
                    break
                shift += 7

            # Decode varint_b
            vb = 0
            shift = 0
            while True:
                b = packet_bytes[offset]
                offset += 1
                vb |= (b & 0x7F) << shift
                if (b & 0x80) == 0:
                    break
                shift += 7

            payload_len = int.from_bytes(packet_bytes[offset : offset + 2], "big")
            offset += 2

            payload = packet_bytes[offset : offset + payload_len]
            offset += payload_len

            expected_digest8 = packet_bytes[offset : offset + 8]
            offset += 8

            actual_digest = hashlib.sha256(payload).digest()
            if actual_digest[:8] == expected_digest8:
                verified_digests += 1

            tail_tag = int.from_bytes(payload[-4:], "big") & 0xFFFF
            digest_prefix = int.from_bytes(actual_digest[:2], "big")
            rec = PacketRecord(
                version=version,
                flags=flags,
                seq_id=seq_id,
                timestamp_ns=timestamp_ns,
                varint_a=va,
                varint_b=vb,
                tail_tag=tail_tag,
                digest_prefix=digest_prefix,
                payload_len=payload_len,
            )
            decoded_packets += 1
            varint_accum += (va ^ vb) & 0xFFFFFF
            record_score_sum += rec.score() & 0xFFFFFF

    return decoded_packets, verified_digests, varint_accum, record_score_sum


def _domain_object_hook(d: dict[str, Any]) -> Any:
    tag = d.get("__type__")
    if tag == "metric_node":
        kind = d["kind"]
        weight = d["weight"]
        score = d["score"]
        active = d["active"]
        tags = d["tags"]
        if (
            not isinstance(kind, str)
            or not isinstance(weight, int)
            or not isinstance(score, (int, float))
            or not isinstance(active, bool)
            or not isinstance(tags, list)
        ):
            raise TypeError("Invalid metric_node schema")
        return DomainNode(kind, weight, float(score), active, tags)
    if tag == "line_item":
        qty = d["qty"]
        price = d["unit_price"]
        disc = d["discounted"]
        if not isinstance(qty, int) or not isinstance(price, (int, float)) or not isinstance(disc, bool):
            raise TypeError("Invalid line_item schema")
        eff_cents = int(round(qty * float(price) * (90.0 if disc else 100.0)))
        return (d["sku"], qty, eff_cents)
    if tag == "order_envelope":
        reason = d["reason"]
        if reason is not None and not isinstance(reason, str):
            raise TypeError("Invalid reason field")
        return d
    return d


def scenario_json_and_csv_ingestion(
    json_docs: list[str], csv_text: str
) -> tuple[int, int, int, int]:
    """Parse nested JSON documents with object_hook validation and multi-column CSV streams."""
    envelope_count = 0
    total_item_cents = 0
    node_weight_sum = 0
    csv_net_cents = 0

    for _ in range(925):
        for raw_doc in json_docs:
            env = json.loads(raw_doc, object_hook=_domain_object_hook)
            if isinstance(env, dict) and env.get("active") is True:
                envelope_count += 1
            elif isinstance(env, dict) and env.get("reason") is not None:
                envelope_count += 2

            node = env["metrics"]
            if isinstance(node, DomainNode):
                node_weight_sum += node.weight + int(node.score * 10.0) + len(node.tags)
                if node.active:
                    node_weight_sum += 5

            items = env["items"]
            if isinstance(items, list):
                for sku, qty, eff_cents in items:
                    total_item_cents += eff_cents + (qty if sku.startswith("SKU-") else 0)

        reader = csv.reader(io.StringIO(csv_text))
        header = next(reader, None)
        if header is None or len(header) != 6:
            raise ValueError("Malformed CSV header")
        for row in reader:
            seq_s, acct_s, amt_s, rate_s, act_s, note_s = row
            seq_num = int(seq_s)
            amt_val = float(amt_s)
            rate_val = float(rate_s)
            gross_cents = int(round(amt_val * (1.0 + rate_val) * 100.0))
            if act_s == "true":
                csv_net_cents += gross_cents + (seq_num & 0x3F)
            else:
                csv_net_cents += (gross_cents >> 1) + len(acct_s)
            if "," in note_s or '"' in note_s:
                csv_net_cents += 7

    return envelope_count, total_item_cents, node_weight_sum, csv_net_cents


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
    packet_bytes, json_docs, csv_text = prepare(size)
    prep_s = time.perf_counter() - t0

    expected = None
    bin_s = 0.0
    struct_s = 0.0
    for _ in range(loops):
        r1 = ()
        r2 = ()
        if scenario in ("all", "binary_framing_and_hashing"):
            s0 = time.perf_counter()
            r1 = scenario_binary_framing_and_hashing(packet_bytes)
            bin_s += time.perf_counter() - s0
        if scenario in ("all", "json_and_csv_ingestion"):
            s1 = time.perf_counter()
            r2 = scenario_json_and_csv_ingestion(json_docs, csv_text)
            struct_s += time.perf_counter() - s1
        combined = r1 + r2
        if expected is not None and combined != expected:
            raise AssertionError("Result mismatch across loops")
        expected = combined

    total_s = (bin_s + struct_s) / loops
    print("topic\tio/binary_and_structured")
    print(f"size\t{size}")
    print(f"loops\t{loops}")
    print(f"result\t{expected}")
    print(f"prepare\t{prep_s:.6f}")
    if scenario in ("all", "binary_framing_and_hashing"):
        print(f"binary_framing_and_hashing\t{bin_s / loops:.6f}")
    if scenario in ("all", "json_and_csv_ingestion"):
        print(f"json_and_csv_ingestion\t{struct_s / loops:.6f}")
    print(f"total\t{total_s:.6f}")


if __name__ == "__main__":
    main()
