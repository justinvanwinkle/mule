"""Generated Unicode name records match the host Python's Unicode database."""

import importlib.util
import unicodedata
from pathlib import Path


def test_all_generated_names_and_algorithmic_ranges_match_python():
    root = Path(__file__).resolve().parents[3]
    spec = importlib.util.spec_from_file_location(
        "_unicode_names", root / "stdlib/_unicode_names.py"
    )
    assert spec is not None and spec.loader is not None
    data = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(data)
    for cp in range(0x110000):
        name = unicodedata.name(chr(cp), "")
        if not name:
            continue
        prefix, _, suffix = name.rpartition("-")
        if prefix.encode() + b"-" in data.ALGORITHMIC_RANGES and suffix == f"{cp:X}":
            assert any(
                first <= cp <= last
                for first, last in data.ALGORITHMIC_RANGES[prefix.encode() + b"-"]
            )
        else:
            assert data.lookup_name(name.encode()) == chr(cp), name
    for prefix, ranges in data.ALGORITHMIC_RANGES.items():
        for first, last in ranges:
            for cp in range(first, last + 1):
                assert unicodedata.lookup(prefix.decode() + f"{cp:X}") == chr(cp)
    for filename in ("NameAliases.txt", "NamedSequences.txt"):
        for line in (root / "tools/unicode" / filename).read_text().splitlines():
            fields = line.partition("#")[0].strip().split(";")
            if len(fields) == 1:
                continue
            name = fields[1] if filename == "NameAliases.txt" else fields[0]
            assert data.lookup_name(name.encode()) == unicodedata.lookup(name), name
