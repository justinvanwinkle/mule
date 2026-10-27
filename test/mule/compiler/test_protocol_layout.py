"""Shared table helpers consume receiver facts from ordinary method callers."""

import re

import pytest

from mule.compiler.ctype import NONE_TYPE, InstanceType, union_type
from mule.compiler.vops import INSTANCE_FIELDS_OFFSET
from test.mule.compiler.rewrite_support import build_case


def test_union_layout_uses_only_common_fixed_positions():
    left = InstanceType("left", fixed_slots=("first", "second"))
    right = InstanceType("right", fixed_slots=("first", "second", "third"))
    common = union_type(left, right)
    assert common.fixed_slot_index("first") == 0
    assert common.fixed_slot_index("second") == 1
    assert common.fixed_slot_index("third") is None
    reordered = InstanceType("reordered", fixed_slots=("second", "first"))
    assert union_type(left, reordered).fixed_slot_index("second") is None
    assert union_type(left, NONE_TYPE).fixed_slot_index("first") is None


@pytest.mark.integration
def test_dict_lookup_uses_general_layout_and_native_hash_comparison(tmp_path):
    case = build_case(
        tmp_path,
        """
n = len(input())
table = {}
for index in range(n):
    table[index] = index + 1
for index in range(n):
    print(table.get(index), table[index])
print(table.get(n + 1))
for index in range(n):
    del table[index]
print(len(table))
""",
    )
    # The real shared lookup has an observed receiver union. Its borrowed
    # internal entries are the paths ordinary dict methods call.
    lookup = case.function_ir(
        r"mule_fn__table_lookup\.borrow\.observed", unit="builtins"
    )
    assert re.search(
        rf"getelementptr i8, ptr %[-\w.]+, i64 {INSTANCE_FIELDS_OFFSET}\b", lookup
    )
    decoded = set(re.findall(r"(%[-\w.]+) = ashr i64 %[-\w.]+, 1\b", lookup))
    equalities = re.findall(r"icmp eq i64 (%[-\w.]+), (%[-\w.]+)", lookup)
    # Both hash operands must already be decoded fixnums. Cache/tag checks
    # cannot satisfy this assertion, and generic key equality may remain.
    assert any(left in decoded and right in decoded for left, right in equalities)
    case.compare("abcdef\n")
    case.compare("\n")
