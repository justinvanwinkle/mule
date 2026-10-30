"""Mutable calls enforce their checked definition-derived result contract."""

import pytest

from mule.compiler.driver import emit_llvm
from test.mule.compiler.test_modules import execute

# Each constructor call that local-call analysis sees passes a fixnum ident,
# while bits accumulates past the fixnum range.
BIGINT_FIELD_FROM_SEEN_CALLS_SOURCE = """
class Block:
    def __init__(self, ident: int, bits: int) -> None:
        self.ident = ident
        self.bits = bits


def build(seed: int) -> list[Block]:
    blocks = []
    for ident in range(4):
        bits = 0
        for offset in range(3):
            bits |= 1 << ((seed + ident * 70 + offset) % 300)
        blocks.append(Block(ident, bits))
    return blocks


def checksum(blocks: list[Block]) -> int:
    total = 0
    for block in blocks:
        total ^= block.bits + block.ident
    return total


print(checksum(build(7)))
"""

# Local-call analysis sees only fixnum arguments; the bigint reaches the
# constructor through a class object it cannot resolve.
BIGINT_FIELD_FROM_UNSEEN_CALL_SOURCE = """
class Block:
    def __init__(self, ident: int) -> None:
        self.ident = ident


def total(blocks: list[Block]) -> int:
    result = 0
    for block in blocks:
        result += block.ident
    return result


blocks = [Block(ident) for ident in range(4)]
kinds = [int, Block]
kind = kinds[len(blocks) % 3]
blocks.append(kind(1 << 70))
print(total(blocks))
"""


def run_source(tmp_path, source):
    result = execute(tmp_path, {}, source)
    assert result.returncode == 0, result.stderr
    return result.stdout


@pytest.mark.parametrize(
    ("source", "expected"),
    [
        (
            BIGINT_FIELD_FROM_SEEN_CALLS_SOURCE,
            "1474372083359800613772101229302503767360458393641048028605216457600\n",
        ),
        (BIGINT_FIELD_FROM_UNSEEN_CALL_SOURCE, "1180591620717411303430\n"),
    ],
)
def test_int_parameter_field_reads_bigint_values(tmp_path, source, expected):
    assert run_source(tmp_path, source) == expected


@pytest.mark.integration
def test_inherited_readinto_uses_overridden_read_result(tmp_path):
    source = """import io
stream = io.BytesIO(b"abcdef")
buffer = bytearray(3)
print(stream.readinto(buffer), buffer)
"""
    assert run_source(tmp_path, source) == "3 bytearray(b'abc')\n"


@pytest.mark.parametrize("safety", [0, 2, 3])
@pytest.mark.parametrize("bad_result", ["1.5", "11", "True"])
@pytest.mark.parametrize(
    "replacement", ["produce = incompatible", "globals()['produce'] = incompatible"]
)
def test_incompatible_rebinding_errors_without_freezing_compatible_replacement(
    tmp_path, safety, replacement, bad_result
):
    source = f"""__safety__ = {safety}
def produce(flag):
    return 3 if flag else 10
def consume(flag):
    return produce(flag) + 2
for flag in (True, False):
    print(consume(flag))
def compatible(flag):
    return 7
produce = compatible
print(consume(True))
def incompatible(flag):
    return {bad_result}
{replacement}
try:
    consume(False)
except TypeError:
    print('incompatible')
"""
    assert run_source(tmp_path, source) == "5\n12\n9\nincompatible\n"


@pytest.mark.parametrize(
    "value", ["[7, 'changed']", "(7, 42)", "(7, 'changed', 1)", "(11, 'changed')"]
)
def test_original_tuple_shape_and_fields_are_checked_before_unpacking(tmp_path, value):
    source = f"""def produce(flag):
    return (3, 'a') if flag else (10, 'b')
def consume(flag):
    number, text = produce(flag)
    return number + len(text)
print(consume(True))
def replacement(flag):
    return {value}
globals()['produce'] = replacement
try:
    consume(False)
except TypeError:
    print('incompatible tuple')
"""
    assert run_source(tmp_path, source) == "4\nincompatible tuple\n"


def test_result_assertion_precedes_later_operand_effects(tmp_path):
    source = """def produce():
    return 3
def later():
    print('later operand')
    return 2
def consume():
    return produce() + later()
def replacement():
    return 1.5
globals()['produce'] = replacement
try:
    consume()
except TypeError:
    print('checked first')
"""
    assert run_source(tmp_path, source) == "checked first\n"


def test_recursive_tuple_contract_accepts_branches_and_checks_rebinding(tmp_path):
    source = """def make_tree(item, depth):
    if depth <= 0:
        return (item, None, None)
    return (item, make_tree(item, depth - 1), make_tree(item, depth - 1))
def root_shape(depth):
    tree = make_tree(1, depth)
    return (tree[0], tree[1] is None)
for depth in (0, 1, 3):
    print(root_shape(depth))
def incompatible(item, depth):
    return ('wrong', None, None)
globals()['make_tree'] = incompatible
try:
    root_shape(1)
except TypeError:
    print('incompatible tree')
"""
    assert run_source(tmp_path, source) == (
        "(1, True)\n(1, False)\n(1, False)\nincompatible tree\n"
    )


@pytest.mark.parametrize("directive", ["__debug_level__ = 3", "__speed__ = 0"])
def test_existing_policy_opt_out_keeps_result_type_dynamic(tmp_path, directive):
    source = f"""{directive}
def produce():
    return 3
def consume():
    return produce() + 2
def replacement():
    return 1.5
produce = replacement
print(consume())
"""
    assert run_source(tmp_path, source) == "3.5\n"


def test_direct_call_helper_absorbs_return_contract_into_fallback_branch():
    ir = emit_llvm(
        """
def produce(flag):
    return 3 if flag else 10
def consume(flag):
    return produce(flag) + 2
"""
    )
    marker = 'define internal ptr @"mule.direct.mule_fn_produce.'
    start = ir.index(marker)
    end = ir.index("\n}", start)
    helper = ir[start:end]
    direct_part, fallback_part = helper.split("\nfallback:\n", 1)
    assert "mule.return.check" not in direct_part
    assert "mule.return.check" in fallback_part


def test_field_initialized_to_int_and_updated_with_unknown_expression_widens(tmp_path):
    source = """
class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

class Particle:
    def __init__(self, x, y, mass=1.0):
        self.position = Point(x, y)
        self.mass = mass

class QuadTree:
    def __init__(self):
        self.center_of_mass = Point(0, 0)
        self.total_mass = 0

    def update_mass_distribution(self, particle):
        total_mass_new = self.total_mass + particle.mass
        if total_mass_new > 0:
            self.center_of_mass.x = (
                self.center_of_mass.x * self.total_mass
                + particle.position.x * particle.mass
            ) / total_mass_new
            self.center_of_mass.y = (
                self.center_of_mass.y * self.total_mass
                + particle.position.y * particle.mass
            ) / total_mass_new
        self.total_mass = total_mass_new

tree = QuadTree()
tree.update_mass_distribution(Particle(2.0, 4.0, 1.5))
tree.update_mass_distribution(Particle(6.0, 8.0, 2.5))
print(tree.total_mass, tree.center_of_mass.x, tree.center_of_mass.y)
"""
    assert run_source(tmp_path, source) == "4.0 4.5 6.5\n"
