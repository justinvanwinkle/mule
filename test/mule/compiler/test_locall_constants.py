"""Tests for module-level constant global collection and propagation in ``locall.py``."""

from mule.compiler.ctype import (
    BOOLEAN,
    FIXNUM_MAX,
    FIXNUM_MIN,
    INTEGER,
    TEXT_TYPE,
    UNKNOWN,
    IntegerType,
    TupleType,
)
from mule.compiler.locall import collect_global_constants
from mule.compiler.main import compile_source, compile_toplevel
from mule.compiler.node import GlobalVar, Ref


def _refs_to_global(lambda_, name: str) -> list[Ref]:
    return [
        node
        for block in lambda_.component.blocks()
        for node in block.nodes()
        if isinstance(node, Ref)
        and isinstance(node.leaf, GlobalVar)
        and node.leaf.name == name
    ]


def test_collect_global_constants_filters_and_widens_user_constants():
    source = (
        "_EMPTY = -1\n"
        "_PERTURB_MASK = 0x3FFFFFFFFFFFFFFF\n"
        "_BIG_MASK = 0x10000000000000000\n"
        "_FLAG = True\n"
        "_VOCAB = ('alpha', 'bravo')\n"
        "_X = 1\n"
        "_X = 2\n"
        "_MUTATED_IN_FN = 10\n"
        "c = 1\n"
        "_lower = 2\n"
        "PUBLIC = 3\n"
        "_CACHE = None\n"
        "def mutate():\n"
        " global _MUTATED_IN_FN\n"
        " _MUTATED_IN_FN = 20\n"
        "def read(i: int):\n"
        " return _EMPTY, _PERTURB_MASK, _VOCAB[i], c, _CACHE\n"
    )
    compilation = compile_source(source)

    by_component = collect_global_constants(compilation.components[0])
    by_lambdas = collect_global_constants(compilation.lambdas)
    assert by_component == by_lambdas

    by_name = {leaf.name: vtype for leaf, vtype in by_component.items()}
    assert set(by_name) == {
        "_EMPTY",
        "_PERTURB_MASK",
        "_BIG_MASK",
        "_FLAG",
        "_VOCAB",
    }
    assert by_name["_EMPTY"] == IntegerType(FIXNUM_MIN, FIXNUM_MAX)
    assert by_name["_PERTURB_MASK"] == IntegerType(FIXNUM_MIN, FIXNUM_MAX)
    assert by_name["_BIG_MASK"] is INTEGER
    assert by_name["_FLAG"] is BOOLEAN
    assert by_name["_VOCAB"] == TupleType(2, (TEXT_TYPE, TEXT_TYPE))

    for excluded in ("_X", "_MUTATED_IN_FN", "c", "_lower", "PUBLIC", "_CACHE"):
        assert excluded not in by_name


def test_global_constant_refs_receive_derived_types_and_propagate():
    source = (
        "_EMPTY = -1\n"
        "_PERTURB_MASK = 0x3FFFFFFFFFFFFFFF\n"
        "_VOCAB = ('alpha', 'bravo')\n"
        "c = 1\n"
        "_CACHE = None\n"
        "def lookup(state: int):\n"
        " word = _VOCAB[state & 1]\n"
        " masked = (state & 0x7FFFFFFF) & _PERTURB_MASK\n"
        " is_empty = masked == _EMPTY\n"
        " return word, masked, is_empty, c, _CACHE\n"
    )
    compilation = compile_source(source)
    lookup_fn = compilation.lambdas[1]

    empty_refs = _refs_to_global(lookup_fn, "_EMPTY")
    mask_refs = _refs_to_global(lookup_fn, "_PERTURB_MASK")
    vocab_refs = _refs_to_global(lookup_fn, "_VOCAB")
    c_refs = _refs_to_global(lookup_fn, "c")
    cache_refs = _refs_to_global(lookup_fn, "_CACHE")

    assert len(empty_refs) == 1
    assert empty_refs[0].derived_type == IntegerType(FIXNUM_MIN, FIXNUM_MAX)
    assert empty_refs[0].lvar is not None
    assert empty_refs[0].lvar.derived_type == IntegerType(FIXNUM_MIN, FIXNUM_MAX)

    assert len(mask_refs) == 1
    assert mask_refs[0].derived_type == IntegerType(FIXNUM_MIN, FIXNUM_MAX)
    assert mask_refs[0].lvar is not None
    assert mask_refs[0].lvar.derived_type == IntegerType(FIXNUM_MIN, FIXNUM_MAX)

    assert len(vocab_refs) == 1
    assert vocab_refs[0].derived_type == TupleType(2, (TEXT_TYPE, TEXT_TYPE))
    assert vocab_refs[0].lvar is not None
    assert vocab_refs[0].lvar.derived_type == TupleType(2, (TEXT_TYPE, TEXT_TYPE))

    assert len(c_refs) == 1
    assert c_refs[0].derived_type is UNKNOWN

    assert len(cache_refs) == 1
    assert cache_refs[0].derived_type is UNKNOWN

    assert compilation.facts is not None
    assert compilation.facts[lookup_fn].return_type == TupleType(
        5,
        (
            TEXT_TYPE,
            IntegerType(0, 0x7FFFFFFF),
            BOOLEAN,
            UNKNOWN,
            UNKNOWN,
        ),
    )


def test_protocol_module_keeps_exact_constant_types():
    source = (
        "_EMPTY = -1\n"
        "_PERTURB_MASK = 0x3FFFFFFFFFFFFFFF\n"
        "_helper_const = 8\n"
        "def probe(code: int):\n"
        " return code & _PERTURB_MASK, _EMPTY, _helper_const\n"
    )
    compilation = compile_toplevel(
        source,
        prefix="mule_sequences_",
        namespace="$mule.sequences",
    )
    constants = collect_global_constants(compilation.components[0])
    by_name = {leaf.name: vtype for leaf, vtype in constants.items()}

    assert by_name["_EMPTY"] == IntegerType(-1, -1)
    assert by_name["_PERTURB_MASK"] == IntegerType(
        0x3FFFFFFFFFFFFFFF, 0x3FFFFFFFFFFFFFFF
    )
    assert by_name["_helper_const"] == IntegerType(8, 8)
