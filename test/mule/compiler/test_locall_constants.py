"""Tests for module-level constant global collection and propagation in ``locall.py``."""

from mule.compiler.ctype import (
    BOOLEAN,
    BYTES_TYPE,
    FIXNUM_MAX,
    FIXNUM_MIN,
    FLOAT,
    FROZENSET_TYPE,
    INTEGER,
    NONE_TYPE,
    TEXT_TYPE,
    UNKNOWN,
    IntegerType,
    ListType,
    TupleType,
)
from mule.compiler.locall import collect_global_constants
from mule.compiler.main import compile_source, compile_toplevel
from mule.compiler.modules import ModuleCompiler
from mule.compiler.node import Combination, Constant, GlobalVar, Ref


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
        "c",
        "_lower",
        "PUBLIC",
    }
    assert by_name["_EMPTY"] == IntegerType(FIXNUM_MIN, FIXNUM_MAX)
    assert by_name["_PERTURB_MASK"] == IntegerType(FIXNUM_MIN, FIXNUM_MAX)
    assert by_name["_BIG_MASK"] is INTEGER
    assert by_name["_FLAG"] is BOOLEAN
    assert by_name["_VOCAB"] == TupleType(2, (TEXT_TYPE, TEXT_TYPE))
    assert by_name["c"] == IntegerType(FIXNUM_MIN, FIXNUM_MAX)
    assert by_name["_lower"] == IntegerType(FIXNUM_MIN, FIXNUM_MAX)
    assert by_name["PUBLIC"] == IntegerType(FIXNUM_MIN, FIXNUM_MAX)

    for excluded in ("_X", "_MUTATED_IN_FN", "_CACHE"):
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
    assert c_refs[0].derived_type == IntegerType(FIXNUM_MIN, FIXNUM_MAX)

    assert len(cache_refs) == 1
    assert cache_refs[0].derived_type is UNKNOWN

    assert compilation.facts is not None
    assert compilation.facts[lookup_fn].return_type == TupleType(
        5,
        (
            TEXT_TYPE,
            IntegerType(0, 0x7FFFFFFF),
            BOOLEAN,
            IntegerType(FIXNUM_MIN, FIXNUM_MAX),
            UNKNOWN,
        ),
    )


def test_protocol_module_keeps_exact_constant_types():
    source = (
        "_EMPTY = -1\n"
        "_PERTURB_MASK = 0x3FFFFFFFFFFFFFFF\n"
        "_helper_const = 8\n"
        "public_const = 16\n"
        "def probe(code: int):\n"
        " return code & _PERTURB_MASK, _EMPTY, _helper_const, public_const\n"
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
    assert by_name["public_const"] == IntegerType(16, 16)


def test_final_annotations_and_frozen_container_constants(tmp_path):
    (tmp_path / "typing.py").write_text("Final = object()\n", encoding="utf-8")
    source = (
        "from typing import Final\n"
        "import typing\n"
        "LIMIT: Final = 100\n"
        "TYPED_LIMIT: Final[int] = 200\n"
        "QUALIFIED_FINAL: typing.Final[str] = 'ok'\n"
        "EMPTY_SENTINEL: Final = None\n"
        "FROZEN_WORDS = frozenset(('a', 'b'))\n"
        "TUPLE_WORDS = tuple(('x', 'y'))\n"
        "RAW_BYTES = bytes(b'abc')\n"
        "def read(word: str):\n"
        " return LIMIT, TYPED_LIMIT, QUALIFIED_FINAL, EMPTY_SENTINEL, word in FROZEN_WORDS, TUPLE_WORDS, RAW_BYTES\n"
    )
    main_path = tmp_path / "main.py"
    main_path.write_text(source, encoding="utf-8")
    compilation = ModuleCompiler(str(main_path), roots=(tmp_path,)).compile(
        source, str(main_path)
    )
    by_name = {
        leaf.name: vtype
        for leaf, vtype in collect_global_constants(compilation.lambdas).items()
        if leaf.namespace == "__main__"
    }
    assert by_name["LIMIT"] == IntegerType(100, 100)
    assert by_name["TYPED_LIMIT"] == IntegerType(200, 200)
    assert by_name["QUALIFIED_FINAL"] is TEXT_TYPE
    assert by_name["EMPTY_SENTINEL"] is NONE_TYPE
    assert by_name["FROZEN_WORDS"] is FROZENSET_TYPE
    assert by_name["TUPLE_WORDS"] == TupleType()
    assert by_name["RAW_BYTES"] is BYTES_TYPE

    read_fn = compilation.lambdas[1]
    for final_name in ("LIMIT", "TYPED_LIMIT", "QUALIFIED_FINAL", "EMPTY_SENTINEL"):
        assert _refs_to_global(read_fn, final_name) == []
    constant_values = {
        getattr(node.leaf.value, "value", None)
        for block in read_fn.component.blocks()
        for node in block.nodes()
        if isinstance(node, Ref) and isinstance(node.leaf, Constant)
    }
    assert {100, 200, "ok", None}.issubset(constant_values)
    assert compilation.facts is not None
    assert compilation.facts[read_fn].return_type == TupleType(
        7,
        (
            IntegerType(100, 100),
            IntegerType(200, 200),
            TEXT_TYPE,
            NONE_TYPE,
            BOOLEAN,
            TupleType(),
            BYTES_TYPE,
        ),
    )


def test_flushable_known_call_with_singleton_derived_type_folds_and_flushes_global_ref():
    source = (
        "_RECORDS = ('a', 'b', 'c', 'd', 'e', 'f')\n"
        "_VOCAB = ('alpha', 'bravo', 'charlie', 'delta')\n"
        "def pick(state: int) -> tuple[str, str]:\n"
        " next_state = state & 0x7FFFFFFF\n"
        " rec = _RECORDS[(next_state >> 8) % len(_RECORDS)]\n"
        " word = _VOCAB[(next_state >> 8) % len(_VOCAB)]\n"
        " return rec, word\n"
    )
    compilation = compile_source(source)
    pick_fn = compilation.lambdas[1]

    records_refs = _refs_to_global(pick_fn, "_RECORDS")
    vocab_refs = _refs_to_global(pick_fn, "_VOCAB")
    assert len(records_refs) == 1
    assert len(vocab_refs) == 1

    call_names = [
        node.fun_info.name
        for block in pick_fn.component.blocks()
        for node in block.nodes()
        if isinstance(node, Combination) and node.fun_info is not None
    ]
    assert "_storage_length" not in call_names
    assert "len" not in call_names
    assert call_names.count("%modulo") == 1
    assert call_names.count("%bit_and") == 2

    constant_values = {
        getattr(node.leaf.value, "value", None)
        for block in pick_fn.component.blocks()
        for node in block.nodes()
        if isinstance(node, Ref) and isinstance(node.leaf, Constant)
    }
    assert 6 in constant_values
    assert 3 in constant_values


def test_dynamic_module_namespace_mutations_disable_constant_folding(tmp_path):
    (tmp_path / "sys.py").write_text("modules = {}\n", encoding="utf-8")
    main_path = tmp_path / "main.py"
    mutating_sources = [
        "X = 1\ndef mutate():\n globals()['X'] = 2\ndef read():\n return X\n",
        "import sys\nX = 1\ndef mutate():\n setattr(sys.modules[__name__], 'X', 2)\ndef read():\n return X\n",
        "import sys\nX = 1\ndef mutate():\n sys.modules[__name__].X = 2\ndef read():\n return X\n",
        "X = 1\ndef mutate():\n exec('X = 2')\ndef read():\n return X\n",
    ]
    for source in mutating_sources:
        main_path.write_text(source, encoding="utf-8")
        compilation = ModuleCompiler(str(main_path), roots=(tmp_path,)).compile(
            source, str(main_path)
        )
        main_consts = {
            leaf.name: vtype
            for leaf, vtype in collect_global_constants(compilation.lambdas).items()
            if leaf.namespace == "__main__"
        }
        assert main_consts == {}

    readonly_globals_source = (
        "X = 1\n"
        "def inspect_ns():\n"
        " g = globals()\n"
        " return g.get('X'), 'X' in g\n"
        "def read():\n"
        " return X\n"
    )
    readonly_comp = compile_source(readonly_globals_source)
    by_name = {
        leaf.name: vtype
        for leaf, vtype in collect_global_constants(readonly_comp.lambdas).items()
    }
    assert by_name["X"] == IntegerType(FIXNUM_MIN, FIXNUM_MAX)


def test_cross_module_imported_constants_propagate_without_underscore_spelling(
    tmp_path,
):
    (tmp_path / "typing.py").write_text("Final = object()\n", encoding="utf-8")
    (tmp_path / "consts.py").write_text(
        "from typing import Final\n"
        "PUBLIC_MASK = 0xFF\n"
        "lower_step = 4\n"
        "FINAL_STEP: Final = 16\n"
        "FROZEN_KEYS = frozenset(('a', 'b'))\n"
        "REASSIGNED = 1\n"
        "REASSIGNED = 2\n"
        "def helper() -> int:\n"
        " return 7\n",
        encoding="utf-8",
    )
    main_source = (
        "from consts import PUBLIC_MASK, lower_step, FINAL_STEP, FROZEN_KEYS, REASSIGNED, helper\n"
        "def compute(x: int, k: str):\n"
        " fn_val = helper\n"
        " return (x & 0x7FFFFFFF) & PUBLIC_MASK, lower_step, FINAL_STEP, k in FROZEN_KEYS, REASSIGNED, helper(), fn_val\n"
    )
    main_path = tmp_path / "main.py"
    main_path.write_text(main_source, encoding="utf-8")

    compiler = ModuleCompiler(str(main_path), roots=(tmp_path,))
    compilation = compiler.compile(main_source, str(main_path))
    compute_fn = compilation.lambdas[1]
    assert _refs_to_global(compute_fn, "FINAL_STEP") == []
    assert compilation.facts is not None
    assert compilation.facts[compute_fn].return_type == TupleType(
        7,
        (
            IntegerType(0, 0x7FFFFFFF),
            IntegerType(FIXNUM_MIN, FIXNUM_MAX),
            IntegerType(16, 16),
            BOOLEAN,
            UNKNOWN,
            IntegerType(7, 7),
            UNKNOWN,
        ),
    )


def test_module_global_containers_propagate_to_default_parameter_specialization():
    source = (
        "def combinations(items):\n"
        " result = []\n"
        " for i in range(len(items)):\n"
        "  for j in range(i + 1, len(items)):\n"
        "   result.append((items[i], items[j]))\n"
        " return result\n"
        "BODIES = {'a': ([1.0, 2.0], [0.1, 0.2], 3.0), 'b': ([4.0, 5.0], [0.3, 0.4], 6.0)}\n"
        "SYSTEM = list(BODIES.values())\n"
        "PAIRS = combinations(SYSTEM)\n"
        "def step(dt, n, bodies=SYSTEM, pairs=PAIRS):\n"
        " for _ in range(n):\n"
        "  for ([x1, y1], v1, m1), ([x2, y2], v2, m2) in pairs:\n"
        "   dx = x1 - x2\n"
        "   dy = y1 - y2\n"
        "   mag = dt * ((dx * dx + dy * dy) ** (-1.5))\n"
        "   v1[0] -= dx * m2 * mag\n"
        "   v2[0] += dx * m1 * mag\n"
        "  for r, [vx, vy], _m in bodies:\n"
        "   r[0] += vx * dt\n"
        "   r[1] += vy * dt\n"
        "def run():\n"
        " step(0.01, 2)\n"
        " return SYSTEM[0][0][0]\n"
    )
    compilation = compile_source(source)
    by_name = {lam.source_name: lam for lam in compilation.lambdas}
    step_fn = by_name["step"]
    assert step_fn.default_types[0] == ListType(
        TupleType(3, (ListType(FLOAT), ListType(FLOAT), FLOAT))
    )
    assert step_fn.default_types[1] == ListType(
        TupleType(
            2,
            (
                TupleType(3, (ListType(FLOAT), ListType(FLOAT), FLOAT)),
                TupleType(3, (ListType(FLOAT), ListType(FLOAT), FLOAT)),
            ),
        )
    )
    assert compilation.facts is not None
    observed_variants = [
        v for v in compilation.facts[step_fn].variants if v.id == "observed"
    ]
    assert len(observed_variants) == 1
    assert observed_variants[0].parameters[0] is FLOAT
    assert observed_variants[0].parameters[2] == step_fn.default_types[0]
    assert observed_variants[0].parameters[3] == step_fn.default_types[1]


def test_higher_order_callable_and_tuple_element_specialization():
    source = (
        "def eval_A(i, j):\n"
        " return 1.0 / ((i + j) * (i + j + 1) // 2 + i + 1)\n"
        "def part_A_times_u(i_u):\n"
        " i, u = i_u\n"
        " partial_sum = 0\n"
        " for j, u_j in enumerate(u):\n"
        "  partial_sum += eval_A(i, j) * u_j\n"
        " return partial_sum\n"
        "def part_At_times_u(i_u):\n"
        " i, u = i_u\n"
        " partial_sum = 0\n"
        " for j, u_j in enumerate(u):\n"
        "  partial_sum += eval_A(j, i) * u_j\n"
        " return partial_sum\n"
        "def eval_times_u(func, u):\n"
        " return [func((i, u)) for i in range(len(list(u)))]\n"
        "def eval_AtA_times_u(u):\n"
        " return eval_times_u(part_At_times_u, eval_times_u(part_A_times_u, u))\n"
        "def bench(n):\n"
        " u = [1] * n\n"
        " for _ in range(3):\n"
        "  v = eval_AtA_times_u(u)\n"
        "  u = eval_AtA_times_u(v)\n"
        " return u\n"
        "bench(4)\n"
    )
    compilation = compile_source(source)
    assert compilation.facts is not None
    by_name = {lam.source_name: lam for lam in compilation.lambdas}
    eval_times_variants = compilation.facts[by_name["eval_times_u"]].variants
    assert len(eval_times_variants) == 2
    for fn_name in ("part_A_times_u", "part_At_times_u"):
        variants = compilation.facts[by_name[fn_name]].variants
        assert len(variants) == 1
        param = variants[0].parameters[0]
        assert param.arity == 2
        assert param.elements is not None
        assert param.elements[1].has_float_specialization()
