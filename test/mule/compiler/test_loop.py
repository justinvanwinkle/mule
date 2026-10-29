"""Dominators, natural loops, and IR1 loop-invariant code motion (LICM)."""

from mule import ast
from mule.compiler import fndb
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.driver import build, emit_llvm
from mule.compiler.main import compile_source
from mule.compiler.node import (
    CallKind,
    Combination,
    GlobalVar,
    NaturalLoop,
    Ref,
)


def _function_by_name(compilation, name: str):
    for lam in compilation.lambdas:
        if lam.source_name == name:
            return lam
    raise KeyError(name)


def _known_calls_in_blocks(blocks, info):
    return [
        node
        for block in blocks
        for node in block.nodes()
        if isinstance(node, Combination)
        and node.kind is CallKind.KNOWN
        and node.fun_info is info
    ]


def test_licm_hoists_nested_arithmetic_and_cses_duplicates():
    compilation = compile_source(
        "def compute(a: int, b: int, n: int) -> int:\n"
        " i = 0\n"
        " total = 0\n"
        " while i < n:\n"
        "  total += (a * b + 3) + (a * b)\n"
        "  i += 1\n"
        " return total\n"
    )
    check_ir1_consistency(compilation.components)
    fn = _function_by_name(compilation, "compute")
    outer = fn.component.outer_loop
    assert len(outer.inferiors) == 1
    loop = outer.inferiors[0]
    assert isinstance(loop, NaturalLoop)
    loop_blocks = loop.all_blocks()
    non_loop_blocks = [b for b in fn.component.blocks() if b not in loop_blocks]

    mul_info = fndb.BINARY[ast.BinaryOperator.MULTIPLY]
    # a * b is hoisted to the preheader and commoned across both uses.
    assert _known_calls_in_blocks(loop_blocks, mul_info) == []
    assert len(_known_calls_in_blocks(non_loop_blocks, mul_info)) == 1


def test_licm_hoists_across_nested_loops_to_outermost_valid_preheader():
    compilation = compile_source(
        "def nested(a: int, b: int, m: int, n: int) -> int:\n"
        " i = 0\n"
        " acc = 0\n"
        " while i < m:\n"
        "  j = 0\n"
        "  while j < n:\n"
        "   acc += (a * b) + (i * 5) + j\n"
        "   j += 1\n"
        "  i += 1\n"
        " return acc\n"
    )
    check_ir1_consistency(compilation.components)
    fn = _function_by_name(compilation, "nested")
    outer = fn.component.outer_loop
    assert len(outer.inferiors) == 1
    outer_loop = outer.inferiors[0]
    assert len(outer_loop.inferiors) == 1
    inner_loop = outer_loop.inferiors[0]

    inner_blocks = inner_loop.all_blocks()
    outer_only_blocks = outer_loop.all_blocks() - inner_blocks
    preheader_blocks = [
        b for b in fn.component.blocks() if b not in outer_loop.all_blocks()
    ]

    mul_info = fndb.BINARY[ast.BinaryOperator.MULTIPLY]
    # Neither (a * b) nor (i * 5) remains in the inner loop:
    # (a * b) is hoisted to the outer preheader, (i * 5) to the inner preheader.
    assert _known_calls_in_blocks(inner_blocks, mul_info) == []
    assert len(_known_calls_in_blocks(outer_only_blocks, mul_info)) == 1
    assert len(_known_calls_in_blocks(preheader_blocks, mul_info)) == 1


def test_licm_hoists_out_of_generic_for_loop_preheader():
    compilation = compile_source(
        "def sum_scaled(items, a: int, b: int) -> int:\n"
        " total = 0\n"
        " for x in items:\n"
        "  if isinstance(x, int):\n"
        "   total += x + (a * b)\n"
        " return total\n"
    )
    check_ir1_consistency(compilation.components)
    fn = _function_by_name(compilation, "sum_scaled")
    outer = fn.component.outer_loop
    assert len(outer.inferiors) == 1
    loop = outer.inferiors[0]
    loop_blocks = loop.all_blocks()
    preheader_blocks = [b for b in fn.component.blocks() if b not in loop_blocks]

    mul_info = fndb.BINARY[ast.BinaryOperator.MULTIPLY]
    assert _known_calls_in_blocks(loop_blocks, mul_info) == []
    assert len(_known_calls_in_blocks(preheader_blocks, mul_info)) == 1


def test_licm_hoists_constant_global_and_sequence_length():
    compilation = compile_source(
        "_FACTOR = 6\n"
        "def scan_text(s: str) -> int:\n"
        " if isinstance(s, str):\n"
        "  i = 0\n"
        "  total = 0\n"
        "  while i < len(s):\n"
        "   total += _FACTOR * 7\n"
        "   i += 1\n"
        "  return total\n"
        " return 0\n"
    )
    check_ir1_consistency(compilation.components)
    fn = _function_by_name(compilation, "scan_text")
    loop = fn.component.outer_loop.inferiors[0]
    loop_blocks = loop.all_blocks()

    # Neither _FACTOR global load, _FACTOR * 7, nor _text_length(s) remains inside the loop.
    loop_global_refs = [
        node
        for block in loop_blocks
        for node in block.nodes()
        if isinstance(node, Ref) and isinstance(node.leaf, GlobalVar)
    ]
    assert loop_global_refs == []
    mul_info = fndb.BINARY[ast.BinaryOperator.MULTIPLY]
    assert _known_calls_in_blocks(loop_blocks, mul_info) == []
    text_len_calls = [
        node
        for block in loop_blocks
        for node in block.nodes()
        if isinstance(node, Combination)
        and node.fun_info is not None
        and node.fun_info.name == "_text_length"
    ]
    assert text_len_calls == []


def test_licm_guards_division_shift_and_loop_narrowed_operands():
    compilation = compile_source(
        "def unsafe_div_and_branch(a: int, b: int, x, n: int) -> int:\n"
        " if isinstance(a, int) and isinstance(b, int) and isinstance(n, int):\n"
        "  i = 0\n"
        "  total = 0\n"
        "  while i < n:\n"
        "   total += (a // b) + (a << b)\n"
        "   if isinstance(x, int):\n"
        "    total += x * 3\n"
        "   i += 1\n"
        "  return total\n"
        " return 0\n"
        "def safe_div_and_shift(a: int, b: int, n: int) -> int:\n"
        " if isinstance(a, int) and isinstance(b, int) and isinstance(n, int):\n"
        "  if 1 <= b < 16:\n"
        "   i = 0\n"
        "   total = 0\n"
        "   while i < n:\n"
        "    total += (a // b) + (a << b)\n"
        "    i += 1\n"
        "   return total\n"
        " return 0\n"
    )
    check_ir1_consistency(compilation.components)
    unsafe_fn = _function_by_name(compilation, "unsafe_div_and_branch")
    unsafe_loop_blocks = unsafe_fn.component.outer_loop.inferiors[0].all_blocks()
    fdiv_info = fndb.BINARY[ast.BinaryOperator.FLOOR_DIVIDE]
    lshift_info = fndb.BINARY[ast.BinaryOperator.LEFT_SHIFT]
    mul_info = fndb.BINARY[ast.BinaryOperator.MULTIPLY]
    assert len(_known_calls_in_blocks(unsafe_loop_blocks, fdiv_info)) == 1
    assert len(_known_calls_in_blocks(unsafe_loop_blocks, lshift_info)) == 1
    assert len(_known_calls_in_blocks(unsafe_loop_blocks, mul_info)) == 1

    safe_fn = _function_by_name(compilation, "safe_div_and_shift")
    safe_loop_blocks = safe_fn.component.outer_loop.inferiors[0].all_blocks()
    assert _known_calls_in_blocks(safe_loop_blocks, fdiv_info) == []
    assert _known_calls_in_blocks(safe_loop_blocks, lshift_info) == []


def test_licm_llvm_codegen_hoists_invariant_multiplies_before_loop():
    ir = emit_llvm(
        "def kernel(a: int, b: int, c: int, d: int, n: int) -> int:\n"
        " if -1000 <= a <= 1000 and -1000 <= b <= 1000 and -1000 <= c <= 1000 and -1000 <= d <= 1000 and 0 <= n <= 1000:\n"
        "  i = 0\n"
        "  s = 0\n"
        "  while i < n:\n"
        "   s += (a * b) + (c * d)\n"
        "   i += 1\n"
        "  return s\n"
        " return 0\n"
        "kernel(2, 3, 4, 5, 10)\n"
        "kernel(3, 5, 6, 7, 12)\n"
    )
    body = ir.split('define internal ptr @"mule_fn_kernel.numeric.observed"(')[1].split(
        "\n}"
    )[0]
    # Both multiplications are hoisted into the preheader before the loop header phi/branch;
    # no multiplication or runtime call remains inside the loop body.
    assert body.count("mul i64") == 2
    assert "@mule_int_mul(" not in body


def test_licm_and_loop_constraints_end_to_end_execution(tmp_path):
    source = (
        "_SCALE = 5\n"
        "def run_loops(a: int, b: int, s: str, t: tuple[int, int, int, int]) -> int:\n"
        " i = 0\n"
        " total = 0\n"
        " while i < len(s):\n"
        "  total += (a * b + _SCALE) + (a * b)\n"
        "  i += 1\n"
        " k = 0\n"
        " while k < 3:\n"
        "  total += t[k] + t[k + 1]\n"
        "  k += 1\n"
        " return total + i + k\n"
        "print(run_loops(4, 7, 'abcd', (10, 20, 30, 40)))\n"
        "print(run_loops(9, -2, '', (1, 2, 3, 4)))\n"
    )
    src_file = tmp_path / "loop_parity.py"
    src_file.write_text(source)
    exe = tmp_path / "loop_parity"
    build(src_file, exe, workers=1)
    import subprocess

    proc = subprocess.run([str(exe)], check=True, capture_output=True, text=True)
    assert proc.stdout.splitlines() == ["401", "18"]
