"""Compiler IR lowering to LLVM-style textual assembly and structured AST source
code transpilation with scope symbol resolution and source-map emission.
"""

import sys
import time
from typing import Any

DEFAULT_SIZE = 2000
DEFAULT_LOOPS = 1

_GLOBAL_SYMS = (
    "mp_rt_alloc_box",
    "mp_rt_dict_lookup",
    "mp_rt_str_concat",
    "mp_rt_check_bounds",
    "mp_rt_gc_safepoint",
    "mp_rt_raise_exc",
)

_STR_LITERALS = (
    "IndexError: list index out of range",
    "KeyError: missing dictionary key",
    "TypeError: unsupported operand types",
    "ValueError: invalid literal for int()",
    "RuntimeError: maximum recursion depth exceeded",
    "ZeroDivisionError: division by zero",
)

_IMPORT_CANDIDATES = (
    ("runtime.memory", "alloc_frame"),
    ("runtime.boxes", "box_int"),
    ("runtime.boxes", "unbox_int"),
    ("runtime.Posted", "dispatch_call"),
    ("runtime.exceptions", "raise_type_error"),
    ("runtime.collections", "dict_get_fast"),
)

_RESERVED_WORDS = frozenset({"int", "float", "return", "while", "if", "else", "void", "char", "struct", "default"})


class IRInstruction:
    def __init__(
        self,
        opcode: str,
        ty: str,
        dest_id: int,
        operands: list[tuple[str, Any]],
        orig_line: int,
    ) -> None:
        self.opcode = opcode
        self.ty = ty
        self.dest_id = dest_id
        self.operands = operands
        self.orig_line = orig_line


class IRPhiNode:
    def __init__(self, ty: str, dest_id: int, incoming: list[tuple[int, int]]) -> None:
        self.ty = ty
        self.dest_id = dest_id
        # list of (pred_block_index, source_reg_id)
        self.incoming = incoming


class IRBasicBlock:
    def __init__(
        self,
        block_id: int,
        label_hint: str,
        preds: list[int],
        phis: list[IRPhiNode],
        instrs: list[IRInstruction],
        terminator: tuple[str, list[int], int],
    ) -> None:
        self.block_id = block_id
        self.label_hint = label_hint
        self.preds = preds
        self.phis = phis
        self.instrs = instrs
        self.terminator = terminator


class IRFunction:
    def __init__(
        self,
        name: str,
        ret_type: str,
        params: list[tuple[str, int, str]],
        blocks: list[IRBasicBlock],
        imports: list[tuple[str, str]],
        orig_line: int,
    ) -> None:
        self.name = name
        self.ret_type = ret_type
        self.params = params
        self.blocks = blocks
        self.imports = imports
        self.orig_line = orig_line


def prepare(size: int) -> list[IRFunction]:
    functions: list[IRFunction] = []
    state = 55192837

    for fn_idx in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        fn_name = f"mule_fn_{fn_idx}_{state & 0xFF:02x}"
        params = [
            ("ptr", 0, "ctx_ptr"),
            ("i64", 1, "arg_count"),
            ("ptr", 2, "default"),
        ]
        base_line = 100 + fn_idx * 25

        # Create a 4-block diamond + loop CFG:
        # B0 (entry) -> B1 (then), B2 (else); B1, B2 -> B3 (merge)
        b0_instrs = [
            IRInstruction(
                "add",
                "i64",
                3,
                [("reg", 1), ("int", (state & 0x3F) + 1)],
                base_line + 1,
            ),
            IRInstruction(
                "str_const",
                "ptr",
                4,
                [("str", _STR_LITERALS[state % len(_STR_LITERALS)])],
                base_line + 2,
            ),
            IRInstruction(
                "call",
                "ptr",
                5,
                [
                    ("sym", _GLOBAL_SYMS[(state >> 3) % len(_GLOBAL_SYMS)]),
                    ("reg", 0),
                    ("reg", 3),
                    ("reg", 4),
                ],
                base_line + 3,
            ),
            IRInstruction(
                "icmp_slt",
                "i1",
                6,
                [("reg", 3), ("int", 64)],
                base_line + 4,
            ),
        ]
        b0 = IRBasicBlock(
            block_id=0,
            label_hint="entry",
            preds=[],
            phis=[],
            instrs=b0_instrs,
            terminator=("cond_br", [1, 2], 6),
        )

        b1_instrs = [
            IRInstruction(
                "mul",
                "i64",
                7,
                [("reg", 3), ("int", 3)],
                base_line + 7,
            ),
            IRInstruction(
                "fadd",
                "double",
                8,
                [("float", 1.5 + (fn_idx % 7) * 0.25), ("float", 0.75)],
                base_line + 8,
            ),
            IRInstruction(
                "call",
                "ptr",
                9,
                [
                    ("sym", _GLOBAL_SYMS[(state >> 6) % len(_GLOBAL_SYMS)]),
                    ("reg", 5),
                    ("reg", 7),
                ],
                base_line + 9,
            ),
        ]
        b1 = IRBasicBlock(
            block_id=1,
            label_hint="bb.then",
            preds=[0],
            phis=[],
            instrs=b1_instrs,
            terminator=("br", [3], -1),
        )

        b2_instrs = [
            IRInstruction(
                "sub",
                "i64",
                10,
                [("reg", 3), ("int", 9)],
                base_line + 12,
            ),
            IRInstruction(
                "str_const",
                "ptr",
                11,
                [("str", _STR_LITERALS[(state >> 9) % len(_STR_LITERALS)])],
                base_line + 13,
            ),
            IRInstruction(
                "call",
                "ptr",
                12,
                [
                    ("sym", _GLOBAL_SYMS[(state >> 12) % len(_GLOBAL_SYMS)]),
                    ("reg", 0),
                    ("reg", 11),
                ],
                base_line + 14,
            ),
        ]
        b2 = IRBasicBlock(
            block_id=2,
            label_hint="bb.else",
            preds=[0],
            phis=[],
            instrs=b2_instrs,
            terminator=("br", [3], -1),
        )

        b3_phis = [
            IRPhiNode("i64", 13, [(1, 7), (2, 10)]),
            IRPhiNode("ptr", 14, [(1, 9), (2, 12)]),
        ]
        b3_instrs = [
            IRInstruction(
                "add",
                "i64",
                15,
                [("reg", 13), ("reg", 1)],
                base_line + 18,
            ),
            IRInstruction(
                "call",
                "ptr",
                16,
                [
                    ("sym", _GLOBAL_SYMS[(state >> 15) % len(_GLOBAL_SYMS)]),
                    ("reg", 14),
                    ("reg", 15),
                ],
                base_line + 19,
            ),
        ]
        b3 = IRBasicBlock(
            block_id=3,
            label_hint="bb.merge",
            preds=[1, 2],
            phis=b3_phis,
            instrs=b3_instrs,
            terminator=("ret", [], 16),
        )

        imp1 = _IMPORT_CANDIDATES[state % len(_IMPORT_CANDIDATES)]
        imp2 = _IMPORT_CANDIDATES[(state >> 4) % len(_IMPORT_CANDIDATES)]
        imp3 = _IMPORT_CANDIDATES[(state >> 8) % len(_IMPORT_CANDIDATES)]

        functions.append(
            IRFunction(
                name=fn_name,
                ret_type="ptr",
                params=params,
                blocks=[b0, b1, b2, b3],
                imports=[imp1, imp2, imp3],
                orig_line=base_line,
            )
        )

    return functions


def _format_operand(
    kind: str,
    val: Any,
    reg_map: dict[int, str],
    str_pool: dict[str, str],
) -> str:
    if kind == "reg":
        return reg_map[val]
    if kind == "int":
        return f"i64 {val}"
    if kind == "float":
        return f"double {val:.4f}"
    if kind == "sym":
        return f"@{val}"
    if kind == "str":
        sym = str_pool.get(val)
        if sym is None:
            sym = f"@.str.{len(str_pool)}"
            str_pool[val] = sym
        return f"ptr {sym}"
    return str(val)


def scenario_llvm_style_ir_emitter(functions: list[IRFunction]) -> tuple[int, int, int, int]:
    """Lower IR modules into textual LLVM-style assembly with SSA renaming and PHI patching."""
    total_ir_chars = 0
    ssa_Counter_sum = 0
    phi_edges_patched = 0
    pooled_strings = 0

    for _ in range(580):
        str_pool: dict[str, str] = {}
        module_lines: list[str] = []
        reg_counter = 0

        for fn in functions:
            reg_map: dict[int, str] = {}
            param_parts: list[str] = []
            for p_ty, p_id, _p_name in fn.params:
                r_name = f"%v.{reg_counter}"
                reg_counter += 1
                reg_map[p_id] = r_name
                param_parts.append(f"{p_ty} {r_name}")

            # Pre-assign block labels and allocate destination registers so forward/PHI refs resolve
            block_labels: dict[int, str] = {}
            for blk in fn.blocks:
                block_labels[blk.block_id] = f"{blk.label_hint}.{blk.block_id}"
                for phi in blk.phis:
                    reg_map[phi.dest_id] = f"%v.{reg_counter}"
                    reg_counter += 1
                for ins in blk.instrs:
                    reg_map[ins.dest_id] = f"%v.{reg_counter}"
                    reg_counter += 1

            module_lines.append(f"define {fn.ret_type} @{fn.name}({', '.join(param_parts)}) {{")

            for blk in fn.blocks:
                b_label = block_labels[blk.block_id]
                if blk.preds:
                    pred_comment = ", ".join(f"%{block_labels[p]}" for p in blk.preds)
                    module_lines.append(f"{b_label}: ; preds = {pred_comment}")
                else:
                    module_lines.append(f"{b_label}:")

                for phi in blk.phis:
                    res = reg_map[phi.dest_id]
                    inc_strs: list[str] = []
                    for pred_id, src_reg_id in phi.incoming:
                        pred_lbl = block_labels[pred_id]
                        src_ssa = reg_map[src_reg_id]
                        inc_strs.append(f"[ {src_ssa}, %{pred_lbl} ]")
                        phi_edges_patched += 1
                    module_lines.append(f"  {res} = phi {phi.ty} {', '.join(inc_strs)}")

                for ins in blk.instrs:
                    res = reg_map[ins.dest_id]
                    op = ins.opcode
                    if op == "call":
                        fn_target = ins.operands[0][1]
                        args = [
                            _format_operand(k, v, reg_map, str_pool)
                            for k, v in ins.operands[1:]
                        ]
                        module_lines.append(f"  {res} = call {ins.ty} @{fn_target}({', '.join(args)})")
                    elif op == "str_const":
                        lit_op = _format_operand(ins.operands[0][0], ins.operands[0][1], reg_map, str_pool)
                        module_lines.append(f"  {res} = getelementptr inbounds i8, {lit_op}, i64 0")
                    else:
                        lhs = _format_operand(ins.operands[0][0], ins.operands[0][1], reg_map, str_pool)
                        rhs = _format_operand(ins.operands[1][0], ins.operands[1][1], reg_map, str_pool)
                        module_lines.append(f"  {res} = {op} {ins.ty} {lhs}, {rhs}")

                t_kind, t_targets, t_reg = blk.terminator
                if t_kind == "cond_br":
                    cond_ssa = reg_map[t_reg]
                    t_lbl = block_labels[t_targets[0]]
                    f_lbl = block_labels[t_targets[1]]
                    module_lines.append(
                        f"  br i1 {cond_ssa}, label %{t_lbl}, label %{f_lbl}"
                    )
                elif t_kind == "br":
                    dst_lbl = block_labels[t_targets[0]]
                    module_lines.append(f"  br label %{dst_lbl}")
                elif t_kind == "ret":
                    ret_ssa = reg_map[t_reg]
                    module_lines.append(f"  ret {fn.ret_type} {ret_ssa}")

            module_lines.append("}")

        header_Globals: list[str] = []
        for s_lit, s_sym in str_pool.items():
            header_Globals.append(f'{s_sym} = private unnamed_addr constant [{len(s_lit) + 1} x i8] c"{s_lit}\\00"')

        full_ir = "\n".join(header_Globals) + "\n" + "\n".join(module_lines) + "\n"
        total_ir_chars += len(full_ir)
        ssa_Counter_sum += reg_counter
        pooled_strings += len(str_pool)

    return total_ir_chars, ssa_Counter_sum, phi_edges_patched, pooled_strings


def _mangle_ident(raw_name: str, scope_depth: int) -> str:
    cleaned = raw_name.replace(".", "_")
    if cleaned in _RESERVED_WORDS:
        return f"_m_kw_{cleaned}_s{scope_depth}"
    return f"_m_{cleaned}_s{scope_depth}"


def scenario_source_code_transpiler(functions: list[IRFunction]) -> tuple[int, int, int, int]:
    """Transpile IR/AST into indented source code with symbol mangling, import sorting, and source maps."""
    emitted_lines_count = 0
    mangled_symbol_chars = 0
    source_map_checksum = 0
    dedup_imports_total = 0

    for _ in range(580):
        import_table: dict[str, set[str]] = {}
        body_lines: list[str] = []
        source_map: list[tuple[int, int, str]] = []
        current_gen_line = 1

        for fn in functions:
            for mod_name, sym_name in fn.imports:
                bucket = import_table.get(mod_name)
                if bucket is None:
                    bucket = set()
                    import_table[mod_name] = bucket
                bucket.add(sym_name)

            outer_scope: dict[int, str] = {}
            param_decls: list[str] = []
            for _p_ty, p_id, p_raw in fn.params:
                mangled = _mangle_ident(p_raw, 1)
                outer_scope[p_id] = mangled
                mangled_symbol_chars += len(mangled)
                param_decls.append(mangled)

            fn_mangled = _mangle_ident(fn.name, 0)
            body_lines.append(f"def {fn_mangled}({', '.join(param_decls)}):")
            source_map.append((current_gen_line, fn.orig_line, fn_mangled))
            current_gen_line += 1

            # Populate block-scoped symbols and emit indented statements
            for blk in fn.blocks:
                indent_level = 1 if blk.block_id in (0, 3) else 2
                pad = "    " * indent_level
                if blk.block_id == 1:
                    cond_sym = outer_scope.get(6, "_cond")
                    body_lines.append(f"    if {cond_sym}:")
                    current_gen_line += 1
                elif blk.block_id == 2:
                    body_lines.append("    else:")
                    current_gen_line += 1

                for phi in blk.phis:
                    phi_sym = _mangle_ident(f"phi_{phi.dest_id}", indent_level)
                    outer_scope[phi.dest_id] = phi_sym
                    mangled_symbol_chars += len(phi_sym)

                for ins in blk.instrs:
                    lhs_sym = _mangle_ident(f"tmp_{ins.dest_id}", indent_level)
                    outer_scope[ins.dest_id] = lhs_sym
                    mangled_symbol_chars += len(lhs_sym)

                    op = ins.opcode
                    if op == "call":
                        callee = ins.operands[0][1]
                        call_args: list[str] = []
                        for k, v in ins.operands[1:]:
                            if k == "reg":
                                call_args.append(outer_scope.get(v, f"_u{v}"))
                            else:
                                call_args.append(str(v))
                        stmt = f"{pad}{lhs_sym} = {callee}({', '.join(call_args)})"
                    elif op == "str_const":
                        stmt = f'{pad}{lhs_sym} = "{ins.operands[0][1]}"'
                    else:
                        a_k, a_v = ins.operands[0]
                        b_k, b_v = ins.operands[1]
                        a_str = outer_scope.get(a_v, f"_u{a_v}") if a_k == "reg" else str(a_v)
                        b_str = outer_scope.get(b_v, f"_u{b_v}") if b_k == "reg" else str(b_v)
                        stmt = f"{pad}{lhs_sym} = ({a_str} /*{op}*/ {b_str})"

                    body_lines.append(stmt)
                    source_map.append((current_gen_line, ins.orig_line, lhs_sym))
                    current_gen_line += 1

                t_kind, _t_targets, t_reg = blk.terminator
                if t_kind == "ret":
                    ret_sym = outer_scope.get(t_reg, "_ret")
                    body_lines.append(f"{pad}return {ret_sym}")
                    current_gen_line += 1

        # Deduplicate and sort import table
        sorted_import_lines: list[str] = []
        for mod_name in sorted(import_table):
            syms = sorted(import_table[mod_name])
            dedup_imports_total += len(syms)
            sorted_import_lines.append(f"from {mod_name} import {', '.join(syms)}")

        import_offset = len(sorted_import_lines)
        for gen_ln, orig_ln, sym in source_map:
            adj_gen = gen_ln + import_offset
            source_map_checksum += ((adj_gen * 31) ^ orig_ln) + len(sym)

        emitted_lines_count += import_offset + len(body_lines)

    return emitted_lines_count, mangled_symbol_chars, source_map_checksum, dedup_imports_total


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
    functions = prepare(size)
    prep_s = time.perf_counter() - t0

    expected = None
    llvm_s = 0.0
    trans_s = 0.0
    for _ in range(loops):
        r1 = ()
        r2 = ()
        if scenario in ("all", "llvm_style_ir_emitter"):
            s0 = time.perf_counter()
            r1 = scenario_llvm_style_ir_emitter(functions)
            llvm_s += time.perf_counter() - s0
        if scenario in ("all", "source_code_transpiler"):
            s1 = time.perf_counter()
            r2 = scenario_source_code_transpiler(functions)
            trans_s += time.perf_counter() - s1
        combined = r1 + r2
        if expected is not None and combined != expected:
            raise AssertionError("Result mismatch across loops")
        expected = combined

    total_s = (llvm_s + trans_s) / loops
    print("topic\tcodegen/ir_and_template_emission")
    print(f"size\t{size}")
    print(f"loops\t{loops}")
    print(f"result\t{expected}")
    print(f"prepare\t{prep_s:.6f}")
    if scenario in ("all", "llvm_style_ir_emitter"):
        print(f"llvm_style_ir_emitter\t{llvm_s / loops:.6f}")
    if scenario in ("all", "source_code_transpiler"):
        print(f"source_code_transpiler\t{trans_s / loops:.6f}")
    print(f"total\t{total_s:.6f}")


if __name__ == "__main__":
    main()
