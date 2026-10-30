"""Linear induction, interval/relational comparison math, and IR1 node visitor for constraint propagation."""

from __future__ import annotations

from mule import ast

from . import fndb
from .annotation_types import InferredReturnContract
from .constraint_domain import (
    _DEAD_EDGE,
    _GLOBAL,
    _GLOBAL_CALLEE_NAME,
    _IMPORTED_CALLEE_NAME,
    _IS_BINARY_ADD_NODE,
    _IS_BUILTIN_SEQUENCE_TYPE,
    _IS_SIMPLE_SEQUENCE_TYPE,
    _LITERAL_SEQ_LENGTH,
    _SEQ_LEN_BOUND_TYPE,
    _SINGLE_ASSIGNED_LAMBDA_VAR,
    NO_FACT,
    UNBOUND_FACT,
    UNKNOWN_FACT,
    Fact,
    _fact,
    _InferredReturnDestination,
    _join_relations_list,
    _key,
    _tighten_relation,
    _UnreassignedRefVar,
    join_fact,
)
from .ctype import (
    BOOLEAN,
    BOOLEAN_FALSE,
    BOOLEAN_TRUE,
    BOTTOM,
    DICT_TYPE,
    EMPTY_DICT_TYPE,
    EMPTY_SET_TYPE,
    FLOAT,
    INTEGER,
    NONE_TYPE,
    PYTHON_INTEGER,
    SET_TYPE,
    TEXT_TYPE,
    UNKNOWN,
    FunctionType,
    IntegerType,
    TextType,
    TupleType,
    ValueType,
    ValueTypeVisitor,
)
from .fndb import INTERNED_TEXTS, SMALL_INTS, constant_type
from .knownfun import defconstrain, defpropagate
from .node import (
    CallKind,
    Cast,
    Combination,
    FunctionalKind,
    GlobalVar,
    If,
    LambdaVar,
    LeafVisitor,
    NodeVisitor,
    Ref,
    Set,
    linked,
)


class _LinearUpdate(NodeVisitor):
    """Extract ``(src_var, d_min, d_max)`` from ``src_var`` or ``src_var +/- d``."""

    def __init__(self, visitor: _Propagation):
        self.visitor = visitor

    def _origin_var(self, lvar):
        src_var = self.visitor.origins.get(lvar)
        if src_var is not None:
            return src_var
        if len(lvar.uses) == 1:
            var = lvar.uses[0].accept(
                _UnreassignedRefVar(self.visitor.walk.graph.headers)
            )
            if var is not None and var in self.visitor.walk.local_vars_set:
                return var
        return None

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        if node.lvar is None:
            return None
        src_var = self._origin_var(node.lvar)
        if src_var is not None and src_var in self.visitor.walk.local_vars_set:
            t = self.visitor.type_of(node.lvar)
            if t.fits(PYTHON_INTEGER) and not t.may_be_unbound:
                return src_var, 0, 0
        return None

    def visit_Cast(self, node: Cast):
        if node.value is not None and len(node.value.uses) == 1:
            return node.value.uses[0].accept(self)
        return None

    def _extract_seq_len_linear(self, seq_lvar):
        src_var = self._origin_var(seq_lvar)
        if src_var is not None and self.visitor.has_vector_length(seq_lvar):
            src_var = self.visitor.walk.temp_source.get(src_var, src_var)
            seq_key = self.visitor.walk.seq_len_keys.get(src_var)
            if seq_key is not None:
                return seq_key, 0, 0
        if len(seq_lvar.uses) == 1:
            _, linear = seq_lvar.uses[0].accept(_SequenceProducerLength(self.visitor))
            if linear is not None:
                return linear
        return None

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN:
            if self.visitor.is_predictable_len(node):
                return self._extract_seq_len_linear(node.args[0])
            return None
        info = node.fun_info
        if (
            info is not None
            and info.name in ("_text_length", "_storage_length", "len")
            and len(node.args) == 1
        ):
            return self._extract_seq_len_linear(node.args[0])
        if info is fndb.RANGE_ADVANCE:
            state = node.info
            if state is not None:
                r_key = self.visitor.walk.range_stop_keys.get(state)
                if r_key is not None and r_key in self.visitor.state:
                    r_fact = self.visitor.state[r_key]
                    if not r_fact.type.may_be_unbound:
                        return r_key, None, -1
            if state is None or not (1 <= len(state.arguments) <= 3):
                return None
            if len(state.arguments) == 1:
                stop_lvar = state.arguments[0]
                positive_step = True
                negative_step = False
            else:
                stop_lvar = state.arguments[1]
                step_t = (
                    self.visitor.type_of(state.arguments[2])
                    if len(state.arguments) == 3
                    else SMALL_INTS[1]
                )
                positive_step = step_t.minimum is not None and step_t.minimum > 0
                negative_step = step_t.maximum is not None and step_t.maximum < 0
            if not (positive_step or negative_step) or len(stop_lvar.uses) != 1:
                return None
            stop_linear = stop_lvar.uses[0].accept(self)
            if stop_linear is None:
                return None
            bvar, bmin, bmax = stop_linear
            if positive_step:
                return bvar, None, (bmax - 1 if bmax is not None else None)
            return bvar, (bmin + 1 if bmin is not None else None), None
        if len(node.args) != 2:
            return None
        arg0, arg1 = node.args
        t0 = self.visitor.type_of(arg0)
        t1 = self.visitor.type_of(arg1)
        if not (
            t0.fits(PYTHON_INTEGER)
            and not t0.may_be_unbound
            and t1.fits(PYTHON_INTEGER)
            and not t1.may_be_unbound
        ):
            return None
        if (
            (
                info is fndb.BINARY[ast.BinaryOperator.RIGHT_SHIFT]
                and t1.exact_integer() == 1
            )
            or (
                info is fndb.BINARY[ast.BinaryOperator.FLOOR_DIVIDE]
                and t1.exact_integer() == 2
            )
        ) and len(arg0.uses) == 1:
            sum_node = arg0.uses[0]
            if (
                sum_node.accept(_IS_BINARY_ADD_NODE)
                and len(sum_node.args) == 2
                and self.visitor.relations
            ):
                term_a = _extract_linear_term(self.visitor, sum_node.args[0])
                term_b = _extract_linear_term(self.visitor, sum_node.args[1])
                if term_a is not None and term_b is not None:
                    u_var, c0 = term_a
                    v_var, c1 = term_b
                    uv = (
                        0
                        if u_var is v_var
                        else self.visitor.relations.get((u_var, v_var))
                    )
                    if uv is not None:
                        return v_var, None, (uv + c0 + c1) // 2
                    vu = self.visitor.relations.get((v_var, u_var))
                    if vu is not None:
                        return u_var, None, (vu + c0 + c1) // 2
        if info in (
            fndb.BINARY[ast.BinaryOperator.ADD],
            fndb.INPLACE_BINARY[ast.BinaryOperator.ADD],
        ):
            src0 = self._origin_var(arg0)
            if src0 is not None and src0 in self.visitor.walk.local_vars_set:
                return src0, t1.minimum, t1.maximum
            src1 = self._origin_var(arg1)
            if src1 is not None and src1 in self.visitor.walk.local_vars_set:
                return src1, t0.minimum, t0.maximum
            if len(arg0.uses) == 1:
                sub0 = arg0.uses[0].accept(self)
                if sub0 is not None:
                    bvar, bmin, bmax = sub0
                    d_min = (
                        bmin + t1.minimum
                        if bmin is not None and t1.minimum is not None
                        else None
                    )
                    d_max = (
                        bmax + t1.maximum
                        if bmax is not None and t1.maximum is not None
                        else None
                    )
                    return bvar, d_min, d_max
            if len(arg1.uses) == 1:
                sub1 = arg1.uses[0].accept(self)
                if sub1 is not None:
                    bvar, bmin, bmax = sub1
                    d_min = (
                        bmin + t0.minimum
                        if bmin is not None and t0.minimum is not None
                        else None
                    )
                    d_max = (
                        bmax + t0.maximum
                        if bmax is not None and t0.maximum is not None
                        else None
                    )
                    return bvar, d_min, d_max
        elif info in (
            fndb.BINARY[ast.BinaryOperator.SUBTRACT],
            fndb.INPLACE_BINARY[ast.BinaryOperator.SUBTRACT],
        ):
            src0 = self._origin_var(arg0)
            if src0 is not None and src0 in self.visitor.walk.local_vars_set:
                d_min = -t1.maximum if t1.maximum is not None else None
                d_max = -t1.minimum if t1.minimum is not None else None
                return src0, d_min, d_max
            if len(arg0.uses) == 1:
                sub0 = arg0.uses[0].accept(self)
                if sub0 is not None:
                    bvar, bmin, bmax = sub0
                    d_min = (
                        bmin - t1.maximum
                        if bmin is not None and t1.maximum is not None
                        else None
                    )
                    d_max = (
                        bmax - t1.minimum
                        if bmax is not None and t1.minimum is not None
                        else None
                    )
                    return bvar, d_min, d_max
        return None


class _SequenceProducerLength(NodeVisitor):
    """Derive a sequence length interval ``ValueType`` (and optional linear term
    ``(base_key, d_min, d_max)``) from a sequence-producing ``Combination``."""

    def __init__(self, visitor: _Propagation):
        self.visitor = visitor

    def visit_Node(self, node):
        return None, None

    def visit_Cast(self, node: Cast):
        if node.value is not None and len(node.value.uses) == 1:
            return node.value.uses[0].accept(self)
        return None, None

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN or node.fun_info is None:
            return None, None
        name = node.fun_info.name
        visitor = self.visitor
        if name == "%add" and len(node.args) == 2:
            left_len = visitor.sequence_length_type(node.args[0])
            right_len = visitor.sequence_length_type(node.args[1])
            if left_len is None or right_len is None:
                return None, None
            sum_type = left_len.binary(ast.BinaryOperator.ADD, right_len)
            if not sum_type.fits(INTEGER):
                return None, None
            if sum_type.minimum is None or sum_type.minimum < 0:
                sum_type = IntegerType(0, sum_type.maximum)
            if sum_type.maximum is None or sum_type.maximum >= (1 << 60):
                sum_type = IntegerType(sum_type.minimum, (1 << 60) - 1)
            linear = None
            src0 = visitor.origins.get(node.args[0])
            src1 = visitor.origins.get(node.args[1])
            if (
                src0 is not None
                and visitor.has_vector_length(node.args[0])
                and (k0 := visitor.walk.seq_len_keys.get(src0)) is not None
            ):
                linear = (k0, right_len.minimum, right_len.maximum)
            elif (
                src1 is not None
                and visitor.has_vector_length(node.args[1])
                and (k1 := visitor.walk.seq_len_keys.get(src1)) is not None
            ):
                linear = (k1, left_len.minimum, left_len.maximum)
            return sum_type, linear
        if name == "_text_slice" and len(node.args) == 3:
            start_t = visitor.type_of(node.args[1])
            stop_t = visitor.type_of(node.args[2])
            if not (start_t.fits(PYTHON_INTEGER) and stop_t.fits(PYTHON_INTEGER)):
                return None, None
            diff_t = stop_t.binary(ast.BinaryOperator.SUBTRACT, start_t)
            lo = max(0, diff_t.minimum) if diff_t.minimum is not None else 0
            hi = (
                min((1 << 60) - 1, max(0, diff_t.maximum))
                if diff_t.maximum is not None
                else (1 << 60) - 1
            )
            return IntegerType(lo, hi), None
        if name == "%getslice" and len(node.args) == 4:
            seq_lvar, start_lvar, stop_lvar, step_lvar = node.args
            step_t = visitor.type_of(step_lvar)
            if not (step_t.fits_none(NONE_TYPE) or step_t.exact_integer() == 1):
                return None, None
            seq_len_t = visitor.sequence_length_type(seq_lvar)
            if seq_len_t is None:
                return None, None
            start_t = visitor.type_of(start_lvar)
            stop_t = visitor.type_of(stop_lvar)
            start_zero = start_t.fits_none(NONE_TYPE) or start_t.exact_integer() == 0
            start_nonneg = start_zero or (
                start_t.fits(PYTHON_INTEGER)
                and start_t.minimum is not None
                and start_t.minimum >= 0
            )
            stop_none = stop_t.fits_none(NONE_TYPE)
            stop_nonneg = (
                not stop_none
                and stop_t.fits(PYTHON_INTEGER)
                and stop_t.minimum is not None
                and stop_t.minimum >= 0
            )
            if not start_nonneg or not (stop_none or stop_nonneg):
                return None, None
            clamped_stop = (
                seq_len_t if stop_none else fndb.derive_min2_types(seq_len_t, stop_t)
            )
            raw_len = (
                clamped_stop
                if start_zero
                else clamped_stop.binary(ast.BinaryOperator.SUBTRACT, start_t)
            )
            lo = max(0, raw_len.minimum) if raw_len.minimum is not None else 0
            hi = (
                min((1 << 60) - 1, max(0, raw_len.maximum))
                if raw_len.maximum is not None
                else (1 << 60) - 1
            )
            return IntegerType(lo, hi), None
        if name == "%_list_fill" and len(node.args) == 2:
            cnt_t = visitor.type_of(node.args[1])
            if not cnt_t.fits(PYTHON_INTEGER) or cnt_t.may_be_unbound:
                return None, None
            lo = max(0, cnt_t.minimum) if cnt_t.minimum is not None else 0
            hi = (
                min((1 << 60) - 1, max(0, cnt_t.maximum))
                if cnt_t.maximum is not None
                else (1 << 60) - 1
            )
            linear = None
            if cnt_t.minimum is not None and cnt_t.minimum >= 0:
                cnt_var = visitor.origins.get(node.args[1])
                if cnt_var is not None and cnt_var in visitor.walk.local_vars_set:
                    linear = (cnt_var, 0, 0)
                elif len(node.args[1].uses) == 1:
                    linear = node.args[1].uses[0].accept(_LinearUpdate(visitor))
            return IntegerType(lo, hi), linear
        if name == "%multiply" and len(node.args) == 2:
            t0 = visitor.type_of(node.args[0])
            t1 = visitor.type_of(node.args[1])
            if t0.accept(_IS_BUILTIN_SEQUENCE_TYPE) and t1.fits(PYTHON_INTEGER):
                seq_lvar, cnt_t = node.args[0], t1
            elif t1.accept(_IS_BUILTIN_SEQUENCE_TYPE) and t0.fits(PYTHON_INTEGER):
                seq_lvar, cnt_t = node.args[1], t0
            else:
                return None, None
            seq_len_t = visitor.sequence_length_type(seq_lvar)
            if seq_len_t is None or cnt_t.may_be_unbound:
                return None, None
            cnt_lo = max(0, cnt_t.minimum) if cnt_t.minimum is not None else 0
            cnt_hi = max(0, cnt_t.maximum) if cnt_t.maximum is not None else None
            prod_t = seq_len_t.binary(
                ast.BinaryOperator.MULTIPLY, IntegerType(cnt_lo, cnt_hi)
            )
            lo = max(0, prod_t.minimum) if prod_t.minimum is not None else 0
            hi = (
                min((1 << 60) - 1, max(0, prod_t.maximum))
                if prod_t.maximum is not None
                else (1 << 60) - 1
            )
            return IntegerType(lo, hi), None
        return None, None


class _Propagation(NodeVisitor, LeafVisitor):
    """Processes one block's nodes against the current variable state."""

    def __init__(self, walk, state, origins):
        self.walk = walk
        self.state = state
        self.origins = origins
        self.relations: dict = {}
        self.edge_relations: dict | None = None
        self.type_of_origins: dict[object, object] = {}
        self.tuple_list_origins: dict[object, TupleType] = {}
        self.handler_state: dict[object, Fact] | None = None
        self.handler_relations: dict | None = None
        self.has_handler = False
        # Keys assigned since the handler state last absorbed the state.
        self.changed = set()
        self.state_mutated = False
        self.globals_invalidated = False
        self._current_ref: Ref | None = None

    def reset(
        self,
        state: dict[object, Fact],
        has_handler: bool,
        relations: dict | None = None,
    ) -> None:
        self.state = state
        self.relations = relations.copy() if relations is not None else {}
        self.edge_relations = None
        self.origins.clear()
        self.type_of_origins.clear()
        self.tuple_list_origins.clear()
        self.changed.clear()
        self.state_mutated = False
        self.globals_invalidated = False
        self.has_handler = has_handler
        self.handler_state = None
        self.handler_relations = (
            relations.copy() if has_handler and relations is not None else None
        )

    def assign(self, key, fact):
        if self.state.get(key) is not fact:
            if not self.state_mutated:
                if self.has_handler:
                    self.handler_state = self.state.copy()
                self.state = self.state.copy()
                self.state_mutated = True
            self.state[key] = fact
            if self.has_handler:
                self.changed.add(key)
            if key in self.walk.invalidated:
                self.globals_invalidated = False

    def fact_of(self, lvar) -> Fact:
        return self.walk.facts.get(lvar.use(), NO_FACT)

    def lvar_fact(self, lvar) -> Fact:
        return self.fact_of(lvar)

    def type_of(self, lvar) -> ValueType:
        return self.fact_of(lvar).type

    def narrowed_fact(self, current: Fact, narrowed_type: ValueType) -> Fact:
        if current.target is None:
            return _fact(narrowed_type)
        return Fact(narrowed_type, current.target, current.proven)

    def apply_element_contract(
        self,
        node,
        static_type: ValueType,
        elem_contract,
        *,
        trusted_static: bool = True,
    ) -> ValueType:
        if elem_contract is None or elem_contract.contradicts(static_type):
            self.walk.element_contracts.pop(node, None)
            return static_type
        if node.lvar is not None and elem_contract.has_element_contracts():
            self.walk.lvar_contracts[node.lvar] = elem_contract
        checked = elem_contract.checked_type
        if checked is UNKNOWN or (
            trusted_static and static_type is not UNKNOWN and static_type.fits(checked)
        ):
            self.walk.element_contracts.pop(node, None)
            return static_type
        if self.walk.lambda_.is_protocol:
            return static_type
        if self.walk.lambda_.policy.safety == 0:
            # Trust the declared type without scheduling an element check.
            return elem_contract.refine(static_type)
        self.walk.element_contracts[node] = elem_contract
        return elem_contract.refine(static_type)

    def has_vector_length(self, seq_lvar) -> bool:
        """Whether a variable's vector-length facts describe ``seq_lvar``.

        Only a simple sequence's length is fixed by its creation; any other
        reference may resize a list or bytearray (see _IsSimpleSequenceType).
        """
        return self.type_of(seq_lvar).accept(_IS_SIMPLE_SEQUENCE_TYPE)

    def sequence_length_type(self, lvar) -> ValueType | None:
        seq_type = self.type_of(lvar)
        if seq_type is BOTTOM:
            return BOTTOM
        if seq_type.may_be_unbound:
            return None
        exact_len = seq_type.exact_length()
        if exact_len is not None:
            return IntegerType(exact_len, exact_len)
        if len(lvar.uses) == 1:
            lit_len = lvar.uses[0].accept(_LITERAL_SEQ_LENGTH)
            if lit_len is not None:
                return IntegerType(lit_len, lit_len)
        src_var = self.origins.get(lvar)
        if src_var is not None and self.has_vector_length(lvar):
            src_var = self.walk.temp_source.get(src_var, src_var)
            seq_key = self.walk.seq_len_keys.get(src_var)
            if seq_key is not None and seq_key in self.state:
                len_fact = self.state[seq_key]
                if not len_fact.type.may_be_unbound and len_fact.type.fits(INTEGER):
                    return len_fact.type
        if len(lvar.uses) == 1:
            prod_len, _ = lvar.uses[0].accept(_SequenceProducerLength(self))
            if prod_len is not None:
                return prod_len
        if seq_type.accept(_IS_BUILTIN_SEQUENCE_TYPE):
            return _SEQ_LEN_BOUND_TYPE
        return None

    def is_proven_in_bounds(self, seq_lvar, idx_lvar) -> bool:
        seq_type = self.type_of(seq_lvar)
        idx_type = self.type_of(idx_lvar)
        if (
            seq_type is BOTTOM
            or idx_type is BOTTOM
            or seq_type.may_be_unbound
            or idx_type.may_be_unbound
            or not idx_type.is_fixnum
            or idx_type.minimum is None
            or idx_type.minimum < 0
        ):
            return False
        exact_len = seq_type.exact_length()
        if (
            exact_len is not None
            and idx_type.maximum is not None
            and idx_type.maximum < exact_len
        ):
            return True
        seq_len_t = self.sequence_length_type(seq_lvar)
        if (
            seq_len_t is not None
            and not seq_len_t.may_be_unbound
            and seq_len_t.minimum is not None
            and idx_type.maximum is not None
            and idx_type.maximum < seq_len_t.minimum
        ):
            return True
        if self.relations:
            idx_term = _extract_linear_term(self, idx_lvar)
            if idx_term is not None:
                seq_linear = _LinearUpdate(self)._extract_seq_len_linear(seq_lvar)
                if (
                    seq_linear is not None
                    and seq_linear[1] is not None
                    and seq_linear[1] == seq_linear[2]
                    and _relational_comparison(
                        ast.ComparisonOperator.LESS,
                        self.relations,
                        idx_term[0],
                        seq_linear[0],
                        idx_term[1] - seq_linear[1],
                    )
                    is BOOLEAN_TRUE
                ):
                    return True
        return False

    def record_in_bounds(self, node, seq_lvar, idx_lvar) -> None:
        self.check_in_bounds_access(node, seq_lvar, idx_lvar)

    def _narrow_var_interval(
        self, var, minimum: int | None, maximum: int | None
    ) -> None:
        if var is None or var not in self.state or var in self.walk.indirect:
            return
        cur = self.state[var]
        if not cur.type.fits(PYTHON_INTEGER) or cur.type.may_be_unbound:
            return
        narrowed = cur.type
        if minimum is not None:
            narrowed = narrowed.narrow(
                ast.ComparisonOperator.GREATER_EQUAL, minimum, True
            )
        if maximum is not None and narrowed is not BOTTOM:
            narrowed = narrowed.narrow(ast.ComparisonOperator.LESS_EQUAL, maximum, True)
        if narrowed is not BOTTOM and narrowed != cur.type:
            self.assign(var, self.narrowed_fact(cur, narrowed))

    def check_in_bounds_access(self, node, seq_lvar, idx_lvar) -> None:
        seq_type = self.type_of(seq_lvar)
        idx_type = self.type_of(idx_lvar)
        if (
            seq_type is BOTTOM
            or idx_type is BOTTOM
            or seq_type.may_be_unbound
            or idx_type.may_be_unbound
            or not idx_type.fits(PYTHON_INTEGER)
            or idx_type.minimum is None
            or idx_type.minimum < 0
        ):
            self.walk.in_bounds_nodes.discard(node)
            return
        in_bounds = self.is_proven_in_bounds(seq_lvar, idx_lvar)
        seq_key = None
        src_var = _LinearUpdate(self)._origin_var(seq_lvar)
        if src_var is not None and self.has_vector_length(seq_lvar):
            src_var = self.walk.temp_source.get(src_var, src_var)
            seq_key = self.walk.seq_len_keys.get(src_var)
        idx_var = None
        idx_d_min = None
        idx_d_max = None
        local = _LinearUpdate(self)._origin_var(idx_lvar)
        if local is not None and local in self.walk.local_vars_set:
            idx_var = self.walk.temp_source.get(local, local)
            idx_d_min = idx_d_max = 0
        elif len(idx_lvar.uses) == 1:
            linear = idx_lvar.uses[0].accept(_LinearUpdate(self))
            if linear is not None:
                idx_var, idx_d_min, idx_d_max = linear
                if idx_var in self.walk.temp_source:
                    idx_var = self.walk.temp_source[idx_var]
        if (
            not in_bounds
            and self.relations
            and seq_key is not None
            and idx_var is not None
            and idx_d_max is not None
        ):
            rel = self.relations.get((idx_var, seq_key))
            if rel is not None and rel + idx_d_max <= -1:
                in_bounds = True
        if in_bounds:
            self.walk.in_bounds_nodes.add(node)
        else:
            self.walk.in_bounds_nodes.discard(node)
        if (
            seq_key is not None
            and idx_var is not None
            and idx_d_max is not None
            and idx_d_min == idx_d_max
            and seq_type.accept(_IS_BUILTIN_SEQUENCE_TYPE)
            and not in_bounds
        ):
            _tighten_relation(self.relations, idx_var, seq_key, -1 - idx_d_max)

    def prove_argument_type(self, lvar, proven_type: ValueType) -> None:
        recv_var = self.origins.get(lvar)
        if recv_var is not None and recv_var not in self.walk.indirect:
            if self.type_of(lvar) is UNKNOWN:
                self.assign(recv_var, _fact(proven_type))
            elif proven_type.fits(INTEGER):
                self._narrow_var_interval(
                    recv_var, proven_type.minimum, proven_type.maximum
                )
        elif len(lvar.uses) == 1 and proven_type.fits(INTEGER):
            linear = lvar.uses[0].accept(_LinearUpdate(self))
            if linear is not None:
                base_var, d_min, d_max = linear
                lo = (
                    proven_type.minimum - d_max
                    if proven_type.minimum is not None and d_max is not None
                    else None
                )
                hi = (
                    proven_type.maximum - d_min
                    if proven_type.maximum is not None and d_min is not None
                    else None
                )
                self._narrow_var_interval(base_var, lo, hi)

    def resolve_call_target(self, node: Combination):
        callee_fact = self.fact_of(node.fun)
        if callee_fact.target is not None:
            if callee_fact.proven:
                return callee_fact.target
            imported = self.walk.imported_target(node.fun)
            return imported if imported is callee_fact.target else None
        imported = self.walk.imported_target(node.fun)
        if imported is not None:
            return imported
        return self.walk.builtin_target(node.fun)

    def invalidate(self):
        if not self.globals_invalidated and self.walk.invalidated:
            state = self.state
            has_handler = self.has_handler
            mutated = self.state_mutated
            for key, val in self.walk.invalidated.items():
                if state.get(key) is not val:
                    if not mutated:
                        if has_handler:
                            self.handler_state = state.copy()
                        state = self.state = state.copy()
                        mutated = self.state_mutated = True
                    state[key] = val
                    if has_handler:
                        self.changed.add(key)
            self.globals_invalidated = True

    def forget_origin(self, var):
        if self.origins:
            self.origins = {
                lvar: origin
                for lvar, origin in self.origins.items()
                if origin is not var
            }

    def note_handler_state(self):
        """Join the state after a node into what an error edge carries.
        Only keys assigned since the previous node can differ."""
        if self.handler_state is not None and self.changed:
            handler = self.handler_state
            state = self.state
            for key in self.changed:
                handler[key] = join_fact(handler[key], state[key])
        if self.handler_relations is not None:
            if not self.relations:
                self.handler_relations.clear()
            else:
                self.handler_relations = _join_relations_list(
                    [self.handler_relations, self.relations]
                )
        self.changed.clear()

    # Nodes

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        self._current_ref = node
        try:
            return node.leaf.accept(self)
        finally:
            self._current_ref = None

    def visit_Leaf(self, leaf):
        # A known function's reference is a compile-time operand with no fact.
        return None

    def visit_Constant(self, leaf):
        fact = leaf.cached_fact
        if fact is None:
            fact = leaf.cached_fact = _fact(constant_type(leaf.value))
        return fact

    def visit_GlobalVar(self, leaf):
        # Any code anywhere may reference a module global and mutate the
        # container it holds, so its references see only the conservative
        # type (SBCL NODE-CONSERVATIVE-TYPE in ir1opt.lisp).
        fact = self.state[(_GLOBAL, leaf.name)].conservative()
        ref = self._current_ref
        if ref is not None and ref.lvar is not None:
            self.origins[ref.lvar] = (_GLOBAL, leaf.name)
        target = fact.target or self.walk.global_targets.get(leaf.name)
        if target is None and self.walk.imported_targets is not None:
            target = self.walk.imported_targets.get(leaf.name)
        if target is None:
            target = self.walk.foreign_builtin_function(leaf.name)
        if (
            fact.type is UNKNOWN
            and ref is not None
            and ref.derived_type is not None
            and ref.derived_type is not UNKNOWN
            and self.walk.lambda_.policy.safety < 3
            and ref.lexenv.policy.safety < 3
        ):
            if target is not None:
                return Fact(ref.derived_type.conservative_type(), target, fact.proven)
            return _fact(ref.derived_type.conservative_type())
        if fact.target is None and target is not None:
            fact = Fact(fact.type, target)
        return fact

    def visit_LambdaVar(self, leaf):
        state = self.state
        if leaf not in state:
            # A free variable of an enclosing function: any value.
            return UNKNOWN_FACT
        fact = state[leaf]
        ref = linked(self._current_ref, leaf, "reference")
        if ref.allow_unbound and fact.type.may_be_unbound:
            return UNKNOWN_FACT
        if ref.final:
            self.forget_origin(leaf)
            self.assign(leaf, UNBOUND_FACT)
        if ref.lvar is not None:
            self.origins[ref.lvar] = leaf
            if leaf in self.walk.var_contracts:
                self.walk.lvar_contracts[ref.lvar] = self.walk.var_contracts[leaf]
        if (
            fact.target is None
            and not fact.type.may_be_unbound
            and self.walk.imported_targets is not None
            and len(leaf.sets) == 1
            and (leaf.home is None or leaf not in leaf.home.vars)
        ):
            target = self.walk.imported_targets.get(leaf.name)
            if target is not None:
                fact = Fact(fact.type, target)
        if len(leaf.refs) > 1:
            # SBCL NODE-CONSERVATIVE-TYPE (ir1opt.lisp): a type that describes
            # a mutable container's contents holds at a reference only when
            # it is the variable's sole reference. Through any other
            # reference, code the analysis cannot see may have changed them.
            fact = fact.conservative()
            if leaf in self.walk.var_contracts and not fact.type.may_be_unbound:
                fact = fact.with_type(self.walk.var_contracts[leaf].refine(fact.type))
        return fact

    def visit_Lambda(self, leaf):
        return Fact(FunctionType(target=leaf), leaf, True)

    def visit_Set(self, node: Set):
        key = _key(node.var)
        if key not in self.state:
            return None
        if self.walk.temp_source:
            for k, v in list(self.walk.temp_source.items()):
                if k is key or v is key:
                    del self.walk.temp_source[k]
        old = self.state[key]
        if node.value is None:
            self.assign(key, UNBOUND_FACT)
            self.type_of_origins.pop(key, None)
            if self.relations and key in self.walk.local_vars_set:
                self._drop_relations_for(key)
            seq_key = self.walk.seq_len_keys.get(key)
            if seq_key is not None:
                self.assign(seq_key, UNBOUND_FACT)
                if self.relations:
                    self._drop_relations_for(seq_key)
        else:
            fact = self.fact_of(node.value)
            if (
                fact.type is UNKNOWN
                and key in self.walk.invalidated
                and self.walk.invalidated[key].type is not UNKNOWN
            ):
                fact = self.walk.invalidated[key]
            if key in self.walk.local_vars_set:
                fact = self._update_relations_on_set(key, node.value, fact)
                if key.type is not None and fact.type.fits(PYTHON_INTEGER):
                    fact = Fact(
                        fact.type.annotated_integer(key.type),
                        fact.target,
                        fact.proven,
                    )
            seq_key = self.walk.seq_len_keys.get(key)
            if seq_key is not None:
                self._update_seq_len_on_set(seq_key, node.value, fact)
            contract = self.walk.local_contracts.get(key)
            if contract is not None:
                if fact.type is not UNKNOWN and fact.type.fits(contract.checked_type):
                    self.walk.var_contracts[key] = contract
                else:
                    self.walk.var_contracts.pop(key, None)
            elif key in self.walk.local_vars_set and not key.indirect:
                is_single = node.var.accept(_SINGLE_ASSIGNED_LAMBDA_VAR)
                is_non_param = key not in self.walk.lambda_.vars
                val_contract = self.walk.lvar_contracts.get(node.value)
                if val_contract is not None and (
                    is_single
                    or (
                        is_non_param
                        and all(
                            s.value is not None
                            and self.walk.lvar_contracts.get(s.value) == val_contract
                            for s in key.sets
                        )
                    )
                ):
                    self.walk.var_contracts[key] = val_contract
                else:
                    self.walk.var_contracts.pop(key, None)
                iter_contract = self.walk.iter_contracts.get(node.value)
                if iter_contract is not None and (
                    is_single
                    or (
                        is_non_param
                        and all(
                            s.value is not None
                            and self.walk.iter_contracts.get(s.value) == iter_contract
                            for s in key.sets
                        )
                    )
                ):
                    self.walk.iter_contracts[key] = iter_contract
                else:
                    self.walk.iter_contracts.pop(key, None)
            src_var = self.origins.get(node.value)
            if (
                src_var is not None
                and src_var in self.state
                and key in self.walk.local_vars_set
                and key.hidden
                and len(key.sets) == 1
                and src_var in self.walk.local_vars_set
            ):
                self.walk.temp_source[key] = src_var
            type_origin = self.type_of_origins.get(node.value) or (
                self.type_of_origins.get(src_var) if src_var is not None else None
            )
            if type_origin is not None:
                self.type_of_origins[key] = type_origin
            else:
                self.type_of_origins.pop(key, None)
            self.assign(key, fact)
        # Origins are locals, so this changes nothing for a global.
        self.forget_origin(node.var)
        return old

    def _drop_relations_for(self, key) -> None:
        rels = self.relations
        for pair in [pair for pair in rels if key in pair]:
            del rels[pair]

    def _update_relations_on_set(self, key, value_lvar, fact: Fact) -> Fact:
        rels = self.relations
        linear = (
            value_lvar.uses[0].accept(_LinearUpdate(self))
            if len(value_lvar.uses) == 1
            else None
        )
        if linear is not None:
            src_var, d_min, d_max = linear
            if src_var is key:
                for (u, v), off in list(rels.items()):
                    if u is key:
                        if d_max is not None:
                            rels[u, v] = off + d_max
                        else:
                            del rels[u, v]
                    elif v is key:
                        if d_min is not None:
                            rels[u, v] = off - d_min
                        else:
                            del rels[u, v]
                if fact.type.fits(PYTHON_INTEGER) and not fact.type.may_be_unbound:
                    narrowed = fact.type
                    for (u, v), off in rels.items():
                        if u is key and v in self.state:
                            v_type = self.state[v].type
                            if (
                                v_type.fits(PYTHON_INTEGER)
                                and not v_type.may_be_unbound
                                and v_type.maximum is not None
                            ):
                                narrowed = narrowed.narrow(
                                    ast.ComparisonOperator.LESS_EQUAL,
                                    v_type.maximum + off,
                                    True,
                                )
                                if narrowed is BOTTOM:
                                    break
                        elif v is key and u in self.state:
                            u_type = self.state[u].type
                            if (
                                u_type.fits(PYTHON_INTEGER)
                                and not u_type.may_be_unbound
                                and u_type.minimum is not None
                            ):
                                narrowed = narrowed.narrow(
                                    ast.ComparisonOperator.GREATER_EQUAL,
                                    u_type.minimum - off,
                                    True,
                                )
                                if narrowed is BOTTOM:
                                    break
                    if narrowed is not BOTTOM and narrowed != fact.type:
                        fact = self.narrowed_fact(fact, narrowed)
                return fact
            self._drop_relations_for(key)
            if d_max is not None:
                _tighten_relation(rels, key, src_var, d_max)
            if d_min is not None:
                _tighten_relation(rels, src_var, key, -d_min)
            return fact
        self._drop_relations_for(key)
        return fact

    def _update_seq_len_on_set(self, seq_key, value_lvar, fact: Fact) -> None:
        rels = self.relations
        if rels:
            self._drop_relations_for(seq_key)
        if (
            fact.type.may_be_unbound
            or fact.type.fits(PYTHON_INTEGER)
            or fact.type is FLOAT
            or fact.type is NONE_TYPE
        ):
            self.assign(seq_key, UNBOUND_FACT)
            return
        src_var = self.origins.get(value_lvar)
        if src_var is not None:
            src_var = self.walk.temp_source.get(src_var, src_var)
            src_seq_key = self.walk.seq_len_keys.get(src_var)
            if src_seq_key is not None and src_seq_key in self.state:
                src_len_fact = self.state[src_seq_key]
                self.assign(seq_key, src_len_fact)
                if not src_len_fact.type.may_be_unbound and src_seq_key is not seq_key:
                    _tighten_relation(rels, seq_key, src_seq_key, 0)
                    _tighten_relation(rels, src_seq_key, seq_key, 0)
                return
        linear = None
        seq_len_t = None
        if len(value_lvar.uses) == 1:
            seq_len_t, linear = value_lvar.uses[0].accept(_SequenceProducerLength(self))
        if seq_len_t is None:
            seq_len_t = self.sequence_length_type(value_lvar)
        if seq_len_t is None or seq_len_t is BOTTOM:
            self.assign(seq_key, UNBOUND_FACT)
            return
        self.assign(seq_key, _fact(seq_len_t))
        if linear is not None:
            base_key, d_min, d_max = linear
            if base_key is not seq_key:
                if d_max is not None:
                    _tighten_relation(rels, seq_key, base_key, d_max)
                if d_min is not None:
                    _tighten_relation(rels, base_key, seq_key, -d_min)

    def visit_Cast(self, node: Cast):
        value = self.type_of(node.value)
        if value is BOTTOM:
            return NO_FACT
        if node.declaration is not None and node.declaration[0] == "inferred-return":
            call = node.declaration[1]
            if call.kind in (CallKind.FULL, CallKind.GUARDED) and call.info is not None:
                callee_fact = self.fact_of(call.fun)
                if (
                    callee_fact.target is not None
                    and (target_lam := callee_fact.target.as_lambda()) is not None
                    and (
                        target_lam in self.walk.recursive_targets
                        or self.walk.is_specialized_context
                        or self.walk.for_return_derivation
                    )
                ):
                    arg_facts = tuple(self.fact_of(arg) for arg in call.args)
                    arg_types = tuple(f.type for f in arg_facts)
                    if any(t is BOTTOM for t in arg_types):
                        return NO_FACT
                    arg_domains = tuple(
                        FunctionType(target=lam)
                        if t is UNKNOWN
                        and f.target is not None
                        and (lam := f.target.as_lambda()) is not None
                        else t
                        for f, t in zip(arg_facts, arg_types, strict=True)
                    )
                    resolved = self.walk.resolve_call(
                        callee_fact,
                        tuple(t.widen_inferred_contract() for t in arg_domains),
                        call.info.location,
                        call.info.keywords,
                        True,
                    )
                    if resolved is BOTTOM:
                        return NO_FACT
                    if node.contract.fact is PYTHON_INTEGER and resolved.is_fixnum:
                        return _fact(resolved.widen_constant())
                    if (
                        (
                            self.walk.is_specialized_context
                            or self.walk.for_return_derivation
                        )
                        and resolved is not UNKNOWN
                        and not resolved.has_unspecialized_part()
                    ):
                        specialized_fact = InferredReturnContract(
                            target_lam.name, resolved.widen_inferred_contract()
                        ).fact
                        if specialized_fact.fits(node.contract.fact):
                            return _fact(specialized_fact)
        if self.walk.annotations.check(
            node.contract, value, node.location, node.declaration
        ):
            return _fact(value)
        if node.lvar is not None and node.contract.has_element_contracts():
            self.walk.lvar_contracts[node.lvar] = node.contract
        return _fact(node.contract.refine(value))

    def is_predictable_len(self, node: Combination) -> bool:
        if (
            node.kind not in (CallKind.FULL, CallKind.GUARDED)
            or len(node.args) != 1
            or node.info is None
            or node.info.keywords
            or self.walk.lambda_.kind is FunctionalKind.TOPLEVEL
            or self.walk.lambda_.policy.safety >= 3
            or node.lexenv.policy.safety >= 3
            or "len" in self.walk.rebound_globals
            or "len" in self.walk.global_targets
            or len(node.fun.uses) != 1
        ):
            return False
        if node.fun.uses[0].accept(_IMPORTED_CALLEE_NAME) != "len":
            return False
        return self.type_of(node.args[0]).accept(_IS_BUILTIN_SEQUENCE_TYPE)

    def visit_Combination(self, node: Combination):
        if node.kind in (CallKind.FULL, CallKind.GUARDED):
            info = linked(node.info, node, "info")
            arg_types = tuple(self.type_of(arg) for arg in node.args)
            if any(t is BOTTOM for t in arg_types):
                return NO_FACT
            if self.is_predictable_len(node):
                seq_len = self.sequence_length_type(node.args[0])
                if seq_len is not None:
                    return _fact(seq_len)
            arg_domains = tuple(
                self.fact_of(arg).argument_domain() for arg in node.args
            )
            callee_fact = self.fact_of(node.fun)
            target = self.resolve_call_target(node)
            resolved = self.walk.resolve_call(
                callee_fact,
                arg_domains,
                info.location,
                info.keywords,
            )
            if resolved is BOTTOM:
                return NO_FACT
            if (
                resolved is UNKNOWN
                and not callee_fact.proven
                and callee_fact.target is not None
                and (target_lam := callee_fact.target.as_lambda()) is not None
                and (
                    target_lam in self.walk.recursive_targets
                    or (
                        not self.walk.lambda_.is_protocol
                        and (
                            not self.walk.unique_targets
                            or target_lam in self.walk.unique_targets
                        )
                    )
                )
                and self.walk.lambda_.policy.infers_return_contracts
                and node.lexenv.policy.infers_return_contracts
                and target_lam.policy.infers_return_contracts
                and target_lam.namespace == self.walk.lambda_.namespace
                and node.lvar is not None
                and len(node.lvar.uses) == 1
                and node.lvar.dest is not None
                and (
                    (
                        self.walk.for_return_derivation
                        and not self.walk.lambda_.is_protocol
                    )
                    or not node.lvar.dest.accept(_InferredReturnDestination(node))
                )
                and len(node.fun.uses) == 1
                and (callee_name := node.fun.use().accept(_GLOBAL_CALLEE_NAME))
                is not None
                and self.walk.global_targets.get(callee_name) is target_lam
                and target_lam.source_name == callee_name
                and target_lam.code is not None
                and info.location is not None
                and target_lam.code.filename == info.location[0]
                and not (
                    self.walk.lambda_.kind is FunctionalKind.TOPLEVEL
                    and target_lam.code.firstlineno > info.location[1]
                )
            ):
                inferred_res = self.walk.resolve_call(
                    callee_fact,
                    tuple(t.widen_inferred_contract() for t in arg_domains),
                    info.location,
                    info.keywords,
                    True,
                )
                if inferred_res is BOTTOM:
                    return NO_FACT
                if inferred_res is not UNKNOWN:
                    resolved = InferredReturnContract(
                        target_lam.name, inferred_res.widen_inferred_contract()
                    ).fact
            if resolved is UNKNOWN and target is not None:
                resolved = target.propagate_call(
                    node, self, node.args, arg_types, info.keywords
                )
                if resolved is BOTTOM:
                    return NO_FACT
            if resolved is UNKNOWN and self.walk.imported_returns:
                callee_name = node.fun.use().accept(_IMPORTED_CALLEE_NAME)
                if callee_name is not None:
                    resolved = self.walk.imported_returns.get(callee_name, UNKNOWN)
            builtin_type = UNKNOWN
            if resolved is UNKNOWN:
                builtin_type = self.walk.builtin_return_type(
                    node.fun, arg_types, info.keywords
                )
                if builtin_type is BOTTOM:
                    return NO_FACT
                resolved = builtin_type
            if len(node.args) == 1 and not info.keywords and node.lvar is not None:
                arg_contract = self.walk.lvar_contracts.get(node.args[0])
                if arg_contract is not None:
                    iter_elem = arg_contract.iter_element_contract()
                    if iter_elem is not None:
                        preserved = self.walk.element_preserving_builtin_contract(
                            node.fun, iter_elem
                        )
                        if preserved is not None:
                            self.walk.lvar_contracts[node.lvar] = preserved
            fact = _fact(resolved)
            if not (
                builtin_type is not UNKNOWN
                and not any(t.may_run_user_code for t in arg_types)
            ):
                self.invalidate()
            return fact
        return self.derive_known(node)

    def derive_known(self, node: Combination):
        info = node.fun_info
        assert info is not None
        if info.propagate is not None:
            res = info.propagate(self, node)
            if res is not None:
                return res.as_fact(_fact)
        if info.derive_type is not None:
            value_type = info.derive_type(node)
            if value_type is not None:
                if info.may_call or info.unsafe:
                    self.invalidate()
                return _fact(value_type)
        if info.protocol is not None:
            self.invalidate()
            return UNKNOWN_FACT
        if info.result:
            return UNKNOWN_FACT
        return None

    def is_proven_fixnum_range(self, node: Combination) -> bool:
        state = linked(node.info, node, "info")
        if state.direct:
            return True
        if (
            self.walk.lambda_.policy.safety >= 3
            or node.lexenv.policy.safety >= 3
            or "range" in self.walk.rebound_globals
            or "range" in self.walk.global_targets
            or not node.args
            or len(node.args[0].uses) != 1
            or not node.args[0].uses[0].accept(_UNMUTATED_RANGE_REF)
        ):
            return False
        if not (1 <= len(state.arguments) <= 3):
            return False
        arg_types = [self.type_of(arg) for arg in state.arguments]
        if not all(
            not arg_t.may_be_unbound and arg_t.fits(PYTHON_INTEGER) and arg_t.is_fixnum
            for arg_t in arg_types
        ):
            return False
        if len(arg_types) == 3:
            step = arg_types[2]
            if not (
                (step.minimum is not None and step.minimum > 0)
                or (step.maximum is not None and step.maximum < 0)
            ):
                return False
        return True

    def range_type(self, state) -> IntegerType:
        minimum, maximum = state.minimum, state.maximum
        arguments = [self.type_of(arg) for arg in state.arguments]
        if arguments and all(arg.fits(PYTHON_INTEGER) for arg in arguments):
            if len(arguments) == 1:
                stop = arguments[0]
                minimum = max(minimum, 0)
                if stop.maximum is not None:
                    maximum = min(maximum, stop.maximum - 1)
            else:
                start, stop = arguments[0], arguments[1]
                step = arguments[2] if len(arguments) == 3 else SMALL_INTS[1]
                if step.minimum is not None and step.minimum > 0:
                    if start.minimum is not None:
                        minimum = max(minimum, start.minimum)
                    if stop.maximum is not None:
                        maximum = min(maximum, stop.maximum - 1)
                elif step.maximum is not None and step.maximum < 0:
                    if stop.minimum is not None:
                        minimum = max(minimum, stop.minimum + 1)
                    if start.maximum is not None:
                        maximum = min(maximum, start.maximum)
        if minimum > maximum:
            return IntegerType(state.minimum, state.maximum)
        return IntegerType(minimum, maximum)

    def visit_If(self, node: If):
        if self.type_of(node.test).may_run_user_code:
            self.invalidate()


class _ResolvedMethod(NodeVisitor):
    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if node.fun_info is fndb.RESOLVE_METHOD:
            return node.args[0], linked(node.info, node, "info").name
        return None


_RESOLVED_METHOD = _ResolvedMethod()


class _ImportedModuleReceiver(NodeVisitor, LeafVisitor):
    """Resolve the module name an unmutated imported module lvar originates from."""

    def __init__(self):
        self._active_aliases: set[LambdaVar] = set()

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is fndb.IMPORT
            and node.info is not None
        ):
            return node.info.module_name
        return None

    def visit_Ref(self, node: Ref):
        if node.lexenv.policy.safety >= 3:
            return None
        return node.leaf.accept(self)

    def visit_GlobalVar(self, leaf: GlobalVar):
        if len(leaf.sets) != 1:
            return None
        store = leaf.sets[0]
        if (
            store.home_lambda is None
            or store.home_lambda.kind is not FunctionalKind.TOPLEVEL
            or store.value is None
            or len(store.value.uses) != 1
        ):
            return None
        return store.value.uses[0].accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        if leaf.home is not None and leaf in leaf.home.vars:
            return None
        if leaf in self._active_aliases:
            return None
        value_sets = [s for s in leaf.sets if s.value is not None]
        if len(value_sets) != 1:
            return None
        val = value_sets[0].value
        if val is None or len(val.uses) != 1:
            return None
        self._active_aliases.add(leaf)
        try:
            return val.uses[0].accept(self)
        finally:
            self._active_aliases.remove(leaf)


class _BuildDictType(ValueTypeVisitor):
    def visit_ValueType(self, fact):
        return DICT_TYPE, True

    def visit_TupleType(self, fact):
        if fact.may_be_unbound:
            return DICT_TYPE, True
        if fact.arity == 0:
            return EMPTY_DICT_TYPE, False
        if fact.elements is not None and len(fact.elements) % 2 == 0:
            dict_type = EMPTY_DICT_TYPE
            may_call = False
            for key_t, val_t in zip(
                fact.elements[0::2], fact.elements[1::2], strict=True
            ):
                dict_type = dict_type.with_entry(key_t, val_t)
                if key_t.may_run_user_code:
                    may_call = True
            return dict_type, may_call
        return DICT_TYPE, True


_BUILD_DICT_TYPE = _BuildDictType()


class _BuildSetType(ValueTypeVisitor):
    def visit_ValueType(self, fact):
        return SET_TYPE, True

    def visit_TupleType(self, fact):
        if fact.may_be_unbound:
            return SET_TYPE, True
        if fact.arity == 0:
            return EMPTY_SET_TYPE, False
        if fact.elements is not None:
            set_type = EMPTY_SET_TYPE
            may_call = False
            for elem_t in fact.elements:
                set_type = set_type.with_element(elem_t)
                if elem_t.may_run_user_code:
                    may_call = True
            return set_type, may_call
        return SET_TYPE, True

    def visit_ListType(self, fact):
        if fact.may_be_unbound:
            return SET_TYPE, True
        if fact.element is BOTTOM:
            return EMPTY_SET_TYPE, False
        return (
            EMPTY_SET_TYPE.with_element(fact.element),
            fact.element.may_run_user_code,
        )


_BUILD_SET_TYPE = _BuildSetType()


class _SequenceIteratorVar(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.fun_info is not None
            and node.fun_info.name in ("%sequence_available", "%byte_available")
            and len(node.args) == 1
            and len(node.args[0].uses) == 1
        ):
            return node.args[0].uses[0].accept(self)
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_LambdaVar(self, leaf):
        return leaf


_SEQUENCE_ITERATOR_VAR = _SequenceIteratorVar()


def _loop_iterator_var(block):
    for pred in block.pred:
        for pred_node in pred.nodes():
            var = pred_node.accept(_SEQUENCE_ITERATOR_VAR)
            if var is not None:
                return var
    return None


class _UnmutatedRangeRef(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return False

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return False

    def visit_GlobalVar(self, leaf):
        return leaf.name == "range" and not leaf.sets

    def visit_LambdaVar(self, leaf):
        if len(leaf.sets) != 1:
            return False
        value = leaf.sets[0].value
        if value is None or len(value.uses) != 1:
            return False
        return value.uses[0].accept(self)


_UNMUTATED_RANGE_REF = _UnmutatedRangeRef()


_BINARY_OPERATORS = {
    **{f"%{operator.name.lower()}": operator for operator in ast.BinaryOperator},
    **{f"%i{operator.name.lower()}": operator for operator in ast.BinaryOperator},
}
_UNARY_OPERATORS = {
    f"%unary_{operator.name.lower()}": operator for operator in ast.UnaryOperator
}
_COMPARISON_OPERATORS = {
    f"%compare_{operator.name.lower()}": operator for operator in ast.ComparisonOperator
}
_SWAPPED = {
    ast.ComparisonOperator.LESS: ast.ComparisonOperator.GREATER,
    ast.ComparisonOperator.LESS_EQUAL: ast.ComparisonOperator.GREATER_EQUAL,
    ast.ComparisonOperator.GREATER: ast.ComparisonOperator.LESS,
    ast.ComparisonOperator.GREATER_EQUAL: ast.ComparisonOperator.LESS_EQUAL,
}
_NEGATED_COMPARISON = {
    ast.ComparisonOperator.LESS: ast.ComparisonOperator.GREATER_EQUAL,
    ast.ComparisonOperator.LESS_EQUAL: ast.ComparisonOperator.GREATER,
    ast.ComparisonOperator.GREATER: ast.ComparisonOperator.LESS_EQUAL,
    ast.ComparisonOperator.GREATER_EQUAL: ast.ComparisonOperator.LESS,
    ast.ComparisonOperator.EQUAL: ast.ComparisonOperator.NOT_EQUAL,
    ast.ComparisonOperator.NOT_EQUAL: ast.ComparisonOperator.EQUAL,
}


def _comparison_bound(
    operator: ast.ComparisonOperator, other_type: ValueType, truth: bool
) -> int | None:
    if other_type.may_be_unbound:
        return None
    effective = operator if truth else _NEGATED_COMPARISON.get(operator, operator)
    if effective in (
        ast.ComparisonOperator.LESS,
        ast.ComparisonOperator.LESS_EQUAL,
    ):
        return other_type.maximum
    if effective in (
        ast.ComparisonOperator.GREATER,
        ast.ComparisonOperator.GREATER_EQUAL,
    ):
        return other_type.minimum
    if effective in (
        ast.ComparisonOperator.EQUAL,
        ast.ComparisonOperator.NOT_EQUAL,
    ):
        return other_type.exact_integer()
    return None


def _extract_linear_term(visitor: _Propagation, lvar):
    local = visitor.origins.get(lvar)
    if local is not None and local in visitor.walk.local_vars_set:
        local = visitor.walk.temp_source.get(local, local)
        return local, 0
    if len(lvar.uses) == 1:
        linear = lvar.uses[0].accept(_LinearUpdate(visitor))
        if linear is not None:
            base_var, d_min, d_max = linear
            if (
                base_var in visitor.walk.relational_vars_set
                and d_min is not None
                and d_min == d_max
            ):
                return base_var, d_min
    return None


def _relational_comparison(
    operator: ast.ComparisonOperator,
    rels: dict,
    u_var,
    v_var,
    delta: int,
) -> ValueType | None:
    uv = 0 if u_var is v_var else rels.get((u_var, v_var))
    vu = 0 if u_var is v_var else rels.get((v_var, u_var))
    if operator is ast.ComparisonOperator.LESS:
        if uv is not None and uv + delta <= -1:
            return BOOLEAN_TRUE
        if vu is not None and vu - delta <= 0:
            return BOOLEAN_FALSE
    elif operator is ast.ComparisonOperator.LESS_EQUAL:
        if uv is not None and uv + delta <= 0:
            return BOOLEAN_TRUE
        if vu is not None and vu - delta <= -1:
            return BOOLEAN_FALSE
    elif operator is ast.ComparisonOperator.GREATER:
        if vu is not None and vu - delta <= -1:
            return BOOLEAN_TRUE
        if uv is not None and uv + delta <= 0:
            return BOOLEAN_FALSE
    elif operator is ast.ComparisonOperator.GREATER_EQUAL:
        if vu is not None and vu - delta <= 0:
            return BOOLEAN_TRUE
        if uv is not None and uv + delta <= -1:
            return BOOLEAN_FALSE
    elif operator is ast.ComparisonOperator.EQUAL:
        if uv is not None and vu is not None and uv + delta <= 0 and vu - delta <= 0:
            return BOOLEAN_TRUE
        if (uv is not None and uv + delta <= -1) or (
            vu is not None and vu - delta <= -1
        ):
            return BOOLEAN_FALSE
    elif operator is ast.ComparisonOperator.NOT_EQUAL:
        if (uv is not None and uv + delta <= -1) or (
            vu is not None and vu - delta <= -1
        ):
            return BOOLEAN_TRUE
        if uv is not None and vu is not None and uv + delta <= 0 and vu - delta <= 0:
            return BOOLEAN_FALSE
    return None


def _apply_relations_to_state(rels: dict, state: dict, visitor: _Propagation) -> bool:
    if not rels:
        return False
    changed = False
    for (u, v), off in rels.items():
        if u not in state or v not in state:
            continue
        u_fact = state[u]
        v_fact = state[v]
        u_type = u_fact.type
        v_type = v_fact.type
        if (
            not u_type.fits(PYTHON_INTEGER)
            or u_type.may_be_unbound
            or not v_type.fits(PYTHON_INTEGER)
            or v_type.may_be_unbound
        ):
            continue
        if v_type.maximum is not None:
            u_narrowed = u_type.narrow(
                ast.ComparisonOperator.LESS_EQUAL, v_type.maximum + off, True
            )
            if u_narrowed is BOTTOM:
                state[_DEAD_EDGE] = True
                return True
            if u_narrowed != u_type:
                state[u] = visitor.narrowed_fact(u_fact, u_narrowed)
                u_fact = state[u]
                u_type = u_narrowed
                changed = True
        if u_type.minimum is not None:
            v_narrowed = v_type.narrow(
                ast.ComparisonOperator.GREATER_EQUAL, u_type.minimum - off, True
            )
            if v_narrowed is BOTTOM:
                state[_DEAD_EDGE] = True
                return True
            if v_narrowed != v_type:
                state[v] = visitor.narrowed_fact(v_fact, v_narrowed)
                changed = True
    return changed


def _make_binary_propagator(operator: ast.BinaryOperator):
    def _prop(visitor: _Propagation, node: Combination):
        args = node.args
        left, right = visitor.type_of(args[0]), visitor.type_of(args[1])
        if left is BOTTOM or right is BOTTOM:
            return NO_FACT
        res_type = left.binary(operator, right)
        if node.lvar is not None:
            left_contract = visitor.walk.lvar_contracts.get(args[0])
            if left_contract is not None:
                right_contract = visitor.walk.lvar_contracts.get(args[1])
                res_contract = left_contract.binary_result_contract(
                    operator, right_contract, right
                )
                if res_contract is not None:
                    visitor.walk.lvar_contracts[node.lvar] = res_contract
        if (
            operator is ast.BinaryOperator.SUBTRACT
            and visitor.relations
            and res_type.fits(PYTHON_INTEGER)
            and left.fits(PYTHON_INTEGER)
            and right.fits(PYTHON_INTEGER)
            and not left.may_be_unbound
            and not right.may_be_unbound
        ):
            left_term = _extract_linear_term(visitor, args[0])
            right_term = _extract_linear_term(visitor, args[1])
            if left_term is not None and right_term is not None:
                u_var, c0 = left_term
                v_var, c1 = right_term
                delta = c0 - c1
                uv = 0 if u_var is v_var else visitor.relations.get((u_var, v_var))
                vu = 0 if u_var is v_var else visitor.relations.get((v_var, u_var))
                narrowed = res_type
                if vu is not None:
                    narrowed = narrowed.narrow(
                        ast.ComparisonOperator.GREATER_EQUAL, -vu + delta, True
                    )
                if uv is not None and narrowed is not BOTTOM:
                    narrowed = narrowed.narrow(
                        ast.ComparisonOperator.LESS_EQUAL, uv + delta, True
                    )
                if narrowed is not BOTTOM:
                    res_type = narrowed
        fact = _fact(res_type)
        if left.may_run_user_code or right.may_run_user_code:
            visitor.invalidate()
        return fact

    return _prop


def _make_unary_propagator(operator: ast.UnaryOperator):
    def _prop(visitor: _Propagation, node: Combination):
        operand = visitor.type_of(node.args[0])
        if operand is BOTTOM:
            return NO_FACT
        fact = _fact(operand.unary(operator))
        if operand.may_run_user_code:
            visitor.invalidate()
        return fact

    return _prop


def _interval_comparison(
    operator: ast.ComparisonOperator, left: ValueType, right: ValueType
) -> ValueType | None:
    if operator is ast.ComparisonOperator.LESS:
        if (
            left.maximum is not None
            and right.minimum is not None
            and left.maximum < right.minimum
        ):
            return BOOLEAN_TRUE
        if (
            left.minimum is not None
            and right.maximum is not None
            and left.minimum >= right.maximum
        ):
            return BOOLEAN_FALSE
    elif operator is ast.ComparisonOperator.LESS_EQUAL:
        if (
            left.maximum is not None
            and right.minimum is not None
            and left.maximum <= right.minimum
        ):
            return BOOLEAN_TRUE
        if (
            left.minimum is not None
            and right.maximum is not None
            and left.minimum > right.maximum
        ):
            return BOOLEAN_FALSE
    elif operator is ast.ComparisonOperator.GREATER:
        if (
            left.minimum is not None
            and right.maximum is not None
            and left.minimum > right.maximum
        ):
            return BOOLEAN_TRUE
        if (
            left.maximum is not None
            and right.minimum is not None
            and left.maximum <= right.minimum
        ):
            return BOOLEAN_FALSE
    elif operator is ast.ComparisonOperator.GREATER_EQUAL:
        if (
            left.minimum is not None
            and right.maximum is not None
            and left.minimum >= right.maximum
        ):
            return BOOLEAN_TRUE
        if (
            left.maximum is not None
            and right.minimum is not None
            and left.maximum < right.minimum
        ):
            return BOOLEAN_FALSE
    elif operator is ast.ComparisonOperator.EQUAL:
        if (
            left.minimum is not None
            and left.maximum is not None
            and right.minimum is not None
            and right.maximum is not None
            and left.minimum == left.maximum == right.minimum == right.maximum
        ):
            return BOOLEAN_TRUE
        if (
            left.maximum is not None
            and right.minimum is not None
            and left.maximum < right.minimum
        ) or (
            left.minimum is not None
            and right.maximum is not None
            and left.minimum > right.maximum
        ):
            return BOOLEAN_FALSE
    elif operator is ast.ComparisonOperator.NOT_EQUAL:
        if (
            left.maximum is not None
            and right.minimum is not None
            and left.maximum < right.minimum
        ) or (
            left.minimum is not None
            and right.maximum is not None
            and left.minimum > right.maximum
        ):
            return BOOLEAN_TRUE
        if (
            left.minimum is not None
            and left.maximum is not None
            and right.minimum is not None
            and right.maximum is not None
            and left.minimum == left.maximum == right.minimum == right.maximum
        ):
            return BOOLEAN_FALSE
    return None


def _make_comparison_propagator(operator: ast.ComparisonOperator):
    def _prop(visitor: _Propagation, node: Combination):
        args = node.args
        left = _equality_target_type(visitor, args[0])
        right = _equality_target_type(visitor, args[1])
        if left is BOTTOM or right is BOTTOM:
            return NO_FACT
        res_type = left.comparison(operator, right)
        if (
            res_type is BOOLEAN
            and left.fits(PYTHON_INTEGER)
            and right.fits(PYTHON_INTEGER)
            and not left.may_be_unbound
            and not right.may_be_unbound
        ):
            if visitor.walk.seq_len_keys:
                iv_cmp = _interval_comparison(operator, left, right)
                if iv_cmp is not None:
                    res_type = iv_cmp
            if res_type is BOOLEAN and visitor.relations:
                left_term = _extract_linear_term(visitor, args[0])
                right_term = _extract_linear_term(visitor, args[1])
                if left_term is not None and right_term is not None:
                    rel_cmp = _relational_comparison(
                        operator,
                        visitor.relations,
                        left_term[0],
                        right_term[0],
                        left_term[1] - right_term[1],
                    )
                    if rel_cmp is not None:
                        res_type = rel_cmp
        fact = _fact(res_type)
        if operator not in (
            ast.ComparisonOperator.IS,
            ast.ComparisonOperator.IS_NOT,
        ) and (left.may_run_user_code or right.may_run_user_code):
            visitor.invalidate()
        return fact

    return _prop


def _constrain_identity_comparison(
    operator: ast.ComparisonOperator,
    visitor: _Propagation,
    node: Combination,
    state: dict,
    truth: bool,
) -> bool:
    if len(node.args) != 2:
        return False
    left, right = node.args
    is_same = (operator is ast.ComparisonOperator.IS) == truth
    left_type = visitor.type_of(left)
    right_type = visitor.type_of(right)
    cmp = left_type.comparison(
        ast.ComparisonOperator.IS if is_same else ast.ComparisonOperator.IS_NOT,
        right_type,
    )
    if cmp is BOOLEAN_FALSE:
        state[_DEAD_EDGE] = True
        return True
    changed = False
    for operand, other in ((left, right), (right, left)):
        other_type = visitor.type_of(other)
        local = visitor.origins.get(operand)
        if local is not None and local in visitor.state:
            current = state[local]
            narrowed = current.type.narrow_identity(other_type, is_same)
            if narrowed is BOTTOM:
                state[_DEAD_EDGE] = True
                return True
            if narrowed != current.type:
                state[local] = visitor.narrowed_fact(current, narrowed)
                changed = True
        subj_var = visitor.type_of_origins.get(operand) or (
            visitor.type_of_origins.get(local) if local is not None else None
        )
        if subj_var is not None and subj_var in visitor.state:
            subj_cur = state[subj_var]
            narrowed_subj = subj_cur.type.narrow_by_runtime_class(other_type, is_same)
            if narrowed_subj is BOTTOM:
                state[_DEAD_EDGE] = True
                return True
            if narrowed_subj != subj_cur.type:
                state[subj_var] = visitor.narrowed_fact(subj_cur, narrowed_subj)
                changed = True
    return changed


class _ConstantTextLiteralType(NodeVisitor, LeafVisitor, ast.Visitor):
    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_Constant(self, leaf):
        return leaf.value.accept(self)

    def visit_TextLiteral(self, node: ast.TextLiteral):
        if len(node.value.encode("utf-8", "surrogatepass")) <= 64:
            return INTERNED_TEXTS.get(node.value) or TextType(node.value)
        return None


_CONSTANT_TEXT_LITERAL_TYPE = _ConstantTextLiteralType()


def _equality_target_type(visitor: _Propagation, lvar) -> ValueType:
    other_type = visitor.type_of(lvar)
    if (
        other_type.fits(TEXT_TYPE)
        and other_type.exact_text() is None
        and len(lvar.uses) == 1
    ):
        literal_type = lvar.uses[0].accept(_CONSTANT_TEXT_LITERAL_TYPE)
        if literal_type is not None:
            return literal_type
    return other_type


def _make_comparison_constrainer(operator: ast.ComparisonOperator):
    if operator in (ast.ComparisonOperator.IS, ast.ComparisonOperator.IS_NOT):

        def _constrain_is(
            visitor: _Propagation, node: Combination, state: dict, truth: bool
        ) -> bool:
            return _constrain_identity_comparison(operator, visitor, node, state, truth)

        return _constrain_is

    swapped = _SWAPPED.get(
        operator,
        operator
        if operator in (ast.ComparisonOperator.EQUAL, ast.ComparisonOperator.NOT_EQUAL)
        else None,
    )

    def _constrain_ord(
        visitor: _Propagation, node: Combination, state: dict, truth: bool
    ) -> bool:
        if len(node.args) != 2:
            return False
        left, right = node.args
        left_type = _equality_target_type(visitor, left)
        right_type = _equality_target_type(visitor, right)
        cmp = left_type.comparison(operator, right_type)
        if (cmp is BOOLEAN_FALSE and truth) or (cmp is BOOLEAN_TRUE and not truth):
            state[_DEAD_EDGE] = True
            return True
        left_term = _extract_linear_term(visitor, left)
        right_term = _extract_linear_term(visitor, right)
        edge_rels = None
        if (
            left_term is not None
            and right_term is not None
            and left_type.fits(PYTHON_INTEGER)
            and right_type.fits(PYTHON_INTEGER)
            and not left_type.may_be_unbound
            and not right_type.may_be_unbound
        ):
            u_var, c0 = left_term
            v_var, c1 = right_term
            delta = c0 - c1
            if visitor.relations:
                rel_cmp = _relational_comparison(
                    operator, visitor.relations, u_var, v_var, delta
                )
                if (rel_cmp is BOOLEAN_FALSE and truth) or (
                    rel_cmp is BOOLEAN_TRUE and not truth
                ):
                    state[_DEAD_EDGE] = True
                    return True
            eff_op = operator if truth else _NEGATED_COMPARISON.get(operator, operator)
            edge_rels = (
                visitor.edge_relations
                if visitor.edge_relations is not None
                else visitor.relations
            ).copy()
            feasible = True
            if eff_op is ast.ComparisonOperator.LESS:
                feasible = _tighten_relation(edge_rels, u_var, v_var, -1 - delta)
            elif eff_op is ast.ComparisonOperator.LESS_EQUAL:
                feasible = _tighten_relation(edge_rels, u_var, v_var, -delta)
            elif eff_op is ast.ComparisonOperator.GREATER:
                feasible = _tighten_relation(edge_rels, v_var, u_var, -1 + delta)
            elif eff_op is ast.ComparisonOperator.GREATER_EQUAL:
                feasible = _tighten_relation(edge_rels, v_var, u_var, delta)
            elif eff_op is ast.ComparisonOperator.EQUAL:
                feasible = _tighten_relation(
                    edge_rels, u_var, v_var, -delta
                ) and _tighten_relation(edge_rels, v_var, u_var, delta)
            if not feasible:
                state[_DEAD_EDGE] = True
                return True
            visitor.edge_relations = edge_rels
        changed = False
        for operand, other_eq_type, other, op, term in (
            (left, right_type, right, operator, left_term),
            (right, left_type, left, swapped, right_term),
        ):
            if op is None:
                continue
            local = visitor.origins.get(operand)
            offset = 0
            if local is None and term is not None:
                local, offset = term
            if local is None or local not in visitor.state:
                continue
            target_locals = (
                (local, visitor.walk.temp_source[local])
                if local in visitor.walk.temp_source
                else (local,)
            )
            other_type = visitor.type_of(other)
            bound = _comparison_bound(op, other_type, truth)
            eq_target = (
                other_eq_type
                if offset == 0
                and op
                in (
                    ast.ComparisonOperator.EQUAL,
                    ast.ComparisonOperator.NOT_EQUAL,
                )
                else None
            )
            for target_local in target_locals:
                if target_local not in state:
                    continue
                current = state[target_local]
                if current is UNBOUND_FACT:
                    continue
                narrowed = current.type
                if eq_target is not None:
                    is_equal = (op is ast.ComparisonOperator.EQUAL) == truth
                    narrowed = narrowed.narrow_equality(eq_target, is_equal)
                if bound is not None and narrowed is not BOTTOM:
                    narrowed = narrowed.narrow(op, bound - offset, truth)
                if narrowed is BOTTOM:
                    state[_DEAD_EDGE] = True
                    return True
                if narrowed != current.type:
                    state[target_local] = visitor.narrowed_fact(current, narrowed)
                    changed = True
        if edge_rels is not None or changed:
            active_rels = edge_rels if edge_rels is not None else visitor.relations
            if active_rels and _apply_relations_to_state(active_rels, state, visitor):
                if _DEAD_EDGE in state:
                    return True
                changed = True
        return changed

    return _constrain_ord


for _op_name, _op_enum in _BINARY_OPERATORS.items():
    defpropagate(_op_name)(_make_binary_propagator(_op_enum))

for _op_name, _op_enum in _UNARY_OPERATORS.items():
    defpropagate(_op_name)(_make_unary_propagator(_op_enum))

for _op_name, _op_enum in _COMPARISON_OPERATORS.items():
    defpropagate(_op_name)(_make_comparison_propagator(_op_enum))
    defconstrain(_op_name)(_make_comparison_constrainer(_op_enum))
