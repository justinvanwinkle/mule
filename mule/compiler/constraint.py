"""Flow-sensitive propagation of variable facts, following
``src/compiler/constraint.lisp``.

SBCL propagates sets of constraints (``(typep var type)``, ``(eql var x)``)
along control edges, intersects them at joins, and narrows each reference's
derived type from the constraints in force. Mule's adaptation propagates one
fact per variable (a ``ctype`` value type plus a predicted callable identity)
through the same forward dataflow: facts join at block entries, branch edges
narrow integer intervals from comparisons against constants, loop headers widen
growing intervals after a few visits, and error edges carry the join of every
state inside the raising block. Node result types are derived in the same walk
rather than in a separate bottom-up pass, so one worklist settles both.

Module globals are tracked within one lambda only: a store of a known function
object proves the binding's identity until any operation that could run user
code, and reading an unproven binding yields the module-level prediction that
local-call analysis may guard.
"""

from __future__ import annotations

import heapq
from collections import defaultdict
from collections.abc import Mapping

from mule import ast

from . import fndb
from .annotation_types import InferredReturnContract, ListContract, SetContract
from .constraint_domain import (
    _AS_TUPLE_TYPE,
    _DEAD_EDGE,
    _GLOBAL,
    _IMPORTED_CALLEE_NAME,
    _IS_BUILTIN_SEQUENCE_TYPE,
    _IS_MUTABLE_CONTAINER_TYPE,
    _IS_UNPACK_NODE,
    _RANGE_STOP,
    _SEQ_LEN,
    _SEQ_LEN_BOUND_FACT,
    _UNBOUNDED_TUPLE_FACT,
    BOOLEAN_FACT,
    BOOLEAN_FALSE_FACT,
    BOOLEAN_TRUE_FACT,
    BOOTSTRAP_BUILTIN_TARGETS,
    BOOTSTRAP_CLASS_TARGETS,
    EMPTY_DICT_FACT,
    EMPTY_LIST_FACT,
    EMPTY_SET_FACT,
    FLOAT_FACT,
    NO_FACT,
    NONE_FACT,
    TEXT_FACT,
    UNBOUND_FACT,
    UNKNOWN_FACT,
    UNKNOWN_UNBOUND_FACT,
    Body,
    CallSite,
    Fact,
    LambdaGraph,
    _AllocatedImplementation,
    _fact,
    _InstanceDeclaredField,
    _InstanceFieldType,
    _join_relations_list,
    _tighten_relation,
    _tuple_fact,
    _widen_relations,
    build_lambda_graph,
    join_fact,
    join_states,
    widen_fact,
)
from .constraint_propagate import (
    _BUILD_DICT_TYPE,
    _BUILD_SET_TYPE,
    _RESOLVED_METHOD,
    _ImportedModuleReceiver,
    _LinearUpdate,
    _loop_iterator_var,
    _Propagation,
)
from .ctype import (
    BOOLEAN_FALSE,
    BOOLEAN_TRUE,
    BOTTOM,
    EMPTY_DICT_TYPE,
    EMPTY_LIST_TYPE,
    EMPTY_SET_TYPE,
    FUNCTION_TYPE,
    LIST_TYPE,
    PYTHON_INTEGER,
    REAL,
    TEXT_TYPE,
    UNKNOWN,
    FunctionType,
    InstanceType,
    IntegerType,
    ValueType,
    ValueTypeVisitor,
)
from .fndb import SMALL_INTS, constant_type
from .ir1util import made_function, made_lambda
from .knownfun import defconstrain, defpropagate
from .node import (
    CallKind,
    ClassConstructorTarget,
    Combination,
    ForeignCallTarget,
    FunctionalKind,
    Lambda,
    LambdaVar,
    NodeVisitor,
    Ref,
    Set,
    linked,
)


@defconstrain("%unary_not")
def _constrain_unary_not(
    visitor: _Propagation, node: Combination, state: dict, truth: bool
) -> bool:
    if len(node.args) != 1:
        return False
    narrowed_edge = (
        node.args[0].use().accept(_ConstrainComparison(visitor, state, not truth))
    )
    if narrowed_edge is _DEAD_EDGE:
        state[_DEAD_EDGE] = True
        return True
    if narrowed_edge is not None:
        state.update(narrowed_edge)
        return True
    return False


@defpropagate("%build_tuple")
def _propagate_build_tuple(visitor: _Propagation, node: Combination):
    args = node.args
    arity = len(args)
    elements = tuple(visitor.type_of(arg) for arg in args) if 0 < arity <= 64 else None
    return _tuple_fact(arity, elements)


@defpropagate("%build_list")
def _propagate_build_list(visitor: _Propagation, node: Combination):
    args = node.args
    if not args:
        return EMPTY_LIST_FACT
    list_type = EMPTY_LIST_TYPE
    for arg in args:
        list_type = list_type.with_element(visitor.type_of(arg))
    return _fact(list_type)


@defpropagate("%build_dict")
def _propagate_build_dict(visitor: _Propagation, node: Combination):
    args = node.args
    if not args:
        return EMPTY_DICT_FACT
    if len(args) == 1:
        dict_type, may_call = visitor.type_of(args[0]).accept(_BUILD_DICT_TYPE)
        if may_call:
            visitor.invalidate()
        return _fact(dict_type)
    dict_type = EMPTY_DICT_TYPE
    for key_arg, val_arg in zip(args[0::2], args[1::2], strict=True):
        dict_type = dict_type.with_entry(
            visitor.type_of(key_arg), visitor.type_of(val_arg)
        )
    if any(visitor.type_of(key_arg).may_run_user_code for key_arg in args[0::2]):
        visitor.invalidate()
    return _fact(dict_type)


@defpropagate("%build_set")
def _propagate_build_set(visitor: _Propagation, node: Combination):
    args = node.args
    if not args:
        return EMPTY_SET_FACT
    if len(args) == 1:
        set_type, may_call = visitor.type_of(args[0]).accept(_BUILD_SET_TYPE)
        if may_call:
            visitor.invalidate()
        return _fact(set_type)
    set_type = EMPTY_SET_TYPE
    for arg in args:
        set_type = set_type.with_element(visitor.type_of(arg))
    if any(visitor.type_of(arg).may_run_user_code for arg in args):
        visitor.invalidate()
    return _fact(set_type)


@defpropagate("%range_prepare")
def _propagate_range_prepare(visitor: _Propagation, node: Combination):
    state = linked(node.info, node, "info")
    r_key = visitor.walk.range_stop_keys.get(state)
    if r_key is not None:
        visitor._drop_relations_for(r_key)
        step_pos = False
        if 1 <= len(state.arguments) <= 2:
            step_pos = True
        elif len(state.arguments) == 3:
            st = visitor.type_of(state.arguments[2])
            step_pos = (
                st.fits(PYTHON_INTEGER)
                and not st.may_be_unbound
                and st.minimum is not None
                and st.minimum > 0
            )
        if step_pos:
            stop_lvar = (
                state.arguments[0] if len(state.arguments) == 1 else state.arguments[1]
            )
            stop_fact = visitor.fact_of(stop_lvar)
            visitor.assign(r_key, stop_fact)
            if (
                stop_fact.type.fits(PYTHON_INTEGER)
                and not stop_fact.type.may_be_unbound
            ):
                linear = None
                stop_local = visitor.origins.get(stop_lvar)
                if stop_local is not None and stop_local in visitor.walk.local_vars_set:
                    linear = (
                        visitor.walk.temp_source.get(stop_local, stop_local),
                        0,
                        0,
                    )
                elif len(stop_lvar.uses) == 1:
                    linear = stop_lvar.uses[0].accept(_LinearUpdate(visitor))
                if linear is not None:
                    src_var, d_min, d_max = linear
                    if d_max is not None:
                        _tighten_relation(visitor.relations, r_key, src_var, d_max)
                    if d_min is not None:
                        _tighten_relation(visitor.relations, src_var, r_key, -d_min)
        else:
            visitor.assign(r_key, UNBOUND_FACT)
    if visitor.is_proven_fixnum_range(node):
        return BOOLEAN_TRUE_FACT
    return BOOLEAN_FACT


@defpropagate("len")
def _propagate_len(visitor: _Propagation, node: Combination):
    if len(node.args) == 1:
        arg_type = visitor.type_of(node.args[0])
        if arg_type is BOTTOM:
            return NO_FACT
        if arg_type.accept(_IS_BUILTIN_SEQUENCE_TYPE):
            seq_len = visitor.sequence_length_type(node.args[0])
            if seq_len is not None:
                return _fact(seq_len)
            return _SEQ_LEN_BOUND_FACT
    visitor.invalidate()
    return _SEQ_LEN_BOUND_FACT


@defpropagate("%range_available")
def _propagate_range_available(visitor: _Propagation, node: Combination):
    return BOOLEAN_FACT


@defpropagate("%has_binding")
def _propagate_has_binding(visitor: _Propagation, node: Combination):
    return BOOLEAN_FACT


@defpropagate("%min2")
def _propagate_min2(visitor: _Propagation, node: Combination):
    if len(node.args) != 2:
        return UNKNOWN_FACT
    left = visitor.type_of(node.args[0])
    right = visitor.type_of(node.args[1])
    if left is BOTTOM or right is BOTTOM:
        return NO_FACT
    if not (left.fits(REAL) and right.fits(REAL)):
        visitor.invalidate()
    return _fact(fndb.derive_min2_types(left, right))


@defpropagate("%max2")
def _propagate_max2(visitor: _Propagation, node: Combination):
    if len(node.args) != 2:
        return UNKNOWN_FACT
    left = visitor.type_of(node.args[0])
    right = visitor.type_of(node.args[1])
    if left is BOTTOM or right is BOTTOM:
        return NO_FACT
    if not (left.fits(REAL) and right.fits(REAL)):
        visitor.invalidate()
    return _fact(fndb.derive_max2_types(left, right))


@defpropagate("%math_sqrt")
def _propagate_math_sqrt(visitor: _Propagation, node: Combination):
    if len(node.args) != 1:
        return UNKNOWN_FACT
    arg = visitor.type_of(node.args[0])
    if arg is BOTTOM:
        return NO_FACT
    if arg.may_run_user_code:
        visitor.invalidate()
    return FLOAT_FACT


@defpropagate("%sequence_available")
def _propagate_sequence_available(visitor: _Propagation, node: Combination):
    if len(node.args) == 1 and visitor.type_of(node.args[0]).is_empty_iterator():
        return BOOLEAN_FALSE_FACT
    return BOOLEAN_FACT


@defpropagate("%isinstance")
def _propagate_instance_check(visitor: _Propagation, node: Combination):
    if not node.args:
        return BOOLEAN_FACT
    value_type = visitor.type_of(node.args[0])
    if value_type is BOTTOM:
        return NO_FACT
    class_types = [visitor.type_of(arg) for arg in node.args[1:]]
    if any(cls is BOTTOM for cls in class_types):
        return NO_FACT
    if not all(cls.fits_class() for cls in class_types):
        visitor.invalidate()
        return BOOLEAN_FACT
    if any(value_type.is_instance_of(cls) is True for cls in class_types):
        return BOOLEAN_TRUE_FACT
    return BOOLEAN_FACT


@defpropagate("%range_advance")
def _propagate_range_advance(visitor: _Propagation, node: Combination):
    state = linked(node.info, node, "info")
    return _fact(visitor.range_type(state))


@defpropagate("%unpack")
def _propagate_unpack(visitor: _Propagation, node: Combination):
    args = node.args
    visitor.invalidate()
    seq_contract = visitor.walk.lvar_contracts.get(args[0])
    if seq_contract is not None and node.lvar is not None:
        visitor.walk.lvar_contracts[node.lvar] = seq_contract
    arity = visitor.type_of(args[1]).exact_integer()
    if arity is not None and arity >= 0:
        seq_type = visitor.type_of(args[0])
        if seq_type is BOTTOM:
            return NO_FACT
        unpacked = seq_type.unpacked_tuple_type(arity)
        return _tuple_fact(unpacked.arity, unpacked.elements)
    return _UNBOUNDED_TUPLE_FACT


@defpropagate("%unpack_ref")
def _propagate_unpack_ref(visitor: _Propagation, node: Combination):
    args = node.args
    seq_type = visitor.type_of(args[0])
    if seq_type is BOTTOM:
        return NO_FACT
    index_type = visitor.type_of(args[1])
    static_item = seq_type.flow_getitem_type(index_type)
    if static_item is BOTTOM:
        return NO_FACT
    seq_contract = visitor.walk.lvar_contracts.get(args[0])
    index = index_type.exact_integer()
    tup = seq_type.accept(_AS_TUPLE_TYPE)
    arity = tup.arity if tup is not None else None
    elem_contract = (
        seq_contract.unpack_element_contract(arity, index)
        if seq_contract is not None and arity is not None and index is not None
        else None
    )
    item_type = visitor.apply_element_contract(node, static_item, elem_contract)
    return _fact(item_type)


@defpropagate("%resolve_method")
def _propagate_resolve_method(visitor: _Propagation, node: Combination):
    return None


@defpropagate("%getattr")
def _propagate_getattr(visitor: _Propagation, node: Combination):
    if node.args and node.info is not None:
        return _predicted_field_result(visitor, node, node.info.name)


def _predicted_field_result(visitor, node, name):
    recv_type = visitor.type_of(node.args[0])
    declared = visitor.walk.instance_declared_field(recv_type, name)
    if declared is not None:
        contract, is_unsafe = declared
        if node.lvar is not None and contract.has_element_contracts():
            visitor.walk.lvar_contracts[node.lvar] = contract
        if is_unsafe:
            visitor.apply_element_contract(
                node, UNKNOWN, contract, trusted_static=False
            )
        else:
            visitor.walk.element_contracts.pop(node, None)
        return _fact(contract.fact)
    field_type = visitor.walk.instance_field_type(recv_type, name)
    if field_type is UNKNOWN:
        return None
    if (
        field_type.fits(PYTHON_INTEGER)
        or field_type.fits_text(TEXT_TYPE)
        or field_type.fits_dict()
    ):
        visitor.apply_element_contract(
            node, UNKNOWN, InferredReturnContract(f"attribute {name}", field_type)
        )
    return _fact(field_type)


def _propagate_instance_field(visitor: _Propagation, node: Combination):
    if len(node.args) == 2:
        # IR1 optimization imports this module during initialization.
        from .ir1opt import bound_constant_text

        name = bound_constant_text(node.args[1])
        if name is None:
            name = visitor.type_of(node.args[1]).interned_text()
        if name is not None:
            return _predicted_field_result(visitor, node, name)


def _propagate_set_instance_field(visitor: _Propagation, node: Combination):
    # Releasing the previous reference can run weak-reference callbacks.
    visitor.invalidate()
    return NONE_FACT


@defpropagate("%physical_field_ref")
def _propagate_physical_field_ref(visitor: _Propagation, node: Combination):
    return _predicted_field_result(visitor, node, node.info.name)


@defpropagate("%physical_field_set")
def _propagate_physical_field_set(visitor: _Propagation, node: Combination):
    return _propagate_set_instance_field(visitor, node)


@defpropagate("%make_function")
def _propagate_make_function(visitor: _Propagation, node: Combination):
    args = node.args
    lambda_ = made_lambda(node)
    definition = linked(node.info, node, "info")
    parameters = lambda_.signature.default_parameters
    for value, contract, index in zip(
        args[1:], definition.default_contracts, parameters, strict=True
    ):
        if contract is not None:
            visitor.walk.annotations.check(
                contract,
                visitor.type_of(value),
                definition.location,
                ("local", lambda_, lambda_.vars[index].name),
            )
    return Fact(FunctionType(target=lambda_), lambda_, True)


@defpropagate("%call_resolved")
def _propagate_call_resolved(visitor: _Propagation, node: Combination):
    args = node.args
    resolved = (
        args[0].uses[0].accept(_RESOLVED_METHOD) if len(args[0].uses) == 1 else None
    )
    if resolved is not None:
        receiver_lvar, method_name = resolved
        receiver_type = visitor.type_of(receiver_lvar)
        if receiver_type is BOTTOM:
            return NO_FACT
        call_info = linked(node.info, node, "info")
        has_keywords = bool(call_info.keywords)
        arg_types = tuple(visitor.type_of(arg) for arg in args[1:])
        if any(t is BOTTOM for t in arg_types):
            return NO_FACT
        method = visitor.walk.instance_method(receiver_type, method_name)
        if method is not None:
            # The class's method, bound to the receiver: predicted, not
            # proven, because class attributes stay mutable. Code generation
            # guards the resolved function's identity before calling the
            # method's body directly, as it guards predicted global calls;
            # like those, the prediction seeds the callee's guarded entry
            # contexts but publishes no return type.
            if not any(kw is None for kw in call_info.keywords):
                receiver_class = receiver_type.instance_class()
                seeded_receiver = (
                    (receiver_type.dispatch_domain() or receiver_type)
                    if receiver_type.fits_set()
                    else InstanceType(
                        receiver_class,
                        fixed_slots=visitor.walk.class_facts[
                            receiver_class
                        ].fixed_slots,
                    )
                    if receiver_class is not None
                    else receiver_type
                )
                visitor.walk.method_calls.append(
                    CallSite(
                        method,
                        (
                            _fact(seeded_receiver),
                            *(visitor.fact_of(arg) for arg in args[1:]),
                        ),
                        False,
                        (None, *call_info.keywords) if call_info.keywords else (),
                    )
                )
            visitor.walk.method_targets[node] = method
            if not receiver_type.fits_set():
                visitor.invalidate()
                return UNKNOWN_FACT
        if (
            len(receiver_lvar.uses) == 1
            and visitor.walk.lambda_.policy.safety < 3
            and node.lexenv.policy.safety < 3
        ):
            mod_name = receiver_lvar.uses[0].accept(_ImportedModuleReceiver())
            if mod_name is not None:
                mod_target = (
                    visitor.walk.module_targets.get((mod_name, method_name))
                    if visitor.walk.module_targets is not None
                    else None
                )
                mod_res = UNKNOWN
                node.predicted = mod_target
                if mod_target is not None:
                    mod_res = mod_target.propagate_call(
                        node, visitor, args[1:], arg_types, call_info.keywords
                    )
                    if mod_res is BOTTOM:
                        return NO_FACT
                if (
                    mod_res is UNKNOWN
                    and not has_keywords
                    and visitor.walk.module_returns is not None
                ):
                    mod_res = visitor.walk.module_returns.get(
                        (mod_name, method_name), UNKNOWN
                    )
                visitor.invalidate()
                return _fact(mod_res)
        (
            result_type,
            _updated_receiver,
            _child_arg_index,
            _returns_child,
            may_call,
        ) = receiver_type.method_call(
            method_name,
            arg_types,
            has_keywords,
            visitor.walk.storage_methods,
        )
        if may_call:
            visitor.invalidate()
        recv_contract = visitor.walk.lvar_contracts.get(receiver_lvar)
        res_contract = (
            recv_contract.method_result_contract(method_name, arg_types)
            if recv_contract is not None and not has_keywords
            else None
        )
        result_type = visitor.apply_element_contract(node, result_type, res_contract)
        return _fact(result_type)
    visitor.invalidate()
    return UNKNOWN_FACT


@defpropagate("%extend_arguments")
def _propagate_extend_arguments(visitor: _Propagation, node: Combination):
    args = node.args
    if len(args) != 2:
        return None
    recv_var = visitor.origins.get(args[0])
    recv_type = visitor.type_of(args[0])
    seq_type = visitor.type_of(args[1])
    tup = seq_type.accept(_AS_TUPLE_TYPE)
    if recv_var is not None and recv_type == EMPTY_LIST_TYPE and tup is not None:
        visitor.tuple_list_origins[recv_var] = tup
    visitor.invalidate()
    return NONE_FACT


@defpropagate("%tuple_from_list")
def _propagate_tuple_from_list(visitor: _Propagation, node: Combination):
    args = node.args
    if len(args) != 1:
        return None
    src_var = visitor.origins.get(args[0])
    if src_var is not None and src_var in visitor.tuple_list_origins:
        tup = visitor.tuple_list_origins[src_var]
        return _tuple_fact(tup.arity, tup.elements)
    list_type = visitor.type_of(args[0])
    elem_type = list_type.iterate_type().next_item_type()
    if elem_type is BOTTOM:
        return _tuple_fact(0, ())
    if elem_type is not UNKNOWN:
        return _tuple_fact(1, (elem_type,))
    return _UNBOUNDED_TUPLE_FACT


def _propagate_call_with_helper(
    visitor: _Propagation, node: Combination, *, expanded_positional: bool
):
    args = node.args
    if not args:
        return None
    callee_lvar = args[0]
    if expanded_positional and len(args) == 2:
        seq_type = visitor.type_of(args[1])
        if seq_type is BOTTOM:
            return NO_FACT
        tup = seq_type.accept(_AS_TUPLE_TYPE)
        if tup is not None and tup.elements is not None:
            arg_types = tup.elements
            keywords: tuple[str | None, ...] = ()
        elif tup is not None and tup.arity == 0:
            arg_types = ()
            keywords = ()
        else:
            arg_types = (seq_type,)
            keywords = ("*",)
    else:
        arg_types = tuple(visitor.type_of(arg) for arg in args[1:])
        keywords = ("*" * len(arg_types),)
    callee_fact = visitor.fact_of(callee_lvar)
    target = (
        callee_fact.target
        or visitor.walk.imported_target(callee_lvar)
        or visitor.walk.builtin_target(callee_lvar)
    )
    resolved = visitor.walk.resolve_call(
        callee_fact,
        arg_types,
        node.source,
        keywords,
    )
    if resolved is BOTTOM:
        return NO_FACT
    if resolved is UNKNOWN and target is not None:
        resolved = target.derive_return_type(arg_types, keywords)
        if resolved is BOTTOM:
            return NO_FACT
    if resolved is UNKNOWN and visitor.walk.imported_returns:
        callee_name = callee_lvar.use().accept(_IMPORTED_CALLEE_NAME)
        if callee_name is not None:
            resolved = visitor.walk.imported_returns.get(callee_name, UNKNOWN)
    builtin_type = UNKNOWN
    if resolved is UNKNOWN:
        builtin_type = visitor.walk.builtin_return_type(
            callee_lvar, arg_types, keywords
        )
        if builtin_type is BOTTOM:
            return NO_FACT
        resolved = builtin_type
    visitor.invalidate()
    return _fact(resolved)


@defpropagate("%call_expanded")
def _propagate_call_expanded(visitor: _Propagation, node: Combination):
    return _propagate_call_with_helper(visitor, node, expanded_positional=False)


@defpropagate("%call_expanded_positional")
def _propagate_call_expanded_positional(visitor: _Propagation, node: Combination):
    return _propagate_call_with_helper(visitor, node, expanded_positional=True)


@defpropagate("%import")
def _propagate_import(visitor: _Propagation, node: Combination):
    visitor.invalidate()
    return UNKNOWN_FACT


@defpropagate("%catch_exception")
def _propagate_catch_exception(visitor: _Propagation, node: Combination):
    visitor.invalidate()
    return UNKNOWN_FACT


@defpropagate("%format")
def _propagate_format(visitor: _Propagation, node: Combination):
    visitor.invalidate()
    return TEXT_FACT


@defpropagate("%getitem")
def _propagate_getitem(visitor: _Propagation, node: Combination):
    args = node.args
    receiver_type = visitor.type_of(args[0])
    index_type = visitor.type_of(args[1])
    if receiver_type is BOTTOM or index_type is BOTTOM:
        visitor.walk.in_bounds_nodes.discard(node)
        return NO_FACT
    static_item = receiver_type.flow_getitem_type(index_type)
    if static_item is BOTTOM:
        visitor.walk.in_bounds_nodes.discard(node)
        return NO_FACT
    if (
        not receiver_type.accept(_IS_BUILTIN_SEQUENCE_TYPE)
        and receiver_type.may_run_user_code
    ) or index_type.may_run_user_code:
        visitor.invalidate()
    if index_type.fits(PYTHON_INTEGER) and not index_type.may_be_unbound:
        visitor.check_in_bounds_access(node, args[0], args[1])
        bounds = receiver_type.valid_index_bounds()
        if bounds is not None:
            visitor.prove_argument_type(args[1], IntegerType(bounds[0], bounds[1]))
    else:
        visitor.walk.in_bounds_nodes.discard(node)
    recv_contract = visitor.walk.lvar_contracts.get(args[0])
    elem_contract = (
        recv_contract.getitem_contract(index_type)
        if recv_contract is not None
        else None
    )
    item_type = visitor.apply_element_contract(
        node,
        static_item,
        elem_contract,
        trusted_static=not receiver_type.accept(_IS_MUTABLE_CONTAINER_TYPE),
    )
    return _fact(item_type)


@defpropagate("%getslice")
def _propagate_getslice(visitor: _Propagation, node: Combination):
    args = node.args
    if not args:
        return None
    if any(visitor.type_of(arg) is BOTTOM for arg in args):
        return NO_FACT
    receiver_type = visitor.type_of(args[0])
    if (
        not receiver_type.accept(_IS_BUILTIN_SEQUENCE_TYPE)
        and receiver_type.may_run_user_code
    ) or any(visitor.type_of(arg).may_run_user_code for arg in args[1:]):
        visitor.invalidate()
    recv_contract = visitor.walk.lvar_contracts.get(args[0])
    if (
        recv_contract is not None
        and node.lvar is not None
        and recv_contract.has_element_contracts()
    ):
        visitor.walk.lvar_contracts[node.lvar] = recv_contract
    arg_types = [visitor.type_of(arg) for arg in args[1:4]]
    return _fact(receiver_type.getslice_type(*arg_types))


@defpropagate("%setitem")
def _propagate_setitem(visitor: _Propagation, node: Combination):
    args = node.args
    if len(args) != 3:
        return None
    recv_type = visitor.type_of(args[0])
    key_type = visitor.type_of(args[1])
    if not recv_type.fits_list(LIST_TYPE) or key_type.may_run_user_code:
        visitor.invalidate()
    if recv_type.fits_list(LIST_TYPE):
        is_int_index = key_type.fits(PYTHON_INTEGER) and not key_type.may_be_unbound
        if is_int_index:
            visitor.check_in_bounds_access(node, args[0], args[1])
            bounds = recv_type.valid_index_bounds()
            if bounds is not None:
                visitor.prove_argument_type(args[1], IntegerType(bounds[0], bounds[1]))
        else:
            visitor.walk.in_bounds_nodes.discard(node)
    else:
        visitor.walk.in_bounds_nodes.discard(node)
    return NONE_FACT


@defpropagate("%context_enter")
def _propagate_context_enter(visitor: _Propagation, node: Combination):
    args = node.args
    if len(args) != 1:
        return None
    operand_type = visitor.type_of(args[0])
    if operand_type is BOTTOM:
        return NO_FACT
    visitor.invalidate()
    if operand_type.fits_iterator() and operand_type.is_stream:
        return _fact(operand_type)
    return UNKNOWN_FACT


@defpropagate("%iterate")
def _propagate_iterate(visitor: _Propagation, node: Combination):
    args = node.args
    if len(args) != 1:
        return None
    operand_type = visitor.type_of(args[0])
    if operand_type is BOTTOM:
        return NO_FACT
    if (
        not operand_type.accept(_IS_BUILTIN_SEQUENCE_TYPE)
        and operand_type.may_run_user_code
    ):
        visitor.invalidate()
    recv_contract = visitor.walk.lvar_contracts.get(args[0])
    if recv_contract is not None and node.lvar is not None:
        iter_elem = recv_contract.iter_element_contract()
        if iter_elem is not None:
            visitor.walk.iter_contracts[node.lvar] = iter_elem
    return _fact(operand_type.flow_iterate_type())


@defpropagate("%next_item")
def _propagate_next_item(visitor: _Propagation, node: Combination):
    iterator_var = _loop_iterator_var(node.block)
    if iterator_var is not None and iterator_var in visitor.state:
        iter_type = visitor.state[iterator_var].type
        if iter_type.is_empty_iterator():
            return NO_FACT
        item_type = iter_type.next_item_type()
        if iter_type.may_run_user_code:
            visitor.invalidate()
        elem_contract = visitor.walk.iter_contracts.get(iterator_var)
        if elem_contract is not None:
            if (
                node.lvar is not None
                and node.lvar.dest is not None
                and node.lvar.dest.accept(_IS_UNPACK_NODE)
            ):
                visitor.walk.lvar_contracts[node.lvar] = elem_contract
            else:
                item_type = visitor.apply_element_contract(
                    node, item_type, elem_contract, trusted_static=False
                )
        if item_type is not UNKNOWN:
            return _fact(item_type)
    visitor.invalidate()
    return UNKNOWN_FACT


class _Walk:
    """One propagation over a lambda under given parameter facts."""

    def __init__(
        self,
        lambda_: Lambda,
        global_targets,
        parameters,
        resolve_call,
        annotations,
        *,
        graph: LambdaGraph | None = None,
        proven_globals: frozenset[str] = frozenset(),
        rebound_globals: frozenset[str] = frozenset(),
        global_constants: Mapping[str, ValueType] | None = None,
        imported_returns: Mapping[str, ValueType] | None = None,
        imported_targets: (Mapping[str, object] | None) = None,
        module_returns: Mapping[tuple[str, str], ValueType] | None = None,
        module_targets: Mapping[tuple[str, str], object] | None = None,
        storage_methods: Mapping[tuple[str, str], ValueType] | None = None,
        builtin_return_types: Mapping[str, ValueType] | None = None,
        builtin_targets: (Mapping[str, object] | None) = None,
        class_facts: Mapping | None = None,
        recursive_targets: frozenset[Lambda] = frozenset(),
        unique_targets: frozenset[Lambda] = frozenset(),
        is_specialized_context: bool = False,
        for_return_derivation: bool = False,
    ):
        self.lambda_ = lambda_
        self.global_targets = global_targets
        self.resolve_call = resolve_call
        self.annotations = annotations
        self.rebound_globals = rebound_globals
        self.global_constants = global_constants
        self.imported_returns = imported_returns
        self.imported_targets = imported_targets
        self.module_returns = module_returns
        self.module_targets = module_targets
        self.storage_methods = storage_methods
        self.builtin_return_types = builtin_return_types
        self.builtin_targets = builtin_targets
        self.class_facts = class_facts or {}
        self.recursive_targets = recursive_targets
        self.unique_targets = unique_targets
        self.is_specialized_context = is_specialized_context
        self.for_return_derivation = for_return_derivation
        # Calls this walk resolved to methods of typed classes.
        self.method_calls: list[CallSite] = []
        self.method_targets: dict[object, Lambda] = {}
        self.facts: dict[object, Fact] = {}
        if graph is None:
            graph = build_lambda_graph(lambda_)
        self.graph = graph
        self.blocks = graph.blocks
        self.block_nodes = graph.block_nodes
        self.handlers = graph.handlers
        self.entry = graph.entry
        self.successors = graph.successors
        self.predecessors = graph.predecessors
        self.consumers = graph.consumers
        self.headers = graph.headers
        self.back_edges = graph.back_edges
        self.global_names = graph.global_names
        self.indirect = graph.indirect
        self.volatile_keys = graph.volatile_keys
        self.thresholds = graph.thresholds
        self.local_vars_tuple = tuple(var for var in lambda_.locals if not var.indirect)
        self.local_vars_set = frozenset(self.local_vars_tuple)
        self.seq_len_keys: dict[LambdaVar, tuple[str, LambdaVar]] = (
            {var: (_SEQ_LEN, var) for var in self.local_vars_tuple}
            if graph.has_seq_length_ops
            else {}
        )
        self.range_stop_keys: dict[object, tuple[str, object]] = {
            rs: (_RANGE_STOP, rs) for rs in graph.range_states
        }
        self.relational_vars_tuple = (
            self.local_vars_tuple
            + tuple(self.seq_len_keys.values())
            + tuple(self.range_stop_keys.values())
        )
        self.relational_vars_set = frozenset(self.relational_vars_tuple)
        self.temp_source: dict[object, object] = {}
        self.local_contracts = {
            var: contract
            for var in lambda_.locals
            if (contract := lambda_.local_annotations.get(var.name)) is not None
            and ("local", lambda_, var.name) not in annotations.ignored
        }
        parameter_facts = dict(zip(lambda_.vars, parameters, strict=True))
        self.initial: dict[object, Fact] = {
            var: _fact(parameter_facts[var]) if var in parameter_facts else UNBOUND_FACT
            for var in lambda_.locals
        }
        for r_key in self.range_stop_keys.values():
            self.initial[r_key] = UNBOUND_FACT
        for var, seq_key in self.seq_len_keys.items():
            if var in parameter_facts:
                p_type = parameter_facts[var]
                exact_len = p_type.exact_length() if not p_type.may_be_unbound else None
                if exact_len is not None:
                    self.initial[seq_key] = _fact(IntegerType(exact_len, exact_len))
                elif p_type.accept(_IS_BUILTIN_SEQUENCE_TYPE):
                    self.initial[seq_key] = _SEQ_LEN_BOUND_FACT
                else:
                    self.initial[seq_key] = UNBOUND_FACT
            else:
                self.initial[seq_key] = UNBOUND_FACT
        self.element_contracts: dict[object, object] = {}
        self.in_bounds_nodes: set[object] = set()
        self.var_contracts: dict[object, object] = {
            var: contract
            for var, contract in self.local_contracts.items()
            if var in parameter_facts
            and parameter_facts[var] is not UNKNOWN
            and parameter_facts[var].fits(contract.checked_type)
        }
        self.lvar_contracts: dict[object, object] = {}
        self.iter_contracts: dict[object, object] = {}
        closure = lambda_.environment.closure if lambda_.environment is not None else ()
        for var in closure:
            vtype = var.optimistic_type
            resolved_type = (
                vtype if vtype is not None and vtype is not BOTTOM else UNKNOWN
            )
            if any(s.value is None for s in var.sets):
                resolved_type = resolved_type.with_unbound()
            target = (
                made_function(var.sets[0].value.uses[0])
                if resolved_type is FUNCTION_TYPE
                and len(var.sets) == 1
                and not var.indirect
                and var.home is not None
                and var not in var.home.vars
                and var.sets[0].value is not None
                and len(var.sets[0].value.uses) == 1
                else None
            )
            self.initial[var] = (
                Fact(resolved_type, target, True)
                if target is not None
                else _fact(resolved_type)
            )
        # What user code may leave behind in module globals and variables
        # captured by nested functions.
        invalidated_globals: dict[object, Fact] = {}
        for name in self.global_names:
            const_type = (
                global_constants.get(name, UNKNOWN)
                if global_constants is not None
                else UNKNOWN
            )
            target = global_targets.get(name)
            if target is None and imported_targets is not None:
                target = imported_targets.get(name)
            if target is not None:
                invalidated_globals[(_GLOBAL, name)] = Fact(
                    const_type
                    if const_type is not UNKNOWN
                    else (FUNCTION_TYPE if name in proven_globals else UNKNOWN),
                    target,
                    name in proven_globals,
                )
            else:
                invalidated_globals[(_GLOBAL, name)] = _fact(const_type)
        self.invalidated = {
            **invalidated_globals,
            **{
                var: (
                    self.initial.get(var, UNKNOWN_FACT)
                    if var in closure
                    else (
                        UNKNOWN_UNBOUND_FACT
                        if any(s.value is None for s in var.sets)
                        else UNKNOWN_FACT
                    )
                )
                for var in self.indirect
            },
        }
        self.initial.update(invalidated_globals)

    def imported_target(
        self, fun_lvar
    ) -> Lambda | ClassConstructorTarget | ForeignCallTarget | None:
        if self.imported_targets is None:
            return None
        name = fun_lvar.use().accept(_IMPORTED_CALLEE_NAME)
        if name is None:
            return None
        return self.imported_targets.get(name)

    def instance_method(self, receiver_type: ValueType, name: str) -> Lambda | None:
        """The method ``name`` resolves to on instances of the receiver's
        class, when the receiver is a typed instance and the class defines
        it as a plain function."""
        facts = self.class_facts.get(receiver_type.instance_class())
        if facts is None:
            return None
        return facts.methods.get(name)

    def instance_field_type(self, receiver_type: ValueType, name: str) -> ValueType:
        """The predicted field type on instances of the receiver's class,
        restricted to shapes whose consumers guard their runtime operations."""
        return receiver_type.accept(
            _InstanceFieldType(self.class_facts, self.storage_methods, name)
        )

    def instance_declared_field(self, receiver_type: ValueType, name: str):
        """Return ``(contract, is_unsafe)`` when ``name`` is a declared field
        on the receiver's class."""
        return receiver_type.accept(_InstanceDeclaredField(self.class_facts, name))

    def allocated_instance_type(self, class_type: ValueType) -> ValueType:
        # Physical allocation bypasses __new__. Its result belongs to the
        # implementation class, independently of constructor result predictions.
        targets = tuple(
            target
            for mapping in (
                self.imported_targets,
                self.module_targets,
                self.builtin_targets,
            )
            if mapping is not None
            for target in mapping.values()
        )
        return class_type.allocate_instance_type().accept(
            _AllocatedImplementation(targets)
        )

    def builtin_target(
        self, fun_lvar
    ) -> Lambda | ClassConstructorTarget | ForeignCallTarget | None:
        return self.builtin_target_named(fun_lvar.use().accept(_IMPORTED_CALLEE_NAME))

    def builtin_target_named(
        self, name: str | None
    ) -> Lambda | ClassConstructorTarget | ForeignCallTarget | None:
        """The builtins-unit definition a reference to global ``name``
        resolves to, when this module never binds that global itself."""
        if (
            self.lambda_.kind is FunctionalKind.TOPLEVEL
            or self.lambda_.policy.safety >= 3
            or self.builtin_targets is None
        ):
            return None
        if name is None or name in self.rebound_globals or name in self.global_targets:
            return None
        return self.builtin_targets.get(name)

    def foreign_builtin_function(self, name: str) -> object | None:
        """The separately compiled builtins-unit function global ``name``
        names, as a predicted call target. Calls through the name stay
        guarded by the callee's identity, so a runtime rebinding still runs.
        Builtin constructors and functions defined in this compilation keep
        their declared return derivations rather than a predicted entry."""
        target = self.builtin_target_named(name)
        if target is None or target.as_lambda() is not None:
            return None
        return target if target.as_function() is not None else None

    def builtin_return_type(
        self,
        fun_lvar,
        arg_types: tuple[ValueType, ...],
        keywords: tuple[str | None, ...] = (),
    ) -> ValueType:
        if self.lambda_.policy.safety >= 3:
            return UNKNOWN
        name = fun_lvar.use().accept(_IMPORTED_CALLEE_NAME)
        if self.lambda_.kind is FunctionalKind.TOPLEVEL and self.lambda_.is_protocol:
            return UNKNOWN
        if name is None or name in self.rebound_globals or name in self.global_targets:
            return UNKNOWN
        bootstrap_target = BOOTSTRAP_BUILTIN_TARGETS.get(name)
        if bootstrap_target is not None:
            derived = bootstrap_target.derive_return_type(arg_types, keywords)
            if derived is not UNKNOWN:
                return derived
        if self.builtin_targets is not None:
            target = self.builtin_targets.get(name)
            if target is not None:
                derived = target.derive_return_type(arg_types, keywords)
                if derived is not UNKNOWN:
                    return derived
        if not keywords and self.builtin_return_types is not None:
            return self.builtin_return_types.get(name, UNKNOWN)
        return UNKNOWN

    def element_preserving_builtin_contract(self, fun_lvar, elem_contract):
        if (
            self.lambda_.kind is FunctionalKind.TOPLEVEL
            or self.lambda_.policy.safety >= 3
        ):
            return None
        name = fun_lvar.use().accept(_IMPORTED_CALLEE_NAME)
        if name is None or name in self.rebound_globals or name in self.global_targets:
            return None
        if name in ("list", "sorted"):
            return ListContract("list", elem_contract)
        if name == "set":
            return SetContract("set", "mule_dict_set", elem_contract)
        if name == "frozenset":
            return SetContract("frozenset", "mule_dict_frozenset", elem_contract)
        return None

    def invalidate_globals(self, state):
        """Forget facts that user code may change."""
        if self.invalidated:
            state.update(self.invalidated)

    def narrow_return_parameters(
        self, body: Body, truth: bool
    ) -> tuple[ValueType, ...] | None:
        if not self.lambda_.vars or not body.return_states:
            return None
        # The returned values' predicates read their operands' types from the
        # propagation that produced ``body``; this walk has not run.
        self.facts = body.facts
        joined: list[ValueType] = [BOTTOM] * len(self.lambda_.vars)
        found_any = False
        for use, ret_state, origins, type_of_origins in body.return_states:
            ret_type = body.facts.get(use, NO_FACT).type
            if ret_type is BOTTOM:
                continue
            ret_truth = ret_type.truth_value()
            if (truth and ret_truth is False) or (not truth and ret_truth is True):
                continue
            path_state = ret_state
            if ret_truth is None:
                prop = _Propagation(self, ret_state, origins.copy())
                prop.type_of_origins = type_of_origins.copy()
                narrowed = use.accept(_ConstrainComparison(prop, ret_state, truth))
                if narrowed is _DEAD_EDGE:
                    continue
                if narrowed is not None:
                    path_state = narrowed
            found_any = True
            for i, var in enumerate(self.lambda_.vars):
                var_fact = path_state.get(var, NO_FACT)
                if not var_fact.type.may_be_unbound:
                    joined[i] = joined[i].join(var_fact.type)
        if not found_any:
            return None
        return tuple(joined)

    def run(self) -> Body:
        incoming, edge_states, out_states = {}, {}, {}
        in_relations, edge_relations = {}, {}
        return_block_origins: dict[object, tuple[dict, dict]] = {}
        return_blocks = (
            frozenset(use.block for use in self.lambda_.return_.result.uses)
            if self.lambda_.return_ is not None
            else frozenset()
        )
        widened_keys = defaultdict(set)
        if self.propagate(
            incoming,
            edge_states,
            out_states,
            widened_keys,
            return_blocks,
            return_block_origins,
            in_relations,
            edge_relations,
            widening=True,
        ):
            self.propagate(
                incoming,
                edge_states,
                out_states,
                widened_keys,
                return_blocks,
                return_block_origins,
                in_relations,
                edge_relations,
                widening=False,
            )
        self.resolve_call = None
        returned = BOTTOM
        facts = self.facts
        calls = tuple(
            CallSite(
                (callee := facts.get(node.fun.use(), NO_FACT)).target,
                tuple(facts.get(arg.use(), NO_FACT) for arg in node.args),
                callee.proven,
                node.info.keywords
                if node.info is not None and node.info.keywords
                else (),
            )
            for block, node in self.graph.call_nodes
            if block in incoming
        ) + tuple(self.method_calls)
        self_type: ValueType | None = None
        return_states = ()
        if self.lambda_.return_ is not None:
            self_var = self.lambda_.vars[0] if self.lambda_.vars else None
            joined_self = BOTTOM
            collected_returns = []
            for use in self.lambda_.return_.result.uses:
                if use.block in incoming:
                    returned = returned.join(facts.get(use, NO_FACT).type)
                    if use.block in out_states:
                        origins, type_of_origins = return_block_origins.get(
                            use.block, ({}, {})
                        )
                        collected_returns.append(
                            (use, out_states[use.block], origins, type_of_origins)
                        )
                        if self_var is not None:
                            self_fact = out_states[use.block].get(self_var, NO_FACT)
                            if not self_fact.type.may_be_unbound:
                                joined_self = joined_self.join(self_fact.type)
            return_states = tuple(collected_returns)
            if self_var is not None and joined_self is not BOTTOM:
                self_type = joined_self
        return Body(
            facts,
            returned,
            calls,
            self_type,
            return_states,
            dict(self.method_targets),
            dict(self.element_contracts),
            frozenset(self.in_bounds_nodes),
        )

    def propagate(
        self,
        incoming,
        edge_states,
        out_states,
        widened_keys,
        return_blocks,
        return_block_origins,
        in_relations,
        edge_relations,
        *,
        widening: bool,
    ) -> bool:
        visits = defaultdict(int)
        key_visits = defaultdict(int)
        did_widen = False
        rpo = {block: idx for idx, block in enumerate(self.blocks)}
        initial = (
            [self.entry]
            if widening
            else [block for block in self.blocks if block in incoming]
        )
        current_heap = [(rpo[block], block) for block in initial]
        heapq.heapify(current_heap)
        next_heap: list[tuple[int, object]] = []
        queued = set(initial)
        cross_changed = set()
        visited_edges = set()
        predecessors = self.predecessors
        successors = self.successors
        block_nodes = self.block_nodes
        handlers = self.handlers
        consumers = self.consumers
        headers = self.headers
        back_edges = self.back_edges
        thresholds = self.thresholds
        facts = self.facts
        visitor = _Propagation(self, {}, {})

        edge_constrainer = _ConstrainEdge(None, visitor, {}, edge_states)
        cmp_constrainer = _ConstrainComparison(visitor, {}, False)
        edge_constrainer.cmp_constrainer = cmp_constrainer

        while current_heap or next_heap:
            if not current_heap:
                current_heap, next_heap = next_heap, current_heap
            cur_rpo, block = heapq.heappop(current_heap)
            queued.remove(block)
            operand_changed = block in cross_changed
            if operand_changed:
                cross_changed.remove(block)
            parent_rels = []
            rel_states = []
            for parent in predecessors[block]:
                edge_key = parent, block
                if edge_key in edge_states:
                    parent_rels.append(edge_relations.get(edge_key, {}))
                    rel_states.append(edge_states[edge_key])
            if widening or block not in headers:
                parents = rel_states.copy()
                stale_backs = None
            else:
                parents = []
                stale_backs = []
                for parent in predecessors[block]:
                    edge_key = parent, block
                    if edge_key not in edge_states:
                        continue
                    if edge_key in back_edges and edge_key not in visited_edges:
                        stale_backs.append(edge_states[edge_key])
                    else:
                        parents.append(edge_states[edge_key])
            if block is self.entry:
                parents.append(self.initial)
                parent_rels.append({})
                rel_states.append(self.initial)
            if not parents:
                continue
            rels = _join_relations_list(parent_rels, rel_states)
            if not widening and (block in headers or visits[block] >= 3):
                rels = _widen_relations(in_relations.get(block, {}), rels)
            state = parents[0]
            for parent in parents[1:]:
                state, _ = join_states(state, parent)
            if stale_backs:
                w_keys = widened_keys.get(block)
                if w_keys:
                    state = state.copy()
                    for back_state in stale_backs:
                        for key in w_keys:
                            state[key] = join_fact(state[key], back_state[key])
            previous = incoming.get(block)
            if widening:
                if previous is None:
                    changed = True
                else:
                    state, changed = join_states(previous, state)
                old_rels = in_relations.get(block)
                if old_rels is not None:
                    if (block in headers and visits[block] >= 2) or visits[block] >= 4:
                        rels = _widen_relations(old_rels, rels)
                    else:
                        rels = _join_relations_list([old_rels, rels], [previous, state])
                rel_changed = rels != old_rels
                if rel_changed:
                    in_relations[block] = rels
                if changed or rel_changed:
                    visits[block] += 1
                    if changed and block in headers and block in incoming:
                        old_in = incoming[block]
                        widened = {}
                        for key, val in state.items():
                            old_val = old_in[key]
                            if (
                                val is not old_val
                                and val != old_val
                                and any(
                                    (pred, block) in back_edges
                                    and (pred, block) in edge_states
                                    and join_fact(
                                        old_val, edge_states[pred, block][key]
                                    )
                                    is not old_val
                                    for pred in predecessors[block]
                                )
                            ):
                                key_visits[block, key] += 1
                                widened_keys[block].add(key)
                                if key_visits[block, key] in (3, 4):
                                    w_val = widen_fact(old_val, val, thresholds)
                                elif key_visits[block, key] > 4:
                                    w_val = widen_fact(old_val, val)
                                else:
                                    w_val = val
                                if (
                                    key in self.local_vars_set
                                    and key.type is not None
                                    and w_val.type.fits(PYTHON_INTEGER)
                                ):
                                    w_val = Fact(
                                        w_val.type.annotated_integer(key.type),
                                        w_val.target,
                                        w_val.proven,
                                    )
                                widened[key] = w_val
                            else:
                                widened[key] = val
                        if widened != state:
                            did_widen = True
                            state = widened
                    incoming[block] = state
                elif not operand_changed:
                    continue
            else:
                old_rels = in_relations.get(block)
                if visits[block] >= 3 and previous is not None:
                    state, state_changed = join_states(previous, state)
                    if state_changed and block in headers:
                        new_state = {}
                        for k, v in state.items():
                            if v is not previous[k] and v != previous[k]:
                                w_v = widen_fact(previous[k], v)
                                if (
                                    k in self.local_vars_set
                                    and k.type is not None
                                    and w_v.type.fits(PYTHON_INTEGER)
                                ):
                                    w_v = Fact(
                                        w_v.type.annotated_integer(k.type),
                                        w_v.target,
                                        w_v.proven,
                                    )
                                new_state[k] = w_v
                            else:
                                new_state[k] = v
                        state = new_state
                else:
                    state_changed = state is not previous and state != previous
                rel_changed = rels != old_rels
                if visits[block] == 0 or state_changed or rel_changed:
                    visits[block] += 1
                    incoming[block] = state
                    in_relations[block] = rels
                else:
                    if previous is not None:
                        state = previous
                    if not operand_changed:
                        continue
            # The visitor copies its state on first mutation.
            handler = handlers[block]
            has_handler = handler is not None
            visitor.reset(state, has_handler, in_relations.get(block))
            terminated = False
            for node in block_nodes[block]:
                fact = node.accept(visitor)
                if fact is not None:
                    if fact is NO_FACT:
                        terminated = True
                    previous_fact = facts.get(node, NO_FACT)
                    if widening:
                        # join_fact returns ``previous_fact`` itself when nothing changed.
                        updated = join_fact(previous_fact, fact)
                        changed = updated is not previous_fact
                    else:
                        updated = fact
                        changed = (
                            updated is not previous_fact and updated != previous_fact
                        )
                    if changed:
                        facts[node] = updated
                        for consumer in consumers.get(node, ()):
                            cross_changed.add(consumer)
                            if consumer not in queued:
                                c_rpo = rpo[consumer]
                                heapq.heappush(
                                    current_heap if c_rpo > cur_rpo else next_heap,
                                    (c_rpo, consumer),
                                )
                                queued.add(consumer)
                if has_handler:
                    visitor.note_handler_state()
                if terminated:
                    break
            state = visitor.state
            out_states[block] = state
            if block in return_blocks and not terminated:
                return_block_origins[block] = (
                    visitor.origins.copy(),
                    visitor.type_of_origins.copy(),
                )
            last = block.last
            for target in successors[block]:
                if target is handler:
                    if visitor.handler_state is not None:
                        edge = visitor.handler_state
                        self.invalidate_globals(edge)
                    elif visitor.globals_invalidated or not self.invalidated:
                        edge = state
                    else:
                        edge = state.copy()
                        self.invalidate_globals(edge)
                    edge_rel = (
                        visitor.handler_relations
                        if visitor.handler_relations is not None
                        else visitor.relations.copy()
                    )
                elif terminated:
                    continue
                elif last is None:
                    edge = state
                    edge_rel = visitor.relations.copy()
                else:
                    visitor.edge_relations = None
                    edge_constrainer.target = target
                    edge_constrainer.state = state
                    edge = last.accept(edge_constrainer)
                    if edge is None:
                        continue
                    edge_rel = (
                        visitor.edge_relations
                        if visitor.edge_relations is not None
                        else visitor.relations.copy()
                    )
                key = block, target
                if not widening:
                    visited_edges.add(key)
                old_edge = edge_states.get(key)
                old_rel = edge_relations.get(key)
                edge_relations[key] = edge_rel
                if (
                    old_edge is None
                    or (old_edge is not edge and old_edge != edge)
                    or old_rel != edge_rel
                ):
                    edge_states[key] = edge
                    if target not in queued:
                        t_rpo = rpo[target]
                        heapq.heappush(
                            current_heap
                            if t_rpo > cur_rpo and key not in back_edges
                            else next_heap,
                            (t_rpo, target),
                        )
                        queued.add(target)
        return did_widen

    def constrain_edge(self, block, target, visitor, state, edge_states=None):
        """Narrow an integer local on a branch edge tested against a constant,
        or return ``None`` when a constant boolean test makes the edge dead."""
        last = block.last
        if last is not None:
            return last.accept(_ConstrainEdge(target, visitor, state, edge_states))
        return state


class _NarrowTruthiness(ValueTypeVisitor):
    """Narrow a value type along a direct truthiness branch edge."""

    def __init__(self, truth: bool):
        self.truth = truth

    def visit_ValueType(self, fact):
        if self.truth:
            return fact.narrow_none(False)
        return fact

    def visit_UnionType(self, fact):
        if fact.may_be_unbound:
            return fact
        return fact.map(
            lambda member: (
                BOTTOM
                if member.truth_value() is not None
                and member.truth_value() != self.truth
                else member.accept(self)
            )
        )

    def visit_NoneType(self, fact):
        return fact

    def visit_BooleanType(self, fact):
        if not fact.may_be_unbound:
            return BOOLEAN_TRUE if self.truth else BOOLEAN_FALSE
        return fact

    def visit_IntegerType(self, fact):
        if not fact.may_be_unbound:
            if (
                not self.truth
                and fact.minimum is not None
                and fact.minimum <= 0
                and (fact.maximum is None or fact.maximum >= 0)
            ):
                return SMALL_INTS[0]
            if self.truth:
                if fact.minimum == 0 and (fact.maximum is None or fact.maximum > 0):
                    return fact.narrow(ast.ComparisonOperator.GREATER, 0, True)
                if fact.maximum == 0 and (fact.minimum is None or fact.minimum < 0):
                    return fact.narrow(ast.ComparisonOperator.LESS, 0, True)
        return fact


_TRUE_NARROWING = _NarrowTruthiness(True)
_FALSE_NARROWING = _NarrowTruthiness(False)


class _ConstrainEdge(NodeVisitor):
    """Only a two-way branch narrows its edges."""

    def __init__(self, target, visitor: _Propagation, state, edge_states=None):
        self.target = target
        self.visitor = visitor
        self.state = state
        self.edge_states = edge_states
        self.cmp_constrainer: _ConstrainComparison | None = None

    def visit_Node(self, node):
        return self.state

    def visit_If(self, node):
        if node.consequent is node.alternative:
            return self.state
        truth = self.target is node.consequent
        test_type = self.visitor.type_of(node.test)
        if test_type is BOTTOM:
            return None
        test_truth = test_type.truth_value()
        if (
            (truth and test_truth is False) or (not truth and test_truth is True)
        ) and not (
            node.block in self.visitor.walk.headers
            and self.visitor.origins.get(node.test) in self.visitor.walk.volatile_keys
        ):
            return None
        cmp_c = self.cmp_constrainer
        if cmp_c is not None:
            cmp_c.state = self.state
            cmp_c.truth = truth
        else:
            cmp_c = _ConstrainComparison(self.visitor, self.state, truth)
        narrowed_edge = node.test.use().accept(cmp_c)
        if narrowed_edge is _DEAD_EDGE:
            return None
        if narrowed_edge is not None:
            return narrowed_edge
        if not self.visitor.origins:
            return self.state
        local = self.visitor.origins.get(node.test)
        if local is not None and local in self.visitor.state:
            walk = self.visitor.walk
            nodes = walk.block_nodes.get(node.block, ())
            test_ref = node.test.use()
            if (
                self.edge_states is not None
                and len(nodes) == 2
                and nodes[0] is test_ref
                and nodes[1] is node
                and local not in walk.indirect
                and node.block is not walk.entry
                and node.block not in walk.headers
            ):
                pending_edges = [
                    (pred, node.block)
                    for pred in walk.predecessors.get(node.block, ())
                    if (pred, node.block) in self.edge_states
                ]
                seen_edges = set(pending_edges)
                feasible = []
                filtered = False
                while pending_edges:
                    u, v = pending_edges.pop()
                    if (
                        len(walk.block_nodes.get(u, ())) == 1
                        and u is not walk.entry
                        and u not in walk.headers
                    ):
                        in_edges = [
                            (gp, u)
                            for gp in walk.predecessors.get(u, ())
                            if (gp, u) in self.edge_states
                        ]
                        if in_edges:
                            for edge_key in in_edges:
                                if edge_key not in seen_edges:
                                    seen_edges.add(edge_key)
                                    pending_edges.append(edge_key)
                            continue
                    pred_edge = self.edge_states[u, v]
                    pred_fact = pred_edge.get(local)
                    pred_truth = (
                        pred_fact.type.truth_value() if pred_fact is not None else None
                    )
                    if (truth and pred_truth is False) or (
                        not truth and pred_truth is True
                    ):
                        filtered = True
                    else:
                        feasible.append(pred_edge)
                if filtered:
                    if not feasible:
                        return None
                    edge = feasible[0].copy()
                    for pred_edge in feasible[1:]:
                        edge, _ = join_states(edge, pred_edge)
                    if test_ref.final:
                        edge[local] = UNBOUND_FACT
                    else:
                        current = edge[local]
                        narrowed = current.type.accept(
                            _TRUE_NARROWING if truth else _FALSE_NARROWING
                        )
                        if narrowed is BOTTOM:
                            return None
                        if narrowed != current.type:
                            edge[local] = self.visitor.narrowed_fact(current, narrowed)
                    return edge
            current = self.visitor.state[local]
            narrowed = current.type.accept(
                _TRUE_NARROWING if truth else _FALSE_NARROWING
            )
            if narrowed is BOTTOM:
                return None
            if narrowed != current.type:
                edge = self.state.copy()
                edge[local] = self.visitor.narrowed_fact(current, narrowed)
                return edge
        return self.state


class _PrecedingLeafSet(NodeVisitor):
    def __init__(self, leaf):
        self.leaf = leaf

    def visit_Node(self, node):
        return None

    def visit_Set(self, node: Set):
        if (
            node.var is self.leaf
            and node.value is not None
            and len(node.value.uses) == 1
        ):
            return node.value.uses[0]
        return None


class _ConstrainComparison(NodeVisitor):
    """A known comparison or predicate call on a branch edge narrows its operands."""

    def __init__(self, visitor: _Propagation, state, truth: bool):
        self.visitor = visitor
        self.state = state
        self.truth = truth

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        if node.prev is not None and node.prev.use is not None:
            producer = node.prev.use.accept(_PrecedingLeafSet(node.leaf))
            if producer is not None and producer.block is node.block:
                return producer.accept(self)
        return None

    def visit_Combination(self, node):
        if node.kind is not CallKind.KNOWN:
            target = self.visitor.resolve_call_target(node)
            if target is not None:
                edge = self.state.copy()
                if target.constrain_call(node, self.visitor, edge, self.truth):
                    return _DEAD_EDGE if edge.get(_DEAD_EDGE) else edge
            return None
        info = node.fun_info
        if info is not None and info.constrain_branch is not None:
            edge = self.state.copy()
            if info.constrain_branch(self.visitor, node, edge, self.truth):
                return _DEAD_EDGE if edge.get(_DEAD_EDGE) else edge
        return None


def propagate(
    lambda_: Lambda,
    global_targets,
    parameters,
    resolve_call,
    annotations,
    *,
    graph: LambdaGraph | None = None,
    proven_globals: frozenset[str] = frozenset(),
    rebound_globals: frozenset[str] = frozenset(),
    global_constants: Mapping[str, ValueType] | None = None,
    imported_returns: Mapping[str, ValueType] | None = None,
    imported_targets: (Mapping[str, object] | None) = None,
    module_returns: Mapping[tuple[str, str], ValueType] | None = None,
    module_targets: Mapping[tuple[str, str], object] | None = None,
    storage_methods: Mapping[tuple[str, str], ValueType] | None = None,
    builtin_return_types: Mapping[str, ValueType] | None = None,
    builtin_targets: (Mapping[str, object] | None) = None,
    class_facts: Mapping | None = None,
    recursive_targets: frozenset[Lambda] = frozenset(),
    unique_targets: frozenset[Lambda] = frozenset(),
    is_specialized_context: bool = False,
    for_return_derivation: bool = False,
) -> Body:
    """Propagate facts through ``lambda_`` from the given parameter types.

    ``resolve_call(callee_fact, argument_types, location, keywords)`` supplies
    the result type of a call; local-call analysis provides it so that proven
    local calls receive their callee's published return type. Annotation
    contradictions are recorded in ``annotations``, the module's
    ``AnnotationLedger``.
    """
    return _Walk(
        lambda_,
        global_targets,
        parameters,
        resolve_call,
        annotations,
        graph=graph,
        proven_globals=proven_globals,
        rebound_globals=rebound_globals,
        global_constants=global_constants,
        imported_returns=imported_returns,
        imported_targets=imported_targets,
        module_returns=module_returns,
        module_targets=module_targets,
        storage_methods=storage_methods,
        builtin_return_types=builtin_return_types,
        builtin_targets=builtin_targets,
        class_facts=class_facts,
        recursive_targets=recursive_targets,
        unique_targets=unique_targets,
        is_specialized_context=is_specialized_context,
        for_return_derivation=for_return_derivation,
    ).run()


__all__ = [
    "BOOTSTRAP_BUILTIN_TARGETS",
    "BOOTSTRAP_CLASS_TARGETS",
    "NO_FACT",
    "UNBOUND_FACT",
    "UNKNOWN_FACT",
    "UNKNOWN_UNBOUND_FACT",
    "Body",
    "CallSite",
    "Fact",
    "LambdaGraph",
    "_InstanceFieldType",
    "_Propagation",
    "_Walk",
    "_fact",
    "build_lambda_graph",
    "constant_type",
    "join_fact",
    "join_states",
    "propagate",
    "widen_fact",
]
