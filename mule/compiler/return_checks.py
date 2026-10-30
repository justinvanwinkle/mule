"""Checked definition-derived call results, following SBCL assert-call-type."""

from collections import defaultdict

from . import fndb
from .annotation_types import (
    ContractVisitor,
    DeclaredFieldContract,
    DeclaredReturnContract,
    InferredReturnContract,
)
from .ctype import (
    BOTTOM,
    LIST_TYPE,
    UNKNOWN,
    FunctionType,
    InstanceType,
    ListType,
    TupleType,
    ValueType,
    ValueTypeVisitor,
)
from .ir1util import insert_cast_after, insert_cast_before
from .locall import global_bindings, made_function
from .node import (
    CallKind,
    Cast,
    Combination,
    FunctionalKind,
    Lambda,
    LambdaVar,
    LeafVisitor,
    NodeVisitor,
    Ref,
)


def assert_inferred_results(compilation) -> bool:
    """Expose definition-derived results only through an ordinary checked Cast."""
    definitions = global_bindings(compilation.lambdas, unique_definitions=True)
    method_collector = _ModuleMethods()
    visited_components = set()
    for function in compilation.lambdas:
        comp = function.component
        if comp is None or comp in visited_components:
            continue
        visited_components.add(comp)
        for block in comp.blocks():
            for node in block.nodes():
                node.accept(method_collector)
    changed = False
    for function in compilation.lambdas:
        if not function.policy.infers_return_contracts:
            continue
        visitor = _AssertResult(
            function,
            compilation.facts,
            definitions,
            method_collector.methods,
            method_collector.shadowed,
        )
        for block in list(function.component.blocks()):
            for node in list(block.nodes()):
                node.accept(visitor)
        changed |= visitor.changed
    return changed


class _ModuleMethods(NodeVisitor):
    def __init__(self):
        self.methods: dict[tuple[str, str], list[Lambda]] = defaultdict(list)
        self.shadowed: set[tuple[str, str]] = set()

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is fndb.CLASS_STORE
            and len(node.args) == 2
            and node.info is not None
        ):
            key = (node.lexenv.lambda_.namespace, node.info.name)
            lam = (
                made_function(node.args[1].uses[0])
                if len(node.args[1].uses) == 1
                else None
            )
            if lam is not None and node.info.name not in (
                "__init_subclass__",
                "__class_getitem__",
            ):
                if lam not in self.methods[key]:
                    self.methods[key].append(lam)
            else:
                self.shadowed.add(key)


class _ResolvedMethodCall(NodeVisitor):
    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is fndb.RESOLVE_METHOD
            and len(node.args) == 1
            and node.info is not None
        ):
            return node.args[0], node.info.name
        return None


_RESOLVED_METHOD_CALL = _ResolvedMethodCall()


class _DefinitionTarget(NodeVisitor, LeafVisitor):
    def __init__(self, definitions):
        self.definitions = definitions

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Ref(self, node):
        return node.leaf.accept(self)

    def visit_GlobalVar(self, leaf):
        return self.definitions.get(leaf.namespace, {}).get(leaf.name)


class _AssertionDestination(NodeVisitor):
    def __init__(self, call):
        self.call = call

    def visit_Node(self, node):
        return False

    def visit_Return(self, node):
        return True

    def visit_Cast(self, node):
        return node.declaration == ("inferred-return", self.call)


def _target_inferred_return_type(
    target: Lambda,
    target_facts,
    arguments: tuple[ValueType, ...],
    keywords: tuple[str | None, ...],
) -> ValueType:
    if not keywords and len(arguments) == len(target.vars):
        inferred = target.derive_return_type(arguments, keywords)
        if inferred is not UNKNOWN and inferred is not BOTTOM:
            return inferred
    if target_facts is not None:
        inferred = target_facts.return_type
        if not keywords:
            for variant in target_facts.variants:
                if (
                    len(arguments) == len(variant.parameters)
                    and all(
                        actual.fits(expected)
                        for actual, expected in zip(
                            arguments, variant.parameters, strict=True
                        )
                    )
                    and variant.return_type is not BOTTOM
                    and (inferred is UNKNOWN or variant.return_type.fits(inferred))
                ):
                    inferred = variant.return_type
        if inferred is not UNKNOWN and inferred is not BOTTOM:
            return inferred
    return target.derive_return_type(arguments, keywords)


class _ContractPrediction(ValueTypeVisitor):
    """Forget literal identities without discarding checked numeric intervals."""

    def visit_ValueType(self, fact):
        return fact.widen_constant()

    def visit_IntegerType(self, fact):
        return fact.widen_constant() if fact.exact_integer() is not None else fact

    def visit_BooleanType(self, fact):
        return fact.widen_constant()

    def visit_TupleType(self, fact):
        return TupleType(
            fact.arity,
            None
            if fact.elements is None
            else tuple(element.accept(self) for element in fact.elements),
            fact.may_be_unbound,
        )

    def visit_ListType(self, fact):
        if fact.element is BOTTOM or fact.element is UNKNOWN:
            return LIST_TYPE if not fact.may_be_unbound else ListType(UNKNOWN, True)
        elem = fact.element.accept(self)
        if elem is UNKNOWN and not fact.may_be_unbound:
            return LIST_TYPE
        return ListType(elem, fact.may_be_unbound)

    def visit_UnionType(self, fact):
        result = BOTTOM
        for member in fact.members:
            result = result.join(member.accept(self))
        return result


_CONTRACT_PREDICTION = _ContractPrediction()


def _target_return_contract(
    target: Lambda,
    target_facts,
    arguments: tuple[ValueType, ...],
    keywords: tuple[str | None, ...],
    *,
    widen_constant: bool = True,
):
    annotation = target.signature.implementation_return_annotation
    declared = None if annotation is None else annotation.definition_contract()
    inferred = _target_inferred_return_type(target, target_facts, arguments, keywords)
    if inferred is BOTTOM or inferred.may_be_unbound:
        inferred = UNKNOWN
    elif widen_constant or declared is not None:
        inferred = inferred.accept(_CONTRACT_PREDICTION)
    if declared is not None:
        return DeclaredReturnContract(target.name, declared, inferred)
    if inferred is UNKNOWN:
        return None
    return InferredReturnContract(target.name, inferred)


class _ReceiverInstanceClasses(ValueTypeVisitor):
    def visit_ValueType(self, fact):
        return None

    def visit_NoneType(self, fact):
        return frozenset() if not fact.may_be_unbound else None

    def visit_InstanceType(self, fact):
        return frozenset({fact.identity}) if not fact.may_be_unbound else None

    def visit_UnionType(self, fact):
        if fact.may_be_unbound:
            return None
        classes: set[str] = set()
        for member in fact.members:
            member_classes = member.accept(self)
            if member_classes is None:
                return None
            classes.update(member_classes)
        return frozenset(classes)


_RECEIVER_INSTANCE_CLASSES = _ReceiverInstanceClasses()


class _AssertResult(NodeVisitor):
    def __init__(self, function, all_facts, definitions, module_methods, shadowed):
        self.function = function
        self.all_facts = all_facts
        self.facts = all_facts[function]
        self.target = _DefinitionTarget(definitions)
        self.module_methods = module_methods
        self.shadowed = shadowed
        self.changed = False

    def lvar_type(self, lvar):
        result = BOTTOM
        for producer in lvar.uses:
            ptype = self.facts.types.get(producer, UNKNOWN)
            if (
                ptype is UNKNOWN
                and (target := self.facts.targets.get(producer)) is not None
                and (lam := target.as_lambda()) is not None
            ):
                ptype = FunctionType(target=lam)
            result = result.join(ptype)
        return result

    def _resolved_method_contract(self, call: Combination):
        if not call.args or len(call.args[0].uses) != 1:
            return None
        resolved = call.args[0].uses[0].accept(_RESOLVED_METHOD_CALL)
        if resolved is None:
            return None
        receiver_lvar, method_name = resolved
        receiver_type = self.lvar_type(receiver_lvar).accept(_CONTRACT_PREDICTION)
        arg_types = tuple(
            self.lvar_type(arg).accept(_CONTRACT_PREDICTION) for arg in call.args[1:]
        )
        arguments = (receiver_type, *arg_types)
        keywords = call.info.keywords
        target = self.facts.targets.get(call)
        if target is not None:
            target_lam = target.as_lambda()
            if (
                target_lam is None
                or target_lam.namespace != self.function.namespace
                or not target_lam.policy.infers_return_contracts
            ):
                return None
            return _target_return_contract(
                target_lam,
                self.all_facts.get(target_lam),
                arguments,
                keywords,
            )
        method_key = (self.function.namespace, method_name)
        if method_key in self.shadowed:
            return None
        candidates = self.module_methods.get(method_key, ())
        receiver_classes = receiver_type.accept(_RECEIVER_INSTANCE_CLASSES)
        if not candidates or not receiver_classes:
            return None
        joined_inferred = BOTTOM
        shared_declared = None
        first_candidate = True
        for cand in candidates:
            if (
                cand.namespace != self.function.namespace
                or not cand.policy.infers_return_contracts
                or cand.call_order(keywords, len(arg_types), receiver=True) is None
            ):
                return None
            ann = cand.signature.implementation_return_annotation
            cand_declared = None if ann is None else ann.definition_contract()
            if first_candidate:
                shared_declared = cand_declared
                first_candidate = False
            elif cand_declared != shared_declared:
                shared_declared = None
            cand_args = (
                InstanceType(cand.method_class)
                if cand.method_class is not None
                else (
                    receiver_type
                    if receiver_type.instance_class() is not None
                    else UNKNOWN
                ),
                *arg_types,
            )
            cand_inferred = _target_inferred_return_type(
                cand, self.all_facts.get(cand), cand_args, keywords
            )
            if (
                cand_inferred is UNKNOWN
                or cand_inferred is BOTTOM
                or cand_inferred.may_be_unbound
            ):
                joined_inferred = UNKNOWN
            elif joined_inferred is not UNKNOWN:
                joined_inferred = joined_inferred.join(
                    cand_inferred.accept(_CONTRACT_PREDICTION)
                )
        if shared_declared is not None:
            return DeclaredReturnContract(
                method_name,
                shared_declared,
                UNKNOWN if joined_inferred is BOTTOM else joined_inferred,
            )
        if joined_inferred is UNKNOWN or joined_inferred is BOTTOM:
            return None
        return InferredReturnContract(method_name, joined_inferred)

    def visit_Combination(self, call):
        if not call.lexenv.policy.infers_return_contracts:
            return
        result = call.lvar
        if result is None or result.dest is None:
            return
        if len(result.uses) != 1 or result.dest.accept(_AssertionDestination(call)):
            return
        if (
            call.kind is CallKind.KNOWN
            and call.fun_info is fndb.CALL_RESOLVED
            and not call.proven
        ):
            contract = self._resolved_method_contract(call)
            if contract is None:
                return
        elif call.kind in (CallKind.FULL, CallKind.GUARDED) and not call.proven:
            if len(call.fun.uses) != 1:
                return
            target = call.fun.use().accept(self.target)
            if (
                target is None
                or target.namespace != self.function.namespace
                or not target.policy.infers_return_contracts
            ):
                return
            location = call.info.location
            if target.code is None or location is None:
                return
            if target.code.filename != location[0] or (
                self.function.kind is FunctionalKind.TOPLEVEL
                and target.code.firstlineno > location[1]
            ):
                return
            arguments = tuple(
                self.lvar_type(arg).accept(_CONTRACT_PREDICTION) for arg in call.args
            )
            contract = _target_return_contract(
                target,
                self.all_facts.get(target),
                arguments,
                call.info.keywords,
                widen_constant=False,
            )
            if contract is None:
                return
        else:
            return
        checked = contract.fact
        if checked is UNKNOWN or checked is BOTTOM or checked.may_be_unbound:
            return

        def make_cast(value):
            return Cast(
                call.lexenv,
                call.source,
                value,
                contract,
                call.info.location,
                int(contract.requires_check(call.lexenv.policy.checks_contracts)),
                declaration=("inferred-return", call),
            )

        insert_cast_after(call, make_cast)
        call.block.component.reoptimize = True
        self.changed = True


class _DeclaredFieldLookup(ValueTypeVisitor):
    def __init__(self, class_facts, field_name: str):
        self.class_facts = class_facts
        self.field_name = field_name

    def visit_ValueType(self, fact):
        return None

    def visit_InstanceType(self, fact):
        if fact.may_be_unbound:
            return None
        identity = fact.instance_class()
        if identity is None:
            return None
        facts = self.class_facts.get(identity)
        if facts is None:
            return None
        contract = facts.declared_fields.get(self.field_name)
        if contract is None:
            return None
        return facts.public_name, contract

    def visit_UnionType(self, fact):
        if fact.may_be_unbound or not fact.members:
            return None
        it = iter(fact.members)
        first = next(it).accept(self)
        if first is None:
            return None
        cls_name, contract = first
        for member in it:
            res = member.accept(self)
            if res is None or res[1] != contract:
                return None
        return cls_name, contract


class _ContainerWriteContracts(ContractVisitor):
    """Return ``(key_contract, value_contract)`` for a container field's elements."""

    def visit_Contract(self, contract):
        return None, None

    def visit_ListContract(self, contract):
        return None, contract.element

    def visit_SetContract(self, contract):
        return None, contract.element

    def visit_DictContract(self, contract):
        return contract.key, contract.value


_CONTAINER_WRITE_CONTRACTS = _ContainerWriteContracts()


class _GetattrFieldOrigin(NodeVisitor, LeafVisitor):
    """Return ``(receiver_lvar, field_name)`` when an lvar is read from an
    instance attribute directly or via a single-assignment local variable."""

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Cast(self, node: Cast):
        if node.value is not None and len(node.value.uses) == 1:
            return node.value.uses[0].accept(self)
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        if len(leaf.sets) == 1 and leaf.sets[0].value is not None:
            val = leaf.sets[0].value
            if len(val.uses) == 1:
                return val.uses[0].accept(self)
        return None

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN or node.fun_info is None:
            return None
        if (
            node.fun_info in (fndb.GETATTR, fndb.PHYSICAL_FIELD_REF)
            and len(node.args) == 1
            and node.info is not None
        ):
            return node.args[0], node.info.name
        if node.fun_info.name == "_get_instance_field" and len(node.args) == 2:
            from .ir1opt import bound_constant_text

            name = bound_constant_text(node.args[1])
            if name is not None:
                return node.args[0], name
        return None


_GETATTR_FIELD_ORIGIN = _GetattrFieldOrigin()


class _HasDeclaredFieldCast(NodeVisitor):
    def __init__(self, target_node, contract):
        self.target_node = target_node
        self.contract = contract

    def visit_Node(self, node):
        return False

    def visit_Cast(self, node: Cast):
        return (
            node.declaration == ("field-store", self.target_node)
            or node.contract == self.contract
        )


class _ContainerDisplay(NodeVisitor):
    """Return ``Combination`` when a node is ``%build_list``, ``%build_set``,
    or ``%build_dict``."""

    def visit_Node(self, node):
        return None

    def visit_Cast(self, node: Cast):
        if node.value is not None and len(node.value.uses) == 1:
            return node.value.uses[0].accept(self)
        return None

    def visit_Combination(self, node: Combination):
        if node.kind is CallKind.KNOWN and node.fun_info in (
            fndb.BUILD_LIST,
            fndb.PROTOCOLS["build_set"],
            fndb.PROTOCOLS["build_dict"],
        ):
            return node
        return None


_CONTAINER_DISPLAY = _ContainerDisplay()


class _AssertDeclaredFieldStores(NodeVisitor):
    """Wrap resolved stores to declared instance fields and element writes to
    declared container fields in policy-controlled Casts, following SBCL's
    slot-access-transform (src/code/defstruct.lisp)."""

    def __init__(self, function: Lambda, facts, class_facts):
        self.function = function
        self.facts = facts
        self.class_facts = class_facts
        self.changed = False

    def lvar_type(self, lvar) -> ValueType:
        result = BOTTOM
        for producer in lvar.uses:
            result = result.join(self.facts.types.get(producer, UNKNOWN))
        return result

    def _wrap_operand(
        self, node: Combination, lvar, cls_name: str, field_name: str, contract
    ) -> None:
        if contract is None or lvar is None:
            return
        field_contract = DeclaredFieldContract(field_name, cls_name, contract)
        if len(lvar.uses) == 1 and lvar.uses[0].accept(
            _HasDeclaredFieldCast(node, field_contract)
        ):
            return
        actual = self.lvar_type(lvar)
        if (
            actual is not UNKNOWN
            and actual is not BOTTOM
            and not actual.may_be_unbound
            and actual.fits(field_contract.checked_type)
            and not contract.has_element_contracts()
        ):
            return
        location = (
            node.info.location
            if node.info is not None and hasattr(node.info, "location")
            else (
                self.function.converter.location(node.source)
                if node.source is not None and hasattr(self.function, "converter")
                else None
            )
        )
        insert_cast_before(
            node,
            lvar,
            lambda value, node=node, fc=field_contract, loc=location: Cast(
                node.lexenv,
                node.source,
                value,
                fc,
                loc,
                int(fc.requires_check(node.lexenv.policy.checks_contracts)),
                declaration=("field-store", node),
            ),
        )
        node.block.component.reoptimize = True
        self.changed = True

    def _wrap_container_display(
        self, display: Combination, cls_name: str, field_name: str, contract
    ) -> None:
        key_contract, val_contract = contract.accept(_CONTAINER_WRITE_CONTRACTS)
        if display.fun_info in (fndb.BUILD_LIST, fndb.PROTOCOLS["build_set"]):
            if val_contract is not None:
                for arg in list(display.args):
                    self._wrap_operand(display, arg, cls_name, field_name, val_contract)
        elif display.fun_info is fndb.PROTOCOLS["build_dict"]:
            for idx, arg in enumerate(list(display.args)):
                elem_c = key_contract if idx % 2 == 0 else val_contract
                if elem_c is not None:
                    self._wrap_operand(display, arg, cls_name, field_name, elem_c)

    def _lookup_container_field(self, seq_lvar):
        if len(seq_lvar.uses) != 1:
            return None
        origin = seq_lvar.uses[0].accept(_GETATTR_FIELD_ORIGIN)
        if origin is None:
            return None
        inst_lvar, field_name = origin
        recv_type = self.lvar_type(inst_lvar)
        return recv_type.accept(_DeclaredFieldLookup(self.class_facts, field_name))

    def visit_Combination(self, node: Combination):
        if (
            self.function.is_protocol
            or node.kind is not CallKind.KNOWN
            or node.fun_info is None
        ):
            return
        info = node.fun_info
        if (
            info in (fndb.SETATTR, fndb.PHYSICAL_FIELD_SET)
            and len(node.args) == 2
            and node.info is not None
        ):
            recv_lvar, val_lvar = node.args
            field_name = node.info.name
            found = self.lvar_type(recv_lvar).accept(
                _DeclaredFieldLookup(self.class_facts, field_name)
            )
            if found is not None:
                cls_name, contract = found
                if len(val_lvar.uses) == 1:
                    display = val_lvar.uses[0].accept(_CONTAINER_DISPLAY)
                    if display is not None and contract.has_element_contracts():
                        self._wrap_container_display(
                            display, cls_name, field_name, contract
                        )
                self._wrap_operand(node, val_lvar, cls_name, field_name, contract)
            return
        if info.name == "_set_instance_field" and len(node.args) == 3:
            from .ir1opt import bound_constant_text

            field_name = bound_constant_text(node.args[1])
            if field_name is not None:
                recv_lvar, _, val_lvar = node.args
                found = self.lvar_type(recv_lvar).accept(
                    _DeclaredFieldLookup(self.class_facts, field_name)
                )
                if found is not None:
                    cls_name, contract = found
                    if len(val_lvar.uses) == 1:
                        display = val_lvar.uses[0].accept(_CONTAINER_DISPLAY)
                        if display is not None and contract.has_element_contracts():
                            self._wrap_container_display(
                                display, cls_name, field_name, contract
                            )
                    self._wrap_operand(node, val_lvar, cls_name, field_name, contract)
            return
        if info is fndb.PROTOCOLS["setitem"] and len(node.args) == 3:
            found = self._lookup_container_field(node.args[0])
            if found is not None:
                cls_name, contract = found
                key_contract, val_contract = contract.accept(_CONTAINER_WRITE_CONTRACTS)
                if key_contract is not None:
                    self._wrap_operand(
                        node, node.args[1], cls_name, contract.name, key_contract
                    )
                if val_contract is not None:
                    self._wrap_operand(
                        node, node.args[2], cls_name, contract.name, val_contract
                    )
            return
        if info.name in ("_storage_append", "%set_add") and len(node.args) == 2:
            found = self._lookup_container_field(node.args[0])
            if found is not None:
                cls_name, contract = found
                _, val_contract = contract.accept(_CONTAINER_WRITE_CONTRACTS)
                if val_contract is not None:
                    self._wrap_operand(
                        node, node.args[1], cls_name, contract.name, val_contract
                    )
            return
        if info.name == "_storage_write" and len(node.args) == 3:
            found = self._lookup_container_field(node.args[0])
            if found is not None:
                cls_name, contract = found
                _, val_contract = contract.accept(_CONTAINER_WRITE_CONTRACTS)
                if val_contract is not None:
                    self._wrap_operand(
                        node, node.args[2], cls_name, contract.name, val_contract
                    )
            return
        if info is fndb.CALL_RESOLVED and node.args and len(node.args[0].uses) == 1:
            resolved = node.args[0].uses[0].accept(_RESOLVED_METHOD_CALL)
            if resolved is not None:
                seq_lvar, method_name = resolved
                if (
                    method_name in ("append", "add")
                    and len(node.args) == 2
                    and not node.info.keywords
                ):
                    found = self._lookup_container_field(seq_lvar)
                    if found is not None:
                        cls_name, contract = found
                        _, val_contract = contract.accept(_CONTAINER_WRITE_CONTRACTS)
                        if val_contract is not None:
                            self._wrap_operand(
                                node,
                                node.args[1],
                                cls_name,
                                contract.name,
                                val_contract,
                            )
                elif (
                    method_name == "insert"
                    and len(node.args) == 3
                    and not node.info.keywords
                ):
                    found = self._lookup_container_field(seq_lvar)
                    if found is not None:
                        cls_name, contract = found
                        _, val_contract = contract.accept(_CONTAINER_WRITE_CONTRACTS)
                        if val_contract is not None:
                            self._wrap_operand(
                                node,
                                node.args[2],
                                cls_name,
                                contract.name,
                                val_contract,
                            )


def assert_declared_field_stores(compilation) -> bool:
    """Enforce declared class field types on resolved field and element stores."""
    if compilation.locall_state is None or compilation.facts is None:
        return False
    class_facts = compilation.locall_state.get("class_facts")
    if not class_facts or not any(cf.declared_fields for cf in class_facts.values()):
        return False
    changed = False
    for function in compilation.lambdas:
        if function.is_protocol or function not in compilation.facts:
            continue
        visitor = _AssertDeclaredFieldStores(
            function, compilation.facts[function], class_facts
        )
        for block in list(function.component.blocks()):
            for node in list(block.nodes()):
                node.accept(visitor)
        changed |= visitor.changed
    return changed
