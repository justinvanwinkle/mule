"""Module class hierarchy, slot table, and instance field type analysis for local call analysis."""

from __future__ import annotations

from collections import defaultdict
from dataclasses import dataclass, field

from mule import ast

from . import ir1opt
from .constraint import (
    BOOTSTRAP_CLASS_TARGETS,
    NO_FACT,
    Body,
)
from .ctype import (
    ANY_CLASS_TYPE,
    BOOLEAN,
    BOTTOM,
    BUILTIN_CLASS_TYPES,
    DICT_TYPE,
    EMPTY_LIST_TYPE,
    FIXNUM_MAX,
    FIXNUM_MIN,
    LIST_TYPE,
    NONE_TYPE,
    PYTHON_INTEGER,
    STORAGE_CLASS_TYPES,
    TEXT_TYPE,
    UNKNOWN,
    ClassType,
    DictType,
    InstanceType,
    IntegerType,
    ListType,
    ValueType,
    ValueTypeVisitor,
)
from .fndb import (
    BUILD_LIST,
    BUILD_TUPLE,
    CLASS_DELETE,
    CLASS_STORE,
    GETATTR,
    MAKE_CLASS,
    PHYSICAL_FIELD_REF,
    PHYSICAL_FIELD_SET,
    SETATTR,
)
from .ir1util import (
    made_function,
)
from .node import (
    CallKind,
    ClassConstructorTarget,
    Combination,
    GlobalVar,
    Lambda,
    LambdaVar,
    LeafVisitor,
    NodeVisitor,
    Ref,
    linked,
)


class _GlobalCalleeRefName(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_GlobalVar(self, leaf: GlobalVar):
        return leaf.name


_GLOBAL_CALLEE_REF_NAME = _GlobalCalleeRefName()


class _ClassBaseNames(NodeVisitor, LeafVisitor):
    """Extract base class global names from the ``bases`` operand of ``%make_class``."""

    def visit_Node(self, node) -> tuple[str, ...]:
        return ()

    def visit_Leaf(self, leaf) -> tuple[str, ...]:
        return ()

    def visit_Ref(self, node: Ref) -> tuple[str, ...]:
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar) -> tuple[str, ...]:
        if (
            len(leaf.sets) == 1
            and leaf.sets[0].value is not None
            and len(leaf.sets[0].value.uses) == 1
        ):
            return leaf.sets[0].value.uses[0].accept(self)
        return ()

    def visit_Combination(self, node: Combination) -> tuple[str, ...]:
        if node.kind is CallKind.KNOWN and node.fun_info is BUILD_TUPLE:
            return tuple(
                base_name
                for arg in node.args
                if len(arg.uses) == 1
                and (base_name := arg.uses[0].accept(_GLOBAL_CALLEE_REF_NAME))
                is not None
            )
        return ()


_CLASS_BASE_NAMES = _ClassBaseNames()


class _CompleteReferenceSlots(ast.Visitor):
    """Whether a declaration's physical names match its compiler field prefix."""

    def visit_Node(self, node):
        return False

    def visit_TextLiteral(self, node):
        # Runtime class creation adds a mangled slot after compiler fields.
        # Until that extra field is modeled, descendants cannot use our prefix.
        return not (node.value.startswith("__") and not node.value.endswith("__"))

    def visit_Tuple(self, node):
        return all(element.accept(self) for element in node.elements)

    def visit_List(self, node):
        return all(element.accept(self) for element in node.elements)


_COMPLETE_REFERENCE_SLOTS = _CompleteReferenceSlots()


class _ModuleClassCollector(NodeVisitor):
    """Collect module-level class definitions and their method lambdas."""

    def __init__(self):
        self.methods: dict[str, dict[str, Lambda]] = defaultdict(dict)
        self.public_names: dict[str, str] = {}
        self.bases: dict[str, tuple[str, ...]] = {}
        # Identities whose definitions pass class keywords (a metaclass
        # among them), so calling the class may construct anything.
        self.keyword_classes: set[str] = set()
        self.definitions: dict[str, int] = defaultdict(int)
        # Names a class body binds to anything other than a plain function.
        self.other_bindings: dict[str, set[str]] = defaultdict(set)
        self.fields: dict[str, tuple[str, ...]] = {}
        self.declared_fields: dict[str, dict[str, object]] = defaultdict(dict)
        self.native_classes: set[str] = set()
        self.unproven_slot_classes: set[str] = set()

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN or node.fun_info is None:
            return
        if node.fun_info is CLASS_STORE and len(node.args) == 2:
            # (class namespace, value): a function stored in a class body.
            info = linked(node.info, node, "info")
            value = node.args[1]
            if info.name in ("__slots__", "__mule_slots__"):
                declaration = ir1opt.bound_constant_literal(value)
                if declaration is None or not declaration.accept(
                    _COMPLETE_REFERENCE_SLOTS
                ):
                    self.unproven_slot_classes.add(info.identity)
            lam = made_function(value.uses[0]) if len(value.uses) == 1 else None
            if lam is not None and info.name not in (
                "__init_subclass__",
                "__class_getitem__",
            ):
                self.methods[info.identity][info.name] = lam
            else:
                # Any other binding (a decorated function, a descriptor, a
                # rebinding, or an implicit classmethod) shadows a method of
                # the same name.
                self.methods[info.identity].pop(info.name, None)
                self.other_bindings[info.identity].add(info.name)
        elif node.fun_info is CLASS_DELETE:
            info = linked(node.info, node, "info")
            self.methods[info.identity].pop(info.name, None)
        elif node.fun_info is MAKE_CLASS:
            info = linked(node.info, node, "class definition")
            if info.class_id is not None:
                self.native_classes.add(info.identity)
            self.fields[info.identity] = info.fields
            self.declared_fields[info.identity] = dict(info.field_annotations)
            self.public_names[info.identity] = info.public_name
            self.definitions[info.public_name] += 1
            if info.has_keywords:
                self.keyword_classes.add(info.identity)
            self.bases[info.public_name] = (
                node.args[0].uses[0].accept(_CLASS_BASE_NAMES)
                if node.args and len(node.args[0].uses) == 1
                else ()
            )
            if len(self.bases[info.public_name]) != info.base_count:
                # A base the analysis cannot name may define anything.
                self.keyword_classes.add(info.identity)


def _collect_module_classes(
    lambdas,
) -> dict[str, dict[str, ClassConstructorTarget]]:
    return _collect_class_tables(lambdas)[0]


@dataclass(slots=True)
class ClassFacts:
    """What analysis knows about one user class whose instances it types:
    its resolved methods (including inherited ones defined in the module),
    its single-inheritance chain of class identities, the names shadowed on
    the class by non-method bindings, and its guarded instance field types."""

    identity: str
    methods: dict[str, Lambda]
    fields: dict[str, ValueType]
    chain: tuple[str, ...] = ()
    shadowed: frozenset[str] = frozenset()
    public_name: str = ""
    is_protocol: bool = False
    fixed_slots: tuple[str, ...] = ()
    constructor_proven: bool = True
    widened_fields: set[str] = field(default_factory=set)
    declared_fields: dict[str, object] = field(default_factory=dict)
    unsafe_declared_fields: frozenset[str] = frozenset()


class _ProtocolSelfPrediction(ValueTypeVisitor):
    """Resolve instance hints through the method's actual declaring owner.

    General entries still treat this as a prediction, not a runtime layout
    proof. Semantic hints without an instance identity retain their meaning.
    """

    def __init__(self, owner: ClassFacts | None):
        self.owner = owner

    def visit_ValueType(self, fact):
        return fact

    def visit_InstanceType(self, fact):
        if self.owner is None:
            return UNKNOWN
        return InstanceType(
            self.owner.identity,
            may_be_unbound=fact.may_be_unbound,
            fixed_slots=self.owner.fixed_slots,
        )


class _SanitizeInstanceFieldType(ValueTypeVisitor):
    """Restricts a value type stored into an instance field to shapes whose
    consumers guard their runtime operations (instance/None unions, fixnums/ints,
    strings, and dicts)."""

    def visit_ValueType(self, fact):
        return UNKNOWN

    def visit_BottomType(self, fact):
        return BOTTOM

    def visit_NoneType(self, fact):
        return UNKNOWN if fact.may_be_unbound else NONE_TYPE

    def visit_IntegerType(self, fact):
        if fact.may_be_unbound:
            return UNKNOWN
        if fact.is_fixnum:
            return IntegerType(FIXNUM_MIN, FIXNUM_MAX)
        return PYTHON_INTEGER

    def visit_BooleanType(self, fact):
        return UNKNOWN if fact.may_be_unbound else BOOLEAN

    def visit_PythonIntegerType(self, fact):
        return UNKNOWN if fact.may_be_unbound else PYTHON_INTEGER

    def visit_TextType(self, fact):
        return UNKNOWN if fact.may_be_unbound else TEXT_TYPE

    def visit_InstanceType(self, fact):
        if fact.may_be_unbound:
            return UNKNOWN
        return fact.as_optional()

    def visit_UnionType(self, fact):
        if fact.may_be_unbound:
            return UNKNOWN
        return fact.map(lambda member: member.accept(self))

    def visit_DictType(self, fact):
        if fact.may_be_unbound:
            return UNKNOWN
        if fact.value is BOTTOM:
            return DictType(BOTTOM, BOTTOM, False, fact.proven)
        key = fact.key.accept(self) if fact.key is not BOTTOM else BOTTOM
        val = fact.value.accept(self)
        if val is BOTTOM or val is NONE_TYPE:
            return DICT_TYPE if fact.proven else UNKNOWN
        if val is UNKNOWN and not fact.proven:
            return UNKNOWN
        return DictType(key, val, False, fact.proven)


class _ReadLambdaVar(NodeVisitor, LeafVisitor):
    """Single-assignment local variable origin of a reference."""

    def __init__(self, seen=frozenset()):
        self.seen = seen

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        if leaf.indirect:
            return None
        definitions = [store.value for store in leaf.sets if store.value is not None]
        if len(definitions) > 1:
            return None
        if (
            leaf not in self.seen
            and len(definitions) == 1
            and len(definitions[0].uses) == 1
        ):
            origin = definitions[0].uses[0].accept(_ReadLambdaVar(self.seen | {leaf}))
            if origin is not None:
                return origin
        return leaf


_READ_LAMBDA_VAR = _ReadLambdaVar()


class _GetattrOnInstance(NodeVisitor, LeafVisitor):
    def __init__(self, seen=frozenset()):
        self.seen = seen

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        definitions = [store.value for store in leaf.sets if store.value is not None]
        if leaf.indirect or leaf in self.seen or not definitions:
            return None
        sub = _GetattrOnInstance(self.seen | {leaf})
        if len(definitions) == 1 and len(definitions[0].uses) == 1:
            return definitions[0].uses[0].accept(sub)
        first_attr = None
        first_recv_var = None
        for defn in definitions:
            if not defn.uses:
                return None
            for use in defn.uses:
                attr = use.accept(sub)
                if attr is None:
                    return None
                inst_lvar, field_name = attr
                recv_var = inst_lvar.use().accept(_READ_LAMBDA_VAR)
                if recv_var is None:
                    return None
                if first_attr is None:
                    first_attr = attr
                    first_recv_var = recv_var
                elif field_name != first_attr[1] or recv_var is not first_recv_var:
                    return None
        return first_attr

    def visit_Combination(self, node: Combination):
        if node.kind is CallKind.KNOWN and node.fun_info is PHYSICAL_FIELD_REF:
            return node.args[0], linked(node.info, node, "physical field").name
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is GETATTR
            and len(node.args) == 1
            and node.info is not None
        ):
            return node.args[0], node.info.name
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.name == "_instance_field"
            and len(node.args) == 2
        ):
            name = ir1opt.bound_constant_text(node.args[1])
            if name is None:
                name = ir1opt.lvar_type(node.args[1]).interned_text()
            if name is not None:
                return node.args[0], name
        return None


_GETATTR_ON_INSTANCE = _GetattrOnInstance()


_SANITIZE_INSTANCE_FIELD_TYPE = _SanitizeInstanceFieldType()


class _SanitizeProtocolFieldType(_SanitizeInstanceFieldType):
    def visit_IntegerType(self, fact):
        if fact.may_be_unbound:
            return UNKNOWN
        if fact.is_fixnum:
            return fact
        return PYTHON_INTEGER

    def visit_ListType(self, fact):
        if fact.may_be_unbound or fact.element is BOTTOM:
            return UNKNOWN
        elem = fact.element.accept(self)
        return ListType(elem)

    def visit_WordVectorType(self, fact):
        # Word-vector templates check the receiver's tag before each access.
        return UNKNOWN if fact.may_be_unbound else fact


_SANITIZE_PROTOCOL_FIELD_TYPE = _SanitizeProtocolFieldType()


class _StorageReadFromInstanceField(NodeVisitor, LeafVisitor):
    def __init__(self, seen=frozenset()):
        self.seen = seen

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        definitions = [store.value for store in leaf.sets if store.value is not None]
        if leaf.indirect or leaf in self.seen or not definitions:
            return None
        sub = _StorageReadFromInstanceField(self.seen | {leaf})
        field_name = None
        for defn in definitions:
            if not defn.uses:
                return None
            for use in defn.uses:
                name = use.accept(sub)
                if name is None or (field_name is not None and name != field_name):
                    return None
                field_name = name
        return field_name

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.name == "_storage_read"
            and len(node.args) == 2
        ):
            attr = node.args[0].use().accept(_GETATTR_ON_INSTANCE)
            if attr is not None:
                return attr[1]
        return None


_STORAGE_READ_FROM_INSTANCE_FIELD = _StorageReadFromInstanceField()


class _EmptyListCopiedFromField(NodeVisitor, LeafVisitor):
    def __init__(self, field_name: str):
        self.field_name = field_name
        self.seen: set[LambdaVar] = set()

    def visit_Node(self, node):
        return False

    def visit_Leaf(self, leaf):
        return False

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        definitions = [store for store in leaf.sets if store.value is not None]
        if (
            leaf.indirect
            or leaf in self.seen
            or len(definitions) != 1
            or len(definitions[0].value.uses) != 1
        ):
            return False
        init_node = definitions[0].value.uses[0]
        if not init_node.accept(_IS_EMPTY_BUILD_LIST):
            # A single-read forwarding local does not add another mutation
            # path. Deletion ends its lifetime but does not define a new value.
            # A deletion also emits a resultless boundness-check reference.
            # It cannot expose the list; keep that check and count value uses.
            value_refs = [ref for ref in leaf.refs if ref.lvar is not None]
            if len(value_refs) != 1:
                return False
            self.seen.add(leaf)
            try:
                return init_node.accept(self)
            finally:
                self.seen.remove(leaf)
        saw_append = False
        for ref in leaf.refs:
            if ref.lvar is None or ref.lvar.dest is None:
                continue
            dest = ref.lvar.dest
            copied = dest.accept(_AppendsFromField(ref.lvar, self.field_name))
            if copied is False:
                return False
            if copied is True:
                saw_append = True
        return saw_append


class _IsEmptyBuildList(NodeVisitor):
    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        return (
            node.kind is CallKind.KNOWN
            and node.fun_info is BUILD_LIST
            and len(node.args) == 0
        )


_IS_EMPTY_BUILD_LIST = _IsEmptyBuildList()


class _AppendsFromField(NodeVisitor):
    def __init__(self, list_lvar, field_name: str):
        self.list_lvar = list_lvar
        self.field_name = field_name

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.name == "_storage_append"
            and len(node.args) == 2
            and node.args[0] is self.list_lvar
        ):
            src_field = node.args[1].use().accept(_STORAGE_READ_FROM_INSTANCE_FIELD)
            return src_field == self.field_name
        return None


class _DependsOnVars(NodeVisitor, LeafVisitor):
    """Whether a value may derive from one of ``targets``. A variable whose
    definitions cannot all be followed may derive from any of them."""

    def __init__(self, targets: set[LambdaVar]):
        self.targets = targets
        self.seen: set[LambdaVar] = set()

    def visit_Node(self, node):
        return False

    def visit_Leaf(self, leaf):
        return False

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        if leaf in self.targets:
            return True
        if leaf in self.seen:
            return False
        if leaf.indirect:
            return True
        self.seen.add(leaf)
        return any(
            store.value is None or any(use.accept(self) for use in store.value.uses)
            for store in leaf.sets
        )

    def visit_Combination(self, node: Combination):
        return any(use.accept(self) for arg in node.args for use in arg.uses)


class _DependsOnInstanceField(NodeVisitor, LeafVisitor):
    def __init__(self, field_name: str):
        self.field_name = field_name
        self.seen: set[LambdaVar] = set()

    def visit_Node(self, node):
        return False

    def visit_Leaf(self, leaf):
        return False

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        if leaf.indirect or leaf in self.seen:
            return False
        self.seen.add(leaf)
        definitions = [store.value for store in leaf.sets if store.value is not None]
        return any(use.accept(self) for defn in definitions for use in defn.uses)

    def visit_Combination(self, node: Combination):
        field = node.accept(_GETATTR_ON_INSTANCE)
        if field is not None:
            return field[1] == self.field_name
        return any(use.accept(self) for arg in node.args for use in arg.uses)


class _CollectSetattrSites(NodeVisitor):
    def __init__(self, sites: list[Combination]):
        self.sites = sites

    def visit_Combination(self, node: Combination):
        if node.kind is CallKind.KNOWN and (
            (node.fun_info is SETATTR and len(node.args) == 2 and node.info is not None)
            or (node.fun_info is PHYSICAL_FIELD_SET and len(node.args) == 2)
            or (
                node.fun_info is not None
                and node.fun_info.name == "_set_instance_field"
                and len(node.args) == 3
            )
        ):
            self.sites.append(node)


class _InstanceClasses(ValueTypeVisitor):
    """The complete class alternatives of an instance receiver fact."""

    def visit_ValueType(self, fact):
        return ()

    def visit_InstanceType(self, fact):
        return (fact.identity,) if not fact.may_be_unbound else ()

    def visit_UnionType(self, fact):
        if fact.may_be_unbound:
            return ()
        alternatives = [member.accept(self) for member in fact.members]
        if not all(alternatives):
            return ()
        return tuple(dict.fromkeys(cid for group in alternatives for cid in group))


_INSTANCE_CLASSES = _InstanceClasses()


def _entry_narrowed_parameters(
    lam: Lambda,
    ctx_name: str,
    parameters: dict[tuple[Lambda, str], tuple[ValueType, ...]],
) -> set[LambdaVar]:
    """The non-receiver parameters whose type in entry ``ctx_name`` is
    narrower than in the general entry that every other call reaches."""
    if ctx_name == "general":
        return set()
    return {
        var
        for var, entry_p, general_p in zip(
            lam.vars[1:],
            parameters.get((lam, ctx_name), ())[1:],
            parameters.get((lam, "general"), ())[1:],
            strict=False,
        )
        if entry_p != general_p
    }


def _update_class_field_types(
    class_facts: dict[str, ClassFacts],
    setattr_sites: list[tuple[Lambda, Combination]],
    bodies: dict[tuple[Lambda, str], Body],
    parameters: dict[tuple[Lambda, str], tuple[ValueType, ...]],
) -> bool:
    if not class_facts or not setattr_sites:
        return False
    direct_stores: dict[str, dict[str, ValueType]] = defaultdict(dict)
    unattributed_stores: dict[str, ValueType] = {}
    ambiguous_fields: set[str] = set()
    unknown_field_write = False
    for lam, node in setattr_sites:
        physical = (
            node.fun_info is not None and node.fun_info.name == "_set_instance_field"
        )
        field_name = (
            ir1opt.bound_constant_text(node.args[1]) if physical else node.info.name
        )
        if field_name is None:
            unknown_field_write = True
            continue
        recv_use = node.args[0].use()
        val_use = node.args[2 if physical else 1].use()
        found_context = False
        saw_typed_receiver = False
        best_stores: dict[str, ValueType] = {}
        unattributed_type = BOTTOM
        for ctx_name in ("observed", "fixnum", "general"):
            body = bodies.get((lam, ctx_name))
            if body is None:
                continue
            recv_fact = body.facts.get(recv_use, NO_FACT)
            val_body = body
            # The observed and fixnum entries are guarded specializations; any
            # call outside their parameter domains, including callers this
            # analysis never saw, runs the general entry. A field fact must
            # cover every store, so a value derived from a parameter those
            # entries narrow takes its general-entry fact, as SBCL's
            # propagate-local-call-args uses call-site argument types only
            # for a lambda without an external entry.
            narrowed = _entry_narrowed_parameters(lam, ctx_name, parameters)
            if (ctx_name == "observed" and lam.signature.default_parameters) or (
                narrowed and val_use.accept(_DependsOnVars(narrowed))
            ):
                val_body = bodies.get((lam, "general")) or body
            val_fact = val_body.facts.get(val_use, NO_FACT)
            if recv_fact.type is BOTTOM or val_fact.type is BOTTOM:
                continue
            found_context = True
            unattributed_type = unattributed_type.join(val_fact.type)
            for recv_cls in recv_fact.type.accept(_INSTANCE_CLASSES):
                if recv_cls in class_facts:
                    saw_typed_receiver = True
                    if recv_cls not in best_stores or best_stores[recv_cls] is UNKNOWN:
                        best_stores[recv_cls] = val_fact.type
        for recv_cls, stored_type in best_stores.items():
            cur_type = class_facts[recv_cls].fields.get(field_name, BOTTOM)
            getattr_info = val_use.accept(_GETATTR_ON_INSTANCE)
            same_field_getattr = (
                getattr_info is not None and getattr_info[1] == field_name
            )
            if (
                stored_type is UNKNOWN
                and cur_type is not BOTTOM
                and same_field_getattr
                and not cur_type.fits(PYTHON_INTEGER)
            ):
                stored_type = cur_type
            elif (
                stored_type == LIST_TYPE
                and cur_type is BOTTOM
                and val_use.accept(_EmptyListCopiedFromField(field_name))
            ):
                stored_type = EMPTY_LIST_TYPE
            if stored_type is not UNKNOWN or (
                not same_field_getattr
                and not (
                    cur_type is BOTTOM
                    and val_use.accept(_DependsOnInstanceField(field_name))
                )
            ):
                prev = direct_stores[recv_cls].get(field_name, BOTTOM)
                direct_stores[recv_cls][field_name] = prev.join(stored_type)
        # Without receiver identity, this value can reach any existing candidate
        # for the attribute. It widens those candidates without creating owners.
        if found_context and not saw_typed_receiver:
            previous = unattributed_stores.get(field_name, BOTTOM)
            unattributed_stores[field_name] = previous.join(unattributed_type)

    if unknown_field_write:
        ambiguous_fields.update(
            name for fields in direct_stores.values() for name in fields
        )
    changed = False
    for class_id, facts in class_facts.items():
        new_fields: dict[str, ValueType] = {
            fname: contract.fact for fname, contract in facts.declared_fields.items()
        }
        chain = facts.chain or (class_id,)
        sanitizer = (
            _SANITIZE_PROTOCOL_FIELD_TYPE
            if facts.is_protocol
            else _SANITIZE_INSTANCE_FIELD_TYPE
        )
        candidate_names = {
            name
            for anc_id in chain
            for name in direct_stores.get(anc_id, ())
            if class_facts[anc_id].constructor_proven
            and name not in facts.declared_fields
            and name not in ambiguous_fields
            and name not in facts.widened_fields
            and name not in facts.methods
            and name not in facts.shadowed
        }
        for name in candidate_names:
            joined = BOTTOM
            for anc_id in chain:
                if not class_facts[anc_id].constructor_proven:
                    continue
                anc_type = direct_stores.get(anc_id, {}).get(name, BOTTOM)
                if anc_type is not BOTTOM:
                    joined = joined.join(anc_type)
            joined = joined.join(unattributed_stores.get(name, BOTTOM))
            sanitized = joined.accept(sanitizer)
            if (
                sanitized is not UNKNOWN
                and sanitized is not BOTTOM
                and sanitized is not NONE_TYPE
                and sanitized != DictType(BOTTOM, BOTTOM, False)
            ):
                new_fields[name] = sanitized
        for name in facts.fields:
            if name not in new_fields:
                facts.widened_fields.add(name)
        if new_fields != facts.fields:
            facts.fields = new_fields
            changed = True
    return changed


def _collect_class_tables(lambdas):
    """Constructor targets by namespace and name, and the facts of every
    class whose instances analysis types, by identity."""
    by_ns: dict[str, dict[str, ClassConstructorTarget]] = defaultdict(dict)
    typed: dict[str, ClassFacts] = {}
    collectors: dict[str, _ModuleClassCollector] = defaultdict(_ModuleClassCollector)
    visited_components = set()
    for lambda_ in lambdas:
        component = lambda_.component
        if component is None or (lambda_.namespace, component) in visited_components:
            continue
        visited_components.add((lambda_.namespace, component))
        collector = collectors[lambda_.namespace]
        for block in component.blocks():
            for node in block.nodes():
                node.accept(collector)
    is_protocol = any(lam.is_protocol for lam in lambdas)
    for ns, collector in collectors.items():
        layouts = {}
        for class_id, cls_name in collector.public_names.items():
            _, chain, _ = _resolved_class_info(collector, class_id, cls_name)
            # Unknown and multiple bases do not establish a physical prefix.
            known_chain = all(
                not collector.bases.get(collector.public_names[cid], ())
                or all(
                    base == "object" or base in collector.public_names.values()
                    for base in collector.bases[collector.public_names[cid]]
                )
                for cid in chain
            ) and all(
                len(collector.bases.get(collector.public_names[cid], ())) <= 1
                for cid in chain
            )
            layouts[class_id] = (
                tuple(
                    dict.fromkeys(
                        name
                        for cid in reversed(chain)
                        for name in collector.fields.get(cid, ())
                    )
                )
                if known_chain
                and not any(cid in collector.native_classes for cid in chain)
                and not any(cid in collector.unproven_slot_classes for cid in chain)
                and not any(cid in collector.keyword_classes for cid in chain)
                and not any(
                    collector.definitions[collector.public_names[cid]] != 1
                    for cid in chain
                )
                else ()
            )
        all_class_ids = list(
            dict.fromkeys([*collector.public_names, *collector.methods])
        )
        for class_id in all_class_ids:
            cls_name = collector.public_names.get(class_id, class_id)
            methods = collector.methods.get(class_id, {})
            base_target = BOOTSTRAP_CLASS_TARGETS.get(cls_name) if is_protocol else None
            if base_target is not None:
                base_type = base_target.base_type
                cls_type = base_target.cls_type
                records_type_origin = base_target.records_type_origin
            else:
                constructed_type = (
                    InstanceType(class_id, fixed_slots=layouts.get(class_id, ()))
                    if _constructs_own_instances(collector, class_id, cls_name)
                    else UNKNOWN
                )
                base_type = (
                    STORAGE_CLASS_TYPES.get(cls_name, constructed_type)
                    if is_protocol
                    else constructed_type
                )
                constructed_class = (
                    ClassType((base_type,))
                    if base_type is not UNKNOWN
                    else ANY_CLASS_TYPE
                )
                cls_type = (
                    BUILTIN_CLASS_TYPES.get(cls_name, constructed_class)
                    if is_protocol
                    else constructed_class
                )
                records_type_origin = False
            by_ns[ns][cls_name] = ClassConstructorTarget(
                base_type=base_type,
                cls_type=cls_type,
                new_target=methods.get("__new__"),
                init_target=methods.get("__init__"),
                records_type_origin=records_type_origin,
                layout_type=InstanceType(
                    class_id, fixed_slots=layouts.get(class_id, ())
                ),
            )
            # A custom constructor can return another class without changing
            # this class's layout or the facts guarded method entries can use.
            if base_target is None or layouts.get(class_id):
                methods_map, chain, shadowed = _resolved_class_info(
                    collector, class_id, cls_name
                )
                declared_fields: dict[str, object] = {}
                unsafe_declared: set[str] = set()
                seen_undeclared_fields: set[str] = set()
                for anc_id in reversed(chain):
                    anc_declared = collector.declared_fields.get(anc_id, {})
                    for fname, contract in anc_declared.items():
                        if fname in methods_map:
                            continue
                        if fname in declared_fields:
                            prev_contract = declared_fields[fname]
                            if not prev_contract.fact.fits(contract.fact):
                                unsafe_declared.add(fname)
                        elif fname in seen_undeclared_fields:
                            unsafe_declared.add(fname)
                        declared_fields[fname] = contract
                    for fname in collector.fields.get(anc_id, ()):
                        if fname not in declared_fields:
                            seen_undeclared_fields.add(fname)
                initial_fields = {
                    fname: contract.fact for fname, contract in declared_fields.items()
                }
                typed[class_id] = ClassFacts(
                    class_id,
                    methods_map,
                    initial_fields,
                    chain,
                    shadowed,
                    public_name=cls_name,
                    is_protocol=is_protocol,
                    fixed_slots=layouts.get(class_id, ()),
                    constructor_proven=_constructs_own_instances(
                        collector, class_id, cls_name
                    ),
                    declared_fields=declared_fields,
                    unsafe_declared_fields=frozenset(unsafe_declared),
                )
    return by_ns, typed


def _resolved_class_info(
    collector, class_id: str, cls_name: str
) -> tuple[dict[str, Lambda], tuple[str, ...], frozenset[str]]:
    """The class's methods in method-resolution order over its module
    bases, its single-inheritance chain of class identities, and the
    non-method attribute names bound on the chain."""
    identities = {name: identity for identity, name in collector.public_names.items()}
    chain = [class_id]
    current = cls_name
    while True:
        bases = [base for base in collector.bases.get(current, ()) if base != "object"]
        if len(bases) != 1 or bases[0] not in identities:
            if bases:
                chain = [class_id]
            break
        current = bases[0]
        chain.append(identities[current])
    resolved: dict[str, Lambda] = {}
    shadowed: set[str] = set()
    for identity in reversed(chain):
        for name in collector.other_bindings.get(identity, ()):
            resolved.pop(name, None)
            shadowed.add(name)
        for name, lam in collector.methods.get(identity, {}).items():
            shadowed.discard(name)
            resolved[name] = lam
    return resolved, tuple(chain), frozenset(shadowed)


def _constructs_own_instances(collector, class_id: str, cls_name: str) -> bool:
    """Whether calling the class returns an exact instance of it, as
    ``type.__call__`` does through ``object.__new__``: one definition under
    the name, no class keywords or metaclass, and no ``__new__`` on the class
    or on a base defined in the module. Bases imported from elsewhere are
    not inspected, so a class with such a base is excluded."""
    if collector.definitions.get(cls_name, 0) != 1:
        return False
    if class_id in collector.keyword_classes:
        return False
    if "__new__" in collector.methods.get(class_id, {}):
        return False
    identities = {name: identity for identity, name in collector.public_names.items()}
    pending = list(collector.bases.get(cls_name, ()))
    seen = set()
    while pending:
        base = pending.pop()
        if base == "object" or base in seen:
            continue
        seen.add(base)
        base_id = identities.get(base)
        if (
            base_id is None
            or base_id in collector.keyword_classes
            or "__new__" in collector.methods.get(base_id, {})
        ):
            return False
        pending.extend(collector.bases.get(base, ()))
    return True
