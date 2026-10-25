"""IR1 structures, following SBCL's ``src/compiler/node.lisp``.

IR1 is a flow graph rather than an annotated syntax tree. Control transfer
inside a block is represented by ``Ctran`` objects linking nodes; value flow
is represented by ``Lvar`` objects connecting producing nodes to one consumer.
The two are decoupled so that a discarded result is simply an lvar with no
destination, and evaluation order never depends on who receives a value.

Python adaptations, recorded in ``spec/python-changes.md``:

- An ``Lvar`` carries exactly one object. Lisp multiple values do not exist.
- A function's locals are all ``LambdaVar`` objects of one ``Lambda``, because
  Python's local namespace is flat and decided at compile time. Compiler
  temporaries are hidden lambda-vars of the same lambda rather than LET-bound
  lambdas; SBCL's LET conversion would produce the same home environment.
- ``Set`` with ``value=None`` represents Python's unbinding (``del`` and the
  implicit deletion of an ``except ... as`` name). Lisp variables are never
  unbound.
- ``Cleanup`` kinds describe Python's exception regions instead of Lisp's
  dynamic bindings. A block's error destination is derived from the innermost
  cleanup that designates a handler block; nothing is stored per block.
- ``Combination.info`` carries compile-time data that Lisp would pass as quoted
  constants (class definitions, import descriptors, range-loop state) so the
  known function's arguments remain ordinary runtime values.
"""

from __future__ import annotations

import itertools
from collections.abc import Callable, Mapping
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Literal

from mule import ast

from .annotation_types import Contract
from .ctype import (
    ANY_CLASS_TYPE,
    BOOLEAN,
    BOTTOM,
    BYTEARRAY_TYPE,
    BYTES_TYPE,
    CLASS_BOOL_TYPE,
    CLASS_BYTEARRAY_TYPE,
    CLASS_BYTES_TYPE,
    CLASS_DICT_TYPE,
    CLASS_FLOAT_TYPE,
    CLASS_INT_TYPE,
    CLASS_LIST_TYPE,
    CLASS_OBJECT_TYPE,
    CLASS_STR_TYPE,
    CLASS_TUPLE_TYPE,
    CLASS_TYPE_TYPE,
    DICT_TYPE,
    EMPTY_DICT_TYPE,
    FLOAT,
    INTEGER,
    LIST_TYPE,
    STORAGE_CLASS_TYPES,
    TEXT_TYPE,
    UNKNOWN,
    DictType,
    IntegerType,
    IteratorType,
    TupleType,
    ValueType,
)
from .policy import OptimizationPolicy

_numbers = itertools.count(1)


def _number() -> int:
    return next(_numbers)


def linked[T](value: T | None, owner: object, link: str) -> T:
    """``owner``'s ``link``. IR being analyzed or emitted is linked, so a
    missing link there means the IR is malformed, not an expected state."""
    if value is None:
        raise ValueError(f"{owner!r} has no {link}")
    return value


class CtranKind(Enum):
    UNUSED = auto()
    INSIDE_BLOCK = auto()
    BLOCK_START = auto()


class Ctran:
    """A control position leading into a node ("lead-in control transfer")."""

    __slots__ = ("block", "kind", "next", "number", "use")

    def __init__(self):
        self.kind = CtranKind.UNUSED
        self.next: Node | None = None
        self.use: Node | None = None
        self.block: Block | None = None
        self.number = _number()

    def __repr__(self):
        return f"<Ctran {self.number} {self.kind.name}>"


class Lvar:
    """Result flow: produced by ``uses``, consumed by exactly one ``dest``."""

    __slots__ = ("derived_type", "dest", "info", "number", "reoptimize", "uses")

    def __init__(self, dest: Node | None = None):
        self.dest = dest
        self.uses: list[ValuedNode] = []
        self.derived_type = None
        self.reoptimize = True
        self.info = None
        self.number = _number()

    def use(self) -> ValuedNode:
        """The unique producing node; callers must know there is exactly one."""
        if len(self.uses) != 1:
            raise ValueError(f"{self!r} does not have a single use")
        return self.uses[0]

    def accept(self, visitor):
        """IR1 conversion takes operands that are either syntax or already
        computed values; both accept its operand visitor (``visit_Lvar``)."""
        return visitor.visit_Lvar(self)

    def __repr__(self):
        return f"<Lvar {self.number}>"


class Node:
    """An operation with control linkage, lexical context, and source."""

    __slots__ = ("lexenv", "next", "number", "prev", "reoptimize", "source")

    def __init__(self, lexenv: Lexenv, source: ast.Node | None):
        self.prev: Ctran | None = None
        self.next: Ctran | None = None
        self.lexenv = lexenv
        self.source = source
        self.reoptimize = True
        self.number = _number()

    @property
    def block(self) -> Block:
        prev = linked(self.prev, self, "preceding control position")
        return linked(prev.block, prev, "block")

    @property
    def home_lambda(self) -> Lambda:
        return self.lexenv.lambda_

    def accept(self, visitor):
        return visitor.visit_Node(self)

    @property
    def value_lvar(self) -> Lvar | None:
        """The lvar receiving this node's value; a node that yields no value
        has none."""
        return None

    def detach_value(self) -> Lvar | None:
        """Stop delivering this node's value and return the lvar it went to."""
        return None

    def __repr__(self):
        return f"<{type(self).__name__} {self.number}>"


class ValuedNode(Node):
    __slots__ = ("derived_type", "lvar")

    def __init__(self, lexenv, source):
        super().__init__(lexenv, source)
        self.lvar: Lvar | None = None
        self.derived_type = None

    @property
    def dest(self) -> Node | None:
        return self.lvar.dest if self.lvar is not None else None

    @property
    def value_lvar(self) -> Lvar | None:
        return self.lvar

    def detach_value(self) -> Lvar | None:
        lvar = self.lvar
        if lvar is not None:
            lvar.uses.remove(self)
            self.lvar = None
        return lvar


class Ref(ValuedNode):
    """A reference to a leaf. ``final`` marks the last reference in a compiler
    temporary's extent; the binding is dead afterwards. User variables never
    carry it; lifetime analysis replaces it when IR2 exists."""

    __slots__ = ("allow_unbound", "final", "leaf")

    def __init__(self, lexenv, source, leaf: Leaf, *, final=False, allow_unbound=False):
        super().__init__(lexenv, source)
        self.leaf = leaf
        self.final = final
        self.allow_unbound = allow_unbound
        self.reoptimize = False

    def accept(self, visitor):
        return visitor.visit_Ref(self)


class If(Node):
    """Two-way branch on the truth of ``test``; always the last node in a block."""

    __slots__ = ("alternative", "consequent", "test")

    def __init__(
        self, lexenv, source, test: Lvar, consequent: Block, alternative: Block
    ):
        super().__init__(lexenv, source)
        self.test = test
        self.consequent = consequent
        self.alternative = alternative

    def accept(self, visitor):
        return visitor.visit_If(self)


class Set(ValuedNode):
    """Assign ``value`` to ``var``; ``value=None`` unbinds the variable."""

    __slots__ = ("missing_ok", "value", "var")

    def __init__(
        self,
        lexenv,
        source,
        var: BasicVar,
        value: Lvar | None,
        *,
        missing_ok: bool = False,
    ):
        super().__init__(lexenv, source)
        self.var = var
        self.value = value
        self.missing_ok = missing_ok

    def accept(self, visitor):
        return visitor.visit_Set(self)


class CallKind(Enum):
    FULL = auto()
    LOCAL = auto()
    KNOWN = auto()
    GUARDED = auto()
    ERROR = auto()


class Combination(ValuedNode):
    """A call. ``fun`` yields the callee; ``args`` the operands in order."""

    __slots__ = (
        "args",
        "fun",
        "fun_info",
        "info",
        "kind",
        "predicted",
        "proven",
        "template",
    )

    def __init__(self, lexenv, source, fun: Lvar):
        super().__init__(lexenv, source)
        self.fun = fun
        self.args: list[Lvar] = []
        self.kind = CallKind.FULL
        self.fun_info = None
        # Compile-time data for the known function (SBCL passes it as quoted
        # constants); the backend template chosen by LTN for a known call.
        self.info = None
        self.template = None
        # A GUARDED call's predicted lambda; ``proven`` when flow analysis
        # established the callee's identity rather than merely predicting it.
        self.predicted: Lambda | None = None
        self.proven = False

    def accept(self, visitor):
        return visitor.visit_Combination(self)


class Bind(Node):
    """Marks the start of a lambda body and its parameter binding."""

    __slots__ = ("lambda_",)

    def __init__(self, lexenv, source):
        super().__init__(lexenv, source)
        self.lambda_: Lambda | None = None

    def accept(self, visitor):
        return visitor.visit_Bind(self)


class Return(Node):
    """Marks the end of a lambda body; ``result`` yields the returned value."""

    __slots__ = ("lambda_", "result", "result_type")

    def __init__(self, lexenv, source, result: Lvar, lambda_: Lambda):
        super().__init__(lexenv, source)
        self.result = result
        self.lambda_ = lambda_
        self.result_type = None

    def accept(self, visitor):
        return visitor.visit_Return(self)


class Cast(ValuedNode):
    """Assert a contract on ``value``. Check generation decides whether the
    assertion becomes a runtime check (``type_check``) according to policy."""

    __slots__ = ("contract", "declaration", "location", "safety", "type_check", "value")

    def __init__(
        self,
        lexenv,
        source,
        value: Lvar,
        contract,
        location,
        safety: int,
        declaration=None,
    ):
        super().__init__(lexenv, source)
        # None once the assertion is removed from the IR.
        self.value: Lvar | None = value
        self.contract = contract
        self.location = location
        self.safety = safety
        self.type_check = True
        # The annotation this assertion enforces: ("return", lambda),
        # ("local", lambda, name) or ("global", namespace, name).
        self.declaration = declaration

    def accept(self, visitor):
        return visitor.visit_Cast(self)


class Entry(Node):
    """Start of the dynamic extent of a lexical exit (a cleanup's mess-up)."""

    __slots__ = ("cleanup", "exits")

    def __init__(self, lexenv, source):
        super().__init__(lexenv, source)
        self.exits: list[Exit] = []
        self.cleanup: Cleanup | None = None

    def accept(self, visitor):
        return visitor.visit_Entry(self)


class Exit(ValuedNode):
    """A lexical exit to ``entry``. With no entry it is degenerate: a filler
    for an otherwise empty block, exactly as in SBCL."""

    __slots__ = ("entry", "value")

    def __init__(
        self, lexenv, source, entry: Entry | None = None, value: Lvar | None = None
    ):
        super().__init__(lexenv, source)
        self.entry = entry
        self.value = value

    def accept(self, visitor):
        return visitor.visit_Exit(self)


class Enclose(Node):
    """Marks where function objects for ``funs`` would be allocated."""

    __slots__ = ("funs",)

    def __init__(self, lexenv, source, funs: list[Functional]):
        super().__init__(lexenv, source)
        self.funs = funs

    def accept(self, visitor):
        return visitor.visit_Enclose(self)


_NODE_VISITOR_FALLBACKS = (
    ("visit_ValuedNode", "visit_Node"),
    ("visit_Ref", "visit_ValuedNode"),
    ("visit_If", "visit_Node"),
    ("visit_Set", "visit_ValuedNode"),
    ("visit_Combination", "visit_ValuedNode"),
    ("visit_Bind", "visit_Node"),
    ("visit_Return", "visit_Node"),
    ("visit_Cast", "visit_ValuedNode"),
    ("visit_Entry", "visit_Node"),
    ("visit_Exit", "visit_ValuedNode"),
    ("visit_Enclose", "visit_Node"),
)

_LEAF_VISITOR_FALLBACKS = (
    ("visit_Constant", "visit_Leaf"),
    ("visit_KnownFunction", "visit_Leaf"),
    ("visit_BasicVar", "visit_Leaf"),
    ("visit_GlobalVar", "visit_BasicVar"),
    ("visit_LambdaVar", "visit_BasicVar"),
    ("visit_Functional", "visit_Leaf"),
    ("visit_Lambda", "visit_Functional"),
    ("visit_OptionalDispatch", "visit_Functional"),
)


def _bind_visitor_fallbacks(cls, fallbacks) -> None:
    if "_explicit_visits" not in cls.__dict__:
        cls._explicit_visits = frozenset(
            name for name in cls.__dict__ if name.startswith("visit_")
        )
    mro = [c for c in cls.__mro__ if "_explicit_visits" in c.__dict__]
    for child, parent in fallbacks:
        for c in mro:
            if child in c._explicit_visits:
                setattr(cls, child, c.__dict__[child])
                break
        else:
            setattr(cls, child, getattr(cls, parent))


class NodeVisitor:
    """A node selects its visitor method directly; unhandled kinds fall back
    to their parent class's method, ending at ``visit_Node``."""

    _explicit_visits: frozenset[str] = frozenset()

    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)
        _bind_visitor_fallbacks(cls, _NODE_VISITOR_FALLBACKS)

    def visit_Node(self, node):
        return None

    def visit_ValuedNode(self, node):
        return self.visit_Node(node)

    def visit_Ref(self, node):
        return self.visit_ValuedNode(node)

    def visit_If(self, node):
        return self.visit_Node(node)

    def visit_Set(self, node):
        return self.visit_ValuedNode(node)

    def visit_Combination(self, node):
        return self.visit_ValuedNode(node)

    def visit_Bind(self, node):
        return self.visit_Node(node)

    def visit_Return(self, node):
        return self.visit_Node(node)

    def visit_Cast(self, node):
        return self.visit_ValuedNode(node)

    def visit_Entry(self, node):
        return self.visit_Node(node)

    def visit_Exit(self, node):
        return self.visit_ValuedNode(node)

    def visit_Enclose(self, node):
        return self.visit_Node(node)


class LeafVisitor:
    """A leaf selects its visitor method directly; unhandled kinds fall back
    to their parent class's method, ending at ``visit_Leaf``."""

    _explicit_visits: frozenset[str] = frozenset()

    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)
        _bind_visitor_fallbacks(cls, _LEAF_VISITOR_FALLBACKS)

    def visit_Leaf(self, leaf):
        return None

    def visit_Constant(self, leaf):
        return self.visit_Leaf(leaf)

    def visit_KnownFunction(self, leaf):
        return self.visit_Leaf(leaf)

    def visit_BasicVar(self, leaf):
        return self.visit_Leaf(leaf)

    def visit_GlobalVar(self, leaf):
        return self.visit_BasicVar(leaf)

    def visit_LambdaVar(self, leaf):
        return self.visit_BasicVar(leaf)

    def visit_Functional(self, leaf):
        return self.visit_Leaf(leaf)

    def visit_Lambda(self, leaf):
        return self.visit_Functional(leaf)

    def visit_OptionalDispatch(self, leaf):
        return self.visit_Functional(leaf)


class Leaf:
    """Something a ``Ref`` can reference: a constant, variable, or function."""

    __slots__ = ("info", "number", "refs", "source_name", "type")

    # Only a local can be a capture belonging to expression evaluation.
    expression_temporary = False

    def __init__(self, source_name: str | None = None):
        self.source_name = source_name
        self.refs: list[Ref] = []
        self.type = None
        self.info = None
        self.number = _number()

    def __repr__(self):
        return f"<{type(self).__name__} {self.source_name or self.number}>"

    def accept(self, visitor: LeafVisitor):
        return visitor.visit_Leaf(self)


class Constant(Leaf):
    __slots__ = ("cached_fact", "value")

    def __init__(self, value: ast.Literal | ast.Tuple):
        super().__init__()
        self.value = value
        self.cached_fact = None

    def accept(self, visitor: LeafVisitor):
        return visitor.visit_Constant(self)


class KnownFunction(Leaf):
    """A compiler-known operation from the function database (``fndb``)."""

    __slots__ = ("fun_info",)

    def __init__(self, fun_info):
        super().__init__(fun_info.name)
        self.fun_info = fun_info

    def accept(self, visitor: LeafVisitor):
        return visitor.visit_KnownFunction(self)


class BasicVar(Leaf):
    __slots__ = ("sets",)

    def __init__(self, source_name: str):
        super().__init__(source_name)
        self.sets: list[Set] = []

    def accept(self, visitor: LeafVisitor):
        return visitor.visit_BasicVar(self)


class GlobalVar(BasicVar):
    """A module-namespace binding, looked up at execution time."""

    __slots__ = ("namespace",)

    def __init__(self, name: str, namespace: str):
        super().__init__(name)
        self.namespace = namespace

    @property
    def name(self) -> str:
        return self.source_name or "" or ""

    def accept(self, visitor: LeafVisitor):
        return visitor.visit_GlobalVar(self)


class LambdaVar(BasicVar):
    """A function local. ``hidden`` marks compiler temporaries."""

    __slots__ = (
        "arg_info",
        "expression_temporary",
        "hidden",
        "home",
        "indirect",
        "optimistic_type",
    )

    def __init__(self, name: str, *, hidden=False):
        super().__init__(name)
        self.home: Lambda | None = None
        self.arg_info = None
        self.indirect = False
        self.hidden = hidden
        # These captures belong to expression evaluation, not the enclosing
        # function's locals, and must be released before an exception transfer.
        self.expression_temporary = False
        self.optimistic_type = None

    @property
    def name(self) -> str:
        return self.source_name or ""

    def accept(self, visitor: LeafVisitor):
        return visitor.visit_LambdaVar(self)


class FunctionalKind(Enum):
    NORMAL = auto()
    LET = auto()
    ASSIGNMENT = auto()
    OPTIONAL = auto()
    EXTERNAL = auto()
    TOPLEVEL = auto()
    DELETED = auto()


class Functional(Leaf):
    __slots__ = ("debug_name", "entry_fun", "kind", "lexenv")

    def __init__(self, source_name, *, debug_name=None, lexenv=None):
        super().__init__(source_name)
        self.kind = FunctionalKind.NORMAL
        self.entry_fun: Functional | None = None
        self.debug_name = debug_name
        self.lexenv = lexenv

    def accept(self, visitor: LeafVisitor):
        return visitor.visit_Functional(self)


@dataclass(slots=True)
class Signature:
    """Python parameter metadata a lambda carries (SBCL's arg-info, flattened)."""

    kinds: tuple[ast.ParameterKind, ...] = ()
    default_parameters: tuple[int, ...] = ()
    default_expressions: tuple = ()
    variadic: bool = False
    annotations: tuple = ()
    return_annotation: object = None
    interface_declaration: bool = False
    annotation_expressions: tuple = ()
    return_annotation_expression: ast.Expression | None = None

    @property
    def implementation_return_annotation(self):
        """A declaration describes an interface, not its placeholder body."""
        return None if self.interface_declaration else self.return_annotation

    @property
    def positional_body(self) -> bool:
        return not self.variadic and ast.ParameterKind.KEYWORD_ONLY not in self.kinds

    def call_order(self, parameters, keywords, count, *, receiver=False):
        """Map a complete static call onto parameter positions: an index into
        the arguments, ``-1`` for the receiver, or ``-2 - n`` for default n.
        Returns ``None`` when the call must use the external entry."""
        if self.variadic:
            return None
        kinds = self.kinds or (ast.ParameterKind.POSITIONAL_ONLY,) * len(parameters)
        order = [None] * len(parameters)
        positional = int(receiver)
        if receiver:
            if not order or kinds[0] == ast.ParameterKind.KEYWORD_ONLY:
                return None
            order[0] = -1
        for index, keyword in enumerate(keywords or (None,) * count):
            if keyword is None:
                target = positional
                positional += 1
                if (
                    target >= len(order)
                    or kinds[target] == ast.ParameterKind.KEYWORD_ONLY
                ):
                    return None
            else:
                try:
                    target = parameters.index(keyword)
                except ValueError:
                    return None
                if kinds[target] == ast.ParameterKind.POSITIONAL_ONLY:
                    return None
            if order[target] is not None:
                return None
            order[target] = index
        defaults = {
            parameter: offset
            for offset, parameter in enumerate(self.default_parameters)
        }
        for parameter, argument in enumerate(order):
            if argument is None:
                if parameter not in defaults:
                    return None
                order[parameter] = -2 - defaults[parameter]
        return tuple(order)

    def bind_parameter_types(
        self,
        parameters: tuple[str, ...],
        arg_types: tuple[ValueType, ...],
        keywords: tuple[str | None, ...] = (),
        *,
        receiver_type: ValueType | None = None,
        default_types: tuple[ValueType, ...] = (),
    ) -> list[ValueType] | None:
        """Bind call-site argument types to ``parameters`` using Python's
        calling convention, including ``*args``, ``**kwargs``, and defaults."""
        kinds = self.kinds or (ast.ParameterKind.POSITIONAL_ONLY,) * len(parameters)
        if len(kinds) != len(parameters):
            return None
        bound: list[ValueType | None] = [None] * len(parameters)
        var_pos_idx: int | None = None
        var_kw_idx: int | None = None
        for idx, kind in enumerate(kinds):
            if kind == ast.ParameterKind.VAR_POSITIONAL:
                var_pos_idx = idx
            elif kind == ast.ParameterKind.VAR_KEYWORD:
                var_kw_idx = idx
        var_pos_elems: list[ValueType] = []
        var_kw_vals: list[ValueType] = []
        positional = 0
        if receiver_type is not None:
            if not parameters or kinds[0] in (
                ast.ParameterKind.KEYWORD_ONLY,
                ast.ParameterKind.VAR_KEYWORD,
            ):
                return None
            if kinds[0] == ast.ParameterKind.VAR_POSITIONAL:
                var_pos_elems.append(receiver_type)
            else:
                bound[0] = receiver_type
                positional = 1
        if keywords == ("*",) and len(arg_types) == 1:
            if var_pos_idx is not None and all(
                b is not None or idx == var_pos_idx or idx in self.default_parameters
                for idx, b in enumerate(bound)
            ):
                bound[var_pos_idx] = TupleType()
                defaults = {
                    param_idx: offset
                    for offset, param_idx in enumerate(self.default_parameters)
                }
                res_star: list[ValueType] = []
                for idx, val in enumerate(bound):
                    if val is None:
                        offset = defaults[idx]
                        val = (
                            default_types[offset]
                            if offset < len(default_types)
                            else UNKNOWN
                        )
                    res_star.append(val)
                return res_star
            return None
        kw_list = keywords or (None,) * len(arg_types)
        if len(kw_list) != len(arg_types):
            return None
        for arg_type, keyword in zip(arg_types, kw_list, strict=True):
            if keyword is None:
                while (
                    positional < len(parameters)
                    and kinds[positional]
                    in (
                        ast.ParameterKind.POSITIONAL_ONLY,
                        ast.ParameterKind.POSITIONAL_OR_KEYWORD,
                    )
                    and bound[positional] is not None
                ):
                    positional += 1
                if positional < len(parameters) and kinds[positional] in (
                    ast.ParameterKind.POSITIONAL_ONLY,
                    ast.ParameterKind.POSITIONAL_OR_KEYWORD,
                ):
                    bound[positional] = arg_type
                    positional += 1
                elif var_pos_idx is not None:
                    var_pos_elems.append(arg_type)
                else:
                    return None
            else:
                if keyword in parameters:
                    target = parameters.index(keyword)
                    if kinds[target] in (
                        ast.ParameterKind.POSITIONAL_OR_KEYWORD,
                        ast.ParameterKind.KEYWORD_ONLY,
                    ):
                        if bound[target] is not None:
                            return None
                        bound[target] = arg_type
                        continue
                if var_kw_idx is not None:
                    var_kw_vals.append(arg_type)
                else:
                    return None
        if var_pos_idx is not None:
            arity = len(var_pos_elems)
            bound[var_pos_idx] = (
                TupleType(arity, tuple(var_pos_elems) if arity <= 16 else None)
                if arity > 0
                else TupleType(0)
            )
        if var_kw_idx is not None:
            if not var_kw_vals:
                bound[var_kw_idx] = EMPTY_DICT_TYPE
            else:
                joined_val = var_kw_vals[0]
                for val_type in var_kw_vals[1:]:
                    joined_val = joined_val.join(val_type)
                bound[var_kw_idx] = DictType(TEXT_TYPE, joined_val)
        defaults = {
            param_idx: offset
            for offset, param_idx in enumerate(self.default_parameters)
        }
        result: list[ValueType] = []
        for idx, val in enumerate(bound):
            if val is None:
                if idx not in defaults:
                    return None
                offset = defaults[idx]
                val = default_types[offset] if offset < len(default_types) else UNKNOWN
            result.append(val)
        return result


@dataclass(frozen=True, slots=True)
class CodeInfo:
    """What a Python code object reports about a function: its names, the
    file it was compiled from, and the line its definition starts on."""

    name: str
    qualname: str
    filename: str
    firstlineno: int
    flags: int = 0


class Lambda(Functional):
    """A function body: ``vars`` are parameters, ``locals`` every local
    including parameters and hidden temporaries."""

    __slots__ = (
        "annotation_expressions",
        "bind",
        "binding",
        "branch_deriver",
        "calls",
        "code",
        "component",
        "coroutine_body",
        "declared_free",
        "default_types",
        "entries",
        "environment",
        "generator_body",
        "generator_outer",
        "home",
        "ir1_revision",
        "is_protocol",
        "lets",
        "local_annotations",
        "locals",
        "method_class",
        "name",
        "namespace",
        "optional_dispatch",
        "owner_class",
        "policy",
        "return_",
        "self_type_deriver",
        "signature",
        "type_deriver",
        "vars",
    )

    def __init__(
        self,
        name: str,
        source_name,
        *,
        policy: OptimizationPolicy,
        namespace: str,
        lexenv=None,
    ):
        super().__init__(source_name, lexenv=lexenv)
        self.name = name
        self.vars: list[LambdaVar] = []
        self.locals: list[LambdaVar] = []
        self.bind: Bind | None = None
        self.return_: Return | None = None
        self.home: Lambda = self
        self.lets: list[Lambda] = []
        self.entries: list[Entry] = []
        self.calls: list[Lambda] = []
        self.component: Component | None = None
        self.environment: Environment | None = None
        self.optional_dispatch = None
        self.policy = policy
        self.namespace = namespace
        self.code: CodeInfo | None = None
        self.coroutine_body: bool = False
        self.generator_body: Lambda | None = None
        self.generator_outer: Lambda | None = None
        self.owner_class: str | None = None
        # The identity of the class whose body defines this function, for
        # receiver typing; ``owner_class`` names native layouts only.
        self.method_class: str | None = None
        self.is_protocol: bool = name.startswith(
            (
                "mule_sequences_",
                "mule_format_",
                "mule_arguments_",
                "mule_native_class_",
                "mule_print_",
            )
        )
        self.signature = Signature()
        self.local_annotations: dict[str, Contract] = {}
        self.annotation_expressions: dict[str, ast.Expression] = {}
        # The constant signature tuple ``_arguments.bind`` binds calls with.
        self.binding: ast.Tuple | None = None
        # Enclosing variables a ``nonlocal`` declaration makes free here.
        # Python closes over them even when the body never uses them.
        self.declared_free: list[LambdaVar] = []
        self.default_types: tuple[ValueType, ...] = ()
        self.type_deriver: Callable[[tuple[ValueType, ...]], ValueType] | None = None
        self.self_type_deriver: (
            Callable[[tuple[ValueType, ...]], ValueType | None] | None
        ) = None
        self.branch_deriver: (
            Callable[[tuple[ValueType, ...], bool], tuple[ValueType, ...] | None] | None
        ) = None
        self.ir1_revision: int = 0

    def as_lambda(self) -> Lambda | None:
        return self

    def global_constant_type(self) -> ValueType:
        return UNKNOWN

    def summary_return_type(self) -> ValueType:
        if self.return_ is None or self.return_.result_type is None:
            return UNKNOWN
        return self.return_.result_type

    @property
    def parameters(self) -> tuple[str, ...]:
        return tuple(var.name for var in self.vars)

    def accept(self, visitor: LeafVisitor):
        return visitor.visit_Lambda(self)

    @property
    def takes_closure(self) -> bool:
        """Whether the body receives captured variables."""
        environment = self.environment
        return environment is not None and bool(environment.closure)

    def call_order(self, keywords, count, *, receiver=False):
        return self.signature.call_order(
            self.parameters, keywords, count, receiver=receiver
        )

    def _bind_call_parameters(
        self,
        arg_types: tuple[ValueType, ...],
        keywords: tuple[str | None, ...] = (),
        *,
        receiver_type: ValueType | None = None,
    ) -> tuple[ValueType, ...] | None:
        bound = self.signature.bind_parameter_types(
            self.parameters,
            arg_types,
            keywords,
            receiver_type=receiver_type,
            default_types=self.default_types,
        )
        return None if bound is None else tuple(bound)

    def derive_return_type(
        self,
        arg_types: tuple[ValueType, ...],
        keywords: tuple[str | None, ...] = (),
        *,
        receiver_type: ValueType | None = None,
    ) -> ValueType:
        if any(arg is BOTTOM for arg in arg_types) or receiver_type is BOTTOM:
            return BOTTOM
        deriver = self.type_deriver
        if deriver is None:
            return UNKNOWN
        param_types = self._bind_call_parameters(
            arg_types, keywords, receiver_type=receiver_type
        )
        if param_types is None:
            return UNKNOWN
        return deriver(param_types)

    def derive_self_type(
        self,
        arg_types: tuple[ValueType, ...],
        keywords: tuple[str | None, ...] = (),
        *,
        receiver_type: ValueType | None = None,
    ) -> ValueType | None:
        if any(arg is BOTTOM for arg in arg_types) or receiver_type is BOTTOM:
            return BOTTOM
        deriver = self.self_type_deriver
        if deriver is None:
            return None
        param_types = self._bind_call_parameters(
            arg_types, keywords, receiver_type=receiver_type
        )
        if param_types is None:
            return None
        return deriver(param_types)

    def constrain_call(self, node, visitor, state, truth: bool) -> bool:
        deriver = self.branch_deriver
        if deriver is None:
            return False
        keywords = (
            node.info.keywords if node.info is not None and node.info.keywords else ()
        )
        arg_types = tuple(visitor.type_of(arg) for arg in node.args)
        if any(arg is BOTTOM for arg in arg_types):
            return False
        param_types = self._bind_call_parameters(arg_types, keywords)
        if param_types is None:
            return False
        narrowed_params = deriver(param_types, truth)
        if narrowed_params is None:
            return False
        changed = False
        if not keywords:
            for idx, arg_lvar in enumerate(node.args):
                if idx >= len(narrowed_params):
                    break
                local = visitor.origins.get(arg_lvar)
                if local is None or local not in visitor.state:
                    continue
                narrowed_type = narrowed_params[idx]
                if narrowed_type is BOTTOM or narrowed_type is UNKNOWN:
                    continue
                current = state[local]
                if narrowed_type != current.type and narrowed_type.fits(current.type):
                    state[local] = visitor.narrowed_fact(current, narrowed_type)
                    changed = True
                src_var = visitor.type_of_origins.get(arg_lvar) or (
                    visitor.type_of_origins.get(local) if local is not None else None
                )
                if src_var is not None and src_var in visitor.state:
                    src_cur = state[src_var]
                    narrowed_src = src_cur.type.narrow_by_runtime_class(
                        narrowed_type, True
                    )
                    if narrowed_src is not BOTTOM and narrowed_src != src_cur.type:
                        state[src_var] = visitor.narrowed_fact(src_cur, narrowed_src)
                        changed = True
        return changed

    def propagate_call(
        self,
        node,
        visitor,
        args,
        arg_types: tuple[ValueType, ...],
        keywords: tuple[str | None, ...] = (),
    ) -> ValueType:
        return self.derive_return_type(arg_types, keywords)

    def protocol_self_type(self) -> ValueType:
        if not self.is_protocol or not self.vars or self.vars[0].name != "self":
            return UNKNOWN
        if self.code is None or "." not in self.code.qualname:
            return UNKNOWN
        cls_name = self.code.qualname.split(".", 1)[0]
        return STORAGE_CLASS_TYPES.get(cls_name, UNKNOWN)


@dataclass(frozen=True, slots=True)
class ClassConstructorTarget:
    """Compile-time callable target representing a class constructor
    (``__new__`` and/or ``__init__`` on ``base_type``)."""

    base_type: ValueType = UNKNOWN
    cls_type: ValueType = UNKNOWN
    new_target: Lambda | None = None
    init_target: Lambda | None = None
    records_type_origin: bool = False

    def as_lambda(self) -> Lambda | None:
        return None

    def global_constant_type(self) -> ValueType:
        return self.cls_type

    def summary_return_type(self) -> ValueType:
        return self.base_type.widen_constant()

    def derive_return_type(
        self,
        arg_types: tuple[ValueType, ...],
        keywords: tuple[str | None, ...] = (),
        *,
        receiver_type: ValueType | None = None,
    ) -> ValueType:
        if self.records_type_origin and len(arg_types) == 1 and not keywords:
            if arg_types[0] is BOTTOM:
                return BOTTOM
            if self.new_target is None:
                return arg_types[0].runtime_class_type()
        result_type = self.base_type
        if self.new_target is not None:
            derived = self.new_target.derive_return_type(
                arg_types, keywords, receiver_type=self.cls_type
            )
            if derived is BOTTOM:
                return BOTTOM
            if derived is not UNKNOWN:
                result_type = derived
        if self.init_target is not None:
            mutated = self.init_target.derive_self_type(
                arg_types, keywords, receiver_type=result_type
            )
            if mutated is BOTTOM:
                return BOTTOM
            if mutated is not None and mutated is not UNKNOWN:
                result_type = mutated
        return result_type

    def derive_self_type(
        self,
        arg_types: tuple[ValueType, ...],
        keywords: tuple[str | None, ...] = (),
        *,
        receiver_type: ValueType | None = None,
    ) -> ValueType | None:
        return None

    def constrain_call(self, node, visitor, state, truth: bool) -> bool:
        return False

    def propagate_call(
        self,
        node,
        visitor,
        args,
        arg_types: tuple[ValueType, ...],
        keywords: tuple[str | None, ...] = (),
    ) -> ValueType:
        if self.records_type_origin and len(args) == 1 and not keywords:
            src_var = visitor.origins.get(args[0])
            if src_var is not None and node.lvar is not None:
                visitor.type_of_origins[node.lvar] = src_var
        return self.derive_return_type(arg_types, keywords)


@dataclass(frozen=True, slots=True)
class ForeignCallTarget:
    """Compile-time callable target for a foreign runtime function (such as an
    export from ``_mule_native``) with encapsulated return-type and branch
    derivation."""

    return_type: ValueType = UNKNOWN
    derive_fn: Callable | None = None
    propagate_fn: Callable | None = None
    constrain_fn: Callable | None = None

    def as_lambda(self) -> Lambda | None:
        return None

    def global_constant_type(self) -> ValueType:
        return UNKNOWN

    def summary_return_type(self) -> ValueType:
        return self.return_type

    def derive_return_type(
        self,
        arg_types: tuple[ValueType, ...],
        keywords: tuple[str | None, ...] = (),
        *,
        receiver_type: ValueType | None = None,
    ) -> ValueType:
        if self.derive_fn is not None:
            return self.derive_fn(arg_types, keywords, receiver_type=receiver_type)
        return self.return_type

    def derive_self_type(
        self,
        arg_types: tuple[ValueType, ...],
        keywords: tuple[str | None, ...] = (),
        *,
        receiver_type: ValueType | None = None,
    ) -> ValueType | None:
        return None

    def constrain_call(self, node, visitor, state, truth: bool) -> bool:
        if self.constrain_fn is not None:
            return self.constrain_fn(node, visitor, state, truth)
        return False

    def propagate_call(
        self,
        node,
        visitor,
        args,
        arg_types: tuple[ValueType, ...],
        keywords: tuple[str | None, ...] = (),
    ) -> ValueType:
        if self.propagate_fn is not None:
            res = self.propagate_fn(node, visitor, args, arg_types, keywords)
            if res is not None:
                return res
        return self.derive_return_type(arg_types, keywords)


def _derive_len_builtin(
    arg_types: tuple[ValueType, ...],
    keywords: tuple[str | None, ...] = (),
    *,
    receiver_type: ValueType | None = None,
) -> ValueType:
    if len(arg_types) != 1 or keywords:
        return UNKNOWN
    if arg_types[0] is BOTTOM:
        return BOTTOM
    exact = arg_types[0].exact_length()
    if exact is not None:
        return IntegerType(exact, exact)
    return IntegerType(0, (1 << 60) - 1)


BOOTSTRAP_BUILTIN_TARGETS: dict[
    str, Lambda | ClassConstructorTarget | ForeignCallTarget
] = {
    "int": ClassConstructorTarget(INTEGER, CLASS_INT_TYPE),
    "bool": ClassConstructorTarget(BOOLEAN, CLASS_BOOL_TYPE),
    "float": ClassConstructorTarget(FLOAT, CLASS_FLOAT_TYPE),
    "str": ClassConstructorTarget(TEXT_TYPE, CLASS_STR_TYPE),
    "bytes": ClassConstructorTarget(BYTES_TYPE, CLASS_BYTES_TYPE),
    "bytearray": ClassConstructorTarget(BYTEARRAY_TYPE, CLASS_BYTEARRAY_TYPE),
    "tuple": ClassConstructorTarget(TupleType(), CLASS_TUPLE_TYPE),
    "list": ClassConstructorTarget(LIST_TYPE, CLASS_LIST_TYPE),
    "dict": ClassConstructorTarget(DICT_TYPE, CLASS_DICT_TYPE),
    "object": ClassConstructorTarget(UNKNOWN, CLASS_OBJECT_TYPE),
    "type": ClassConstructorTarget(
        ANY_CLASS_TYPE, CLASS_TYPE_TYPE, records_type_origin=True
    ),
    "len": ForeignCallTarget(
        IntegerType(0, (1 << 60) - 1), derive_fn=_derive_len_builtin
    ),
    "range": ForeignCallTarget(IteratorType(INTEGER)),
    "input": ForeignCallTarget(TEXT_TYPE),
}


class OptionalDispatch(Functional):
    """Dispatch from a full Python signature to argument-count entries.
    Populated by local-call analysis; conversion only records the arglist."""

    __slots__ = ("arglist", "entry_points", "main_entry", "more_entry")

    def __init__(self, source_name, arglist):
        super().__init__(source_name)
        self.arglist = arglist
        self.entry_points: list[Lambda] = []
        self.more_entry: Lambda | None = None
        self.main_entry: Lambda | None = None

    def accept(self, visitor: LeafVisitor):
        return visitor.visit_OptionalDispatch(self)


class CleanupKind(Enum):
    HANDLER_REGION = auto()
    HANDLER_BODY = auto()
    FINALLY = auto()
    WITH = auto()


@dataclass(slots=True, eq=False)
class Cleanup:
    """A protected region. ``target`` is the block that receives errors raised
    inside the region; ``binding`` names an ``except ... as`` variable the
    region must unbind on exit."""

    kind: CleanupKind
    target: Block | None = None
    mess_up: Node | None = None
    binding: str | None = None
    parent: Cleanup | None = None
    # Kind-specific data: the ``finally`` suite, or the ``with`` exit method's
    # temporary; and the loop targets in effect where the region was entered,
    # which a ``finally`` suite converted at an exit edge must see.
    info: object = None
    loops: tuple = ()
    # Class bodies being converted where the region was entered; cleanup code
    # converted at exit edges resolves names in their namespaces.
    class_scopes: tuple = ()
    nlx_info: list = field(default_factory=list)


@dataclass(slots=True, eq=False)
class LoopTarget:
    """Where ``continue`` and ``break`` transfer, and the cleanup depth to leave."""

    continue_block: Block
    break_block: Block
    cleanup: Cleanup | None


@dataclass(slots=True, eq=False)
class Lexenv:
    """Conversion-time context: visible bindings, the enclosing lambda, the
    innermost cleanup, loop targets, and the optimization policy."""

    vars: Mapping[str, Leaf]
    lambda_: Lambda
    cleanup: Cleanup | None = None
    policy: OptimizationPolicy = field(default_factory=OptimizationPolicy)
    loops: tuple[LoopTarget, ...] = ()
    parent: Lexenv | None = None

    def derive(self, **changes) -> Lexenv:
        return Lexenv(
            vars=changes.get("vars", self.vars),
            lambda_=changes.get("lambda_", self.lambda_),
            cleanup=changes.get("cleanup", self.cleanup),
            policy=changes.get("policy", self.policy),
            loops=changes.get("loops", self.loops),
            parent=self,
        )


@dataclass(slots=True, eq=False)
class Environment:
    """Result of environment analysis for one non-LET lambda."""

    lambda_: Lambda
    closure: list = field(default_factory=list)
    nlx_info: list = field(default_factory=list)
    info: object = None


class Block:
    """A basic block: a chain of nodes from ``start`` to ``last``."""

    __slots__ = (
        "component",
        "delete_p",
        "dominator",
        "flag",
        "flush_p",
        "in_",
        "info",
        "kill",
        "last",
        "loop",
        "next",
        "number",
        "out",
        "pred",
        "prev",
        "reoptimize",
        "start",
        "succ",
        "type_check",
    )

    def __init__(
        self, start: Ctran | None = None, *, component: Component | None = None
    ):
        self.start = start
        self.last: Node | None = None
        self.pred: list[Block] = []
        self.succ: list[Block] = []
        self.next: Block | None = None
        self.prev: Block | None = None
        self.component = component
        self.reoptimize = True
        self.flush_p = True
        self.type_check = True
        self.delete_p = False
        self.flag = None
        self.number = 0
        self.dominator = None
        self.loop = None
        self.kill = None
        self.in_ = None
        self.out = None
        self.info = None

    def nodes(self):
        """Iterate the node chain from ``start`` through ``last``."""
        ctran = self.start
        while ctran is not None:
            node = ctran.next
            if node is None:
                return
            yield node
            if node is self.last:
                return
            ctran = node.next

    def dominated_by(self, other):
        block = self
        while block is not None:
            if block is other:
                return True
            block = block.dominator
        return False

    def __repr__(self):
        if self.component is not None and self is self.component.head:
            return "<Block HEAD>"
        if self.component is not None and self is self.component.tail:
            return "<Block TAIL>"
        return f"<Block {self.number} start={self.start!r}>"


class Loop:
    """A flow-graph loop with its immediate nesting and exit metadata."""

    is_natural = False

    def __init__(self, head, tails=()):
        self.head = head
        self.tails = list(tails)
        self.blocks = []
        self.inferiors = []
        self.superior = None
        self.exits = []
        self.depth = 0

    def all_blocks(self) -> set[Block]:
        blocks: set[Block] = set()
        pending = [self]
        while pending:
            loop = pending.pop()
            blocks.update(loop.blocks)
            pending.extend(loop.inferiors)
        return blocks

    def accepts(self, block):
        raise TypeError(f"loop membership is undefined for {self!r}")

    def predecessor_seeds(self, predecessors):
        return predecessors[self.head]

    def completion_seeds(self, predecessors):
        return ()

    def is_header(self, block):
        return block is self.head

    def attach(self, inner):
        if inner.superior is not None:
            raise ValueError("loop already has a superior")
        inner.superior = self
        self.inferiors.append(inner)

    def assign_depths(self):
        pending = [(self, 0)]
        while pending:
            loop, depth = pending.pop()
            loop.depth = depth
            pending.extend((inner, depth + 1) for inner in loop.inferiors)


class NaturalLoop(Loop):
    is_natural = True

    def accepts(self, block):
        return block.dominated_by(self.head)


class StrangeLoop(Loop):
    def __init__(self, head, tails, successors):
        super().__init__(head, tails)
        self._segments = set()
        pending = [head]
        while pending:
            block = pending.pop()
            if block in self._segments:
                continue
            self._segments.add(block)
            pending.extend(
                successor
                for successor in successors[block]
                if successor.number < head.number
            )

    def accepts(self, block):
        return block in self._segments

    def predecessor_seeds(self, predecessors):
        seeds = []
        pending = [self]
        while pending:
            loop = pending.pop()
            for block in loop.blocks:
                seeds.extend(predecessors[block])
            pending.extend(loop.inferiors)
        return seeds


class OuterLoop(Loop):
    def accepts(self, block):
        return True

    def is_header(self, block):
        return False

    def completion_seeds(self, predecessors):
        seeds = []
        for inner in self.inferiors:
            seeds.extend(inner.predecessor_seeds(predecessors))
        return seeds


class ComponentKind(Enum):
    NORMAL = auto()
    TOPLEVEL = auto()
    INITIAL = auto()
    DELETED = auto()


class Component:
    """A connected piece of the flow graph with dummy ``head``/``tail`` blocks."""

    __slots__ = (
        "constants",
        "delete_blocks",
        "failed_optimizations",
        "head",
        "info",
        "kind",
        "lambdas",
        "last_block",
        "name",
        "new_functionals",
        "outer_loop",
        "reanalyze",
        "reanalyze_functionals",
        "reoptimize",
        "tail",
    )

    def __init__(self, name="<unknown>"):
        self.head = Block(component=self)
        self.tail = Block(component=self)
        self.outer_loop = OuterLoop(self.head, (self.tail,))
        self.head.flag = True
        self.tail.flag = True
        self.head.next = self.tail
        self.tail.prev = self.head
        self.last_block = self.tail
        self.kind = ComponentKind.INITIAL
        self.lambdas: list[Lambda] = []
        self.new_functionals: list[Lambda] = []
        self.reanalyze_functionals: list[Functional] = []
        self.reoptimize: bool | Literal["maybe"] = True
        self.reanalyze = False
        self.name = name
        self.info = None
        self.delete_blocks: list[Block] = []
        self.failed_optimizations: dict = {}
        self.constants: dict = {}

    def blocks(self):
        """Blocks in depth-first order, excluding the dummy head and tail."""
        block = linked(self.head.next, self.head, "next block")
        while block is not self.tail:
            following = block.next
            yield block
            block = linked(following, block, "next block")

    def __repr__(self):
        return f"<Component {self.name}>"
