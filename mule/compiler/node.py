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
from collections.abc import Mapping
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Literal

from mule import ast

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

    def __init__(
        self, lexenv, source, leaf: Leaf, *, final=False, allow_unbound=False
    ):
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

    __slots__ = ("value", "var")

    def __init__(self, lexenv, source, var: BasicVar, value: Lvar | None):
        super().__init__(lexenv, source)
        self.var = var
        self.value = value

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
    __slots__ = ("value",)

    def __init__(self, value: ast.Literal | ast.Tuple):
        super().__init__()
        self.value = value

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

    source_name: str

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
        return self.source_name

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
        return self.source_name

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
        "calls",
        "code",
        "component",
        "declared_free",
        "entries",
        "environment",
        "home",
        "lets",
        "local_annotations",
        "locals",
        "name",
        "namespace",
        "optional_dispatch",
        "owner_class",
        "policy",
        "return_",
        "signature",
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
        self.owner_class: str | None = None
        self.signature = Signature()
        self.local_annotations: dict[str, object] = {}
        self.annotation_expressions: dict[str, ast.Expression] = {}
        # The constant signature tuple ``_arguments.bind`` binds calls with.
        self.binding: ast.Tuple | None = None
        # Enclosing variables a ``nonlocal`` declaration makes free here.
        # Python closes over them even when the body never uses them.
        self.declared_free: list[LambdaVar] = []

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

    def __init__(self, head, tails=()):
        self.head = head
        self.tails = list(tails)
        self.blocks = []
        self.inferiors = []
        self.superior = None
        self.exits = []
        self.depth = 0

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

    def blocks(self):
        """Blocks in depth-first order, excluding the dummy head and tail."""
        block = linked(self.head.next, self.head, "next block")
        while block is not self.tail:
            following = block.next
            yield block
            block = linked(following, block, "next block")

    def __repr__(self):
        return f"<Component {self.name}>"
