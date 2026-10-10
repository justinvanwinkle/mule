"""Convert Mule syntax trees into IR1, following ``src/compiler/ir1tran.lisp``
and ``ir1-translators.lisp``.

Every translator receives ``start`` (the ctran control arrives on), ``next``
(the ctran control leaves on) and ``result`` (the lvar that receives the
value, or ``None`` when the value is unused), and links the nodes it creates
between them. Statement bodies instead return their open *frontier* ctran, or
``None`` when the last statement transferred control elsewhere; Python's
``return``/``break``/``continue``/``raise`` make that knowledge necessary where
Lisp's ``return-from`` simply leaves ``next`` unreached.

Python adaptations beyond those listed in ``node.py``:

- Scope classification runs over the whole function before conversion
  (``_ScopeVisitor``/``_LocalBindings``), because a later assignment makes an
  earlier read local. This replaces Lisp's lexically scoped bindings.
- Module globals are ``GlobalVar`` leaves looked up at execution time; a name
  spelled like a builtin proves nothing about its value.
- ``for`` over a call spelled ``range`` emits a guarded counted loop beside
  the general iteration protocol. This is a source transform in SBCL's sense
  (keyed on the global name at conversion time); the runtime identity guard
  keeps it correct when the binding changes. It moves to an optimizer
  transform on ``%iterate`` once IR1 optimization exists.
- Cleanups for handler regions and handler bodies change at block boundaries
  only, so a block's error destination is a property of its first node.
- Conditional values (``a or b``, ``x if c else y``, comparison chains) use
  hidden temporaries assigned in each branch, mirroring the current backend's
  single-producer values; a multi-use result lvar replaces this when the
  backend consumes IR1 directly.
"""

from __future__ import annotations

from dataclasses import dataclass, replace

from mule import ast

from . import fndb
from .annotation_types import BUILTINS, EXPORTS
from .ir1util import (
    aver,
    ctran_starts_block,
    insert_cast_before,
    link_blocks,
    link_node_to_previous_ctran,
    use_continuation,
    use_ctran,
    use_lvar,
)
from .node import (
    Block,
    CallKind,
    Cast,
    Cleanup,
    CleanupKind,
    Combination,
    Constant,
    Ctran,
    CtranKind,
    Enclose,
    Exit,
    GlobalVar,
    If,
    KnownFunction,
    Lambda,
    LambdaVar,
    Lexenv,
    LoopTarget,
    Lvar,
    Ref,
    Set,
)
from .policy import OptimizationPolicy


class UnsupportedSyntax(SyntaxError):
    """Valid Python syntax that the native compiler cannot yet lower."""


@dataclass(frozen=True, slots=True)
class CallInfo:
    keywords: tuple[str | None, ...]
    location: tuple | None


@dataclass(frozen=True, slots=True)
class AttributeInfo:
    name: str
    location: tuple | None


@dataclass(frozen=True, slots=True)
class FunctionInfo:
    public_name: str
    default_contracts: tuple
    location: tuple | None


@dataclass(frozen=True, slots=True)
class ClassInfo:
    public_name: str
    has_base: bool
    fields: tuple[str, ...]
    method_names: tuple[str, ...]
    field_annotations: tuple
    policy: OptimizationPolicy
    location: tuple | None
    class_id: str | None
    annotation_expressions: tuple


@dataclass(frozen=True, slots=True)
class ImportInfo:
    module_name: str
    initializer: str
    filename: str


@dataclass(frozen=True, slots=True)
class FormatInfo:
    conversion: str | None


@dataclass(frozen=True, slots=True)
class InplaceInfo:
    reuse_local: str | None


@dataclass(slots=True, eq=False)
class RangeState:
    """Identity of one guarded counted loop's iteration state."""

    minimum: int
    maximum: int


class Namespace:
    """Per-compilation leaf tables: free (global) variables, known functions,
    and constants (SBCL's ``*ir1-namespace*``)."""

    def __init__(self):
        self.free_vars: dict[tuple[str, str], GlobalVar] = {}
        self.known: dict[str, KnownFunction] = {}

    def global_var(self, namespace: str, name: str) -> GlobalVar:
        key = namespace, name
        var = self.free_vars.get(key)
        if var is None:
            var = self.free_vars[key] = GlobalVar(name, namespace)
        return var

    def known_function(self, info) -> KnownFunction:
        leaf = self.known.get(info.name)
        if leaf is None:
            leaf = self.known[info.name] = KnownFunction(info)
        return leaf


class Converter:
    """Per-module conversion state (SBCL's compilation-unit variables)."""

    def __init__(
        self,
        source: str,
        filename: str,
        *,
        imports=None,
        prefix="",
        namespace="__main__",
        component=None,
        ir1_namespace: Namespace | None = None,
    ):
        self.source = source.replace("\r\n", "\n").replace("\r", "\n")
        self.filename = filename
        self.imports = imports
        self.prefix = prefix
        self.namespace = namespace
        self.component = component
        self.ir1_namespace = ir1_namespace or Namespace()
        self.functions: list[Lambda] = []
        self.compiler_bindings = {}
        self.active_compiler_bindings = {}
        self.module_annotations = {}
        self.class_definitions = 0
        self.class_fields = {}
        self.module_annotation_shadows = set()

    def location(self, node):
        span = node.span
        start = span.start if span else ast.Position(1, 0)
        end = span.end if span else ast.Position(1, 1)
        lines = self.source.splitlines(keepends=True)
        line = lines[start.line - 1] if start.line <= len(lines) else None
        return (
            self.filename,
            start.line,
            start.column + 1,
            line,
            end.line,
            end.column + 1,
        )

    def error(self, message: str, node: ast.Node, *, unsupported=False):
        error_type = UnsupportedSyntax if unsupported else SyntaxError
        raise error_type(message, self.location(node))

    def imported_binding(self, name, node):
        binding = self.active_compiler_bindings.get(name)
        if binding is None:
            self.error(
                f"'{name}' is not an imported Mule annotation or compiler directive",
                node,
                unsupported=True,
            )
        return binding

    def annotation(self, expression, scope=None):
        return expression.accept(_AnnotationReference(self, scope)).annotation(
            self, expression
        )

    def policy(self, decorators, inherited):
        policy = inherited
        for decorator in decorators:
            policy = decorator.accept(_PolicyDecorator(self, policy))
        return policy

    def reserved(self, name, node):
        if name in self.compiler_bindings:
            self.error(f"cannot shadow compiler-only import '{name}'", node)

    def global_var(self, name) -> GlobalVar:
        return self.ir1_namespace.global_var(self.namespace, name)

    def known(self, info) -> KnownFunction:
        return self.ir1_namespace.known_function(info)

    def function(self, definition, policy, *, owner_class=None) -> Lambda:
        # ir1tran_lambda imports FunctionConverter from this module.
        from .ir1tran_lambda import convert_function

        return convert_function(self, definition, policy, owner_class=owner_class)


class FunctionConverter:
    """Converts one function body (or the module body) into one Lambda."""

    def __init__(
        self,
        converter: Converter,
        lambda_: Lambda,
        statements: list[ast.Statement],
        *,
        is_module: bool,
        origin=None,
    ):
        self.converter = converter
        self.lambda_ = lambda_
        self.is_module = is_module
        self.origin = origin
        self.depth = 0
        self.annotations = {}
        self.hidden = 0
        self.globals: set[str] = set()
        self.locals: dict[str, LambdaVar] = {}
        for var in lambda_.vars:
            self.locals[var.name] = var
        self._bindings(statements)
        self.lexenv = Lexenv(vars=self.locals, lambda_=lambda_, policy=lambda_.policy)
        self.expressions = _Expressions(self)
        self.statements_visitor = _Statements(self)
        self.frames: list[tuple] = []

    @property
    def component(self):
        return self.converter.component

    def error(self, message: str, node: ast.Node, *, unsupported=False):
        self.converter.error(message, node, unsupported=unsupported)

    def _bindings(self, statements):
        """Classify the whole function, including assignments after an early read."""
        scope = _ScopeVisitor(self)
        for statement in statements:
            statement.accept(scope)
        if not self.is_module:
            collector = _LocalBindings(self)
            for statement in statements:
                statement.accept(collector)

    def local(self, name: str) -> bool:
        return name in self.locals

    def declare_local(self, name: str, *, hidden=False) -> LambdaVar:
        var = self.locals.get(name)
        if var is None:
            var = LambdaVar(name, hidden=hidden)
            var.home = self.lambda_
            self.locals[name] = var
            self.lambda_.locals.append(var)
        return var

    def temporary(self) -> LambdaVar:
        name = f"$merge{self.hidden}"
        self.hidden += 1
        return self.declare_local(name, hidden=True)

    def variable(self, name: str):
        var = self.locals.get(name)
        if var is not None:
            return var
        return self.converter.global_var(name)

    def annotation_for(self, name):
        if self.local(name):
            return self.annotations.get(name)
        return self.converter.module_annotations.get(name)

    # Node construction primitives

    def block(self, ctran: Ctran | None = None) -> Block:
        ctran = ctran or Ctran()
        return ctran_starts_block(ctran, self.component)

    def ref(self, start, next_, result, leaf, source, *, final=False) -> Ref:
        node = Ref(self.lexenv, source, leaf, final=final)
        leaf.refs.append(node)
        link_node_to_previous_ctran(node, start)
        use_continuation(node, next_, result)
        return node

    def constant(self, start, next_, result, literal) -> Ref:
        return self.ref(start, next_, result, Constant(literal), literal)

    def load(self, start, next_, result, name, source, *, final=False) -> Ref:
        return self.ref(start, next_, result, self.variable(name), source, final=final)

    def set_var(self, start, next_, var, value: Lvar | None, source) -> Set:
        node = Set(self.lexenv, source, var, value)
        if value is not None:
            value.dest = node
        var.sets.append(node)
        link_node_to_previous_ctran(node, start)
        use_ctran(node, next_)
        return node

    def cast(self, start, next_, result, value: Lvar, contract, node, safety=None):
        cast = Cast(
            self.lexenv,
            node,
            value,
            contract,
            self.converter.location(node),
            self.lambda_.policy.safety if safety is None else safety,
        )
        value.dest = cast
        link_node_to_previous_ctran(cast, start)
        use_continuation(cast, next_, result)
        return cast

    def known(self, start, next_, result, info, operands, source, data=None):
        """Emit a call to a known function. Operands are lvars already produced
        before ``start`` or expressions to convert in order. ``next_`` of
        ``None`` means the call never returns and ends its block."""
        fun = Lvar()
        ctran = Ctran()
        self.ref(start, ctran, fun, self.converter.known(info), source)
        node = Combination(self.lexenv, source, fun)
        node.kind = CallKind.KNOWN
        node.fun_info = info
        node.info = data
        fun.dest = node
        self._combination_args(node, ctran, next_, result, operands)
        return node

    def full_call(self, start, next_, result, callee, operands, source, data):
        """Emit a call through an evaluated callable value."""
        node = Combination(self.lexenv, source, callee)
        callee.dest = node
        node.info = data
        self._combination_args(node, start, next_, result, operands)
        return node

    def _combination_args(self, node, start, next_, result, operands):
        args = []
        for operand in operands:
            if isinstance(operand, Lvar):
                aver(operand.dest is None)
                operand.dest = node
                args.append(operand)
            else:
                ctran, lvar = Ctran(), Lvar(node)
                self.convert(start, ctran, lvar, operand)
                start = ctran
                args.append(lvar)
        node.args = args
        link_node_to_previous_ctran(node, start)
        if next_ is None:
            aver(node.fun_info is not None and node.fun_info.never_returns)
            node.block.last = node
            use_lvar(node, result)
        else:
            use_continuation(node, next_, result)

    def protocol(self, start, next_, result, name, operands, source):
        return self.known(start, next_, result, fndb.PROTOCOLS[name], operands, source)

    def if_node(self, start, test: Lvar, consequent: Block, alternative: Block, source):
        node = If(self.lexenv, source, test, consequent, alternative)
        test.dest = node
        link_node_to_previous_ctran(node, start)
        block = start.block
        aver(block.last is None, "a branch must end an open block")
        block.last = node
        link_blocks(block, consequent)
        if alternative is not consequent:
            link_blocks(block, alternative)
        return node

    def flow(self, frontier: Ctran, next_: Ctran) -> None:
        """Continue from an open frontier to ``next``: retarget the node that
        reached the frontier, or fill an empty block with a degenerate exit."""
        if frontier is next_:
            return
        aver(frontier.next is None, "the frontier already continues")
        if frontier.kind is CtranKind.INSIDE_BLOCK:
            previous = frontier.use
            previous.next = None
            frontier.use = None
            frontier.kind = CtranKind.UNUSED
            frontier.block = None
            use_ctran(previous, next_)
        else:
            aver(frontier.kind is CtranKind.BLOCK_START)
            filler = Exit(self.lexenv, None)
            link_node_to_previous_ctran(filler, frontier)
            use_ctran(filler, next_)

    def jump(self, frontier: Ctran, target: Block) -> None:
        self.flow(frontier, target.start)

    # Conversion drivers

    def convert(self, start, next_, result, expression) -> None:
        self.frames.append((start, next_, result))
        try:
            expression.accept(self.expressions)
        finally:
            self.frames.pop()

    def value(self, start, expression) -> tuple[Ctran, Lvar]:
        """Convert ``expression`` for its value; returns the continuation."""
        ctran, lvar = Ctran(), Lvar()
        self.convert(start, ctran, lvar, expression)
        return ctran, lvar

    def convert_statement(self, start, next_, statement) -> None:
        self.frames.append((start, next_, None))
        try:
            statement.accept(self.statements_visitor)
        finally:
            self.frames.pop()

    def convert_body(self, start: Ctran, statements) -> Ctran | None:
        """Convert statements in sequence from ``start``. Returns the open
        frontier, or ``None`` when the final statement transferred control.
        Code after a transfer is still converted, into an unreachable block, so
        unsupported syntax there is rejected."""
        self.depth += 1
        try:
            current = start
            terminated = False
            for statement in statements:
                if terminated:
                    current = self.block(current).start
                following = Ctran()
                self.convert_statement(current, following, statement)
                if current.kind is not CtranKind.UNUSED and current.next is None:
                    # Nothing was emitted; the next statement starts here too.
                    continue
                terminated = following.kind is CtranKind.UNUSED
                current = following
            return None if terminated else current
        finally:
            self.depth -= 1

    def leave_handlers(self, start: Ctran, stop: Cleanup | None, source) -> Ctran:
        """Emit the handler-exit actions for leaving every handler body between
        the current cleanup and ``stop``."""
        leaving = []
        cleanup = self.lexenv.cleanup
        while cleanup is not stop:
            leaving.append(cleanup)
            cleanup = cleanup.parent
        return self.leave_handler_bodies(start, leaving, source)

    def leave_handler_bodies(self, start: Ctran, cleanups, source) -> Ctran:
        for cleanup in cleanups:
            if cleanup.kind is not CleanupKind.HANDLER_BODY:
                continue
            if cleanup.binding is not None:
                following = Ctran()
                self.set_var(
                    start, following, self.variable(cleanup.binding), None, source
                )
                start = following
            following = Ctran()
            self.known(start, following, None, fndb.LEAVE_HANDLER, (), source)
            start = following
        return start

    def with_cleanup(self, cleanup: Cleanup | None):
        return _CleanupScope(self, cleanup)

    def with_loop(self, target: LoopTarget):
        return _LoopScope(self, target)

    # Expression helpers shared by several translators

    def assert_type(self, start, next_, result, value, contract, node):
        return self.cast(start, next_, result, value, contract, node)

    def store(self, start, next_, name: str, value: Lvar, node=None) -> None:
        if node is not None:
            self.converter.reserved(name, node)
        annotation = self.annotation_for(name)
        if annotation is not None:
            contract, declaration = annotation
            ctran, checked = Ctran(), Lvar()
            self.cast(start, ctran, checked, value, contract, node or declaration)
            start, value = ctran, checked
        self.set_var(start, next_, self.variable(name), value, node)

    def discard(self, start, next_, name, source):
        """Reference a temporary for the last time without using its value."""
        self.load(start, next_, None, name, source, final=True)

    def condition(self, start, node, true: Block, false: Block):
        """Lower a truth-testing context without re-testing short-circuit values."""
        node.accept(_ConditionVisitor(self, start, true, false))

    def value_condition(self, start, node, destination, true: Block, false: Block):
        """Branch on an operand while retaining its selected value in
        ``destination``. Nested and/or expressions share the surrounding edges."""
        node.accept(_ValueConditionVisitor(self, start, destination, true, false))

    def boolean(self, start, next_, result, operator, expressions, source):
        temporary = self.temporary()
        join = self.block()
        current = start
        for expression in expressions[:-1]:
            following = self.block()
            true, false = (
                (following, join)
                if operator == ast.BooleanOperator.AND
                else (join, following)
            )
            self.value_condition(current, expression, temporary, true, false)
            ctran = Ctran()
            self.discard(following.start, ctran, temporary.name, expression)
            current = ctran
        ctran, value = self.value(current, expressions[-1])
        self.set_var(ctran, join.start, temporary, value, source)
        self.ref(join.start, next_, result, temporary, source, final=True)

    def comparison_condition(self, start, node, true: Block, false: Block):
        previous = self.temporary()
        ctran, value = self.value(start, node.left)
        following = Ctran()
        self.set_var(ctran, following, previous, value, node.left)
        current = following
        for index, (operator, comparator) in enumerate(
            zip(node.operators, node.comparators)
        ):
            ctran, left = Ctran(), Lvar()
            self.ref(current, ctran, left, previous, node, final=True)
            current, right = self.value(ctran, comparator)
            last = index == len(node.operators) - 1
            if not last:
                ctran = Ctran()
                self.set_var(current, ctran, previous, right, comparator)
                current, right = Ctran(), Lvar()
                self.ref(ctran, current, right, previous, comparator)
            ctran, compared = Ctran(), Lvar()
            self.known(
                current, ctran, compared, fndb.COMPARE[operator], (left, right), node
            )
            if last:
                self.if_node(ctran, compared, true, false, node)
            else:
                next_block, failed = self.block(), self.block()
                self.if_node(ctran, compared, next_block, failed, node)
                ctran = Ctran()
                self.discard(failed.start, ctran, previous.name, node)
                self.jump(ctran, false)
                current = next_block.start

    def comparison(self, start, next_, result, node: ast.Comparison):
        if len(node.operators) == 1:
            ctran, left = self.value(start, node.left)
            ctran, right = self.value(ctran, node.comparators[0])
            self.known(
                ctran,
                next_,
                result,
                fndb.COMPARE[node.operators[0]],
                (left, right),
                node,
            )
            return
        previous, outcome = self.temporary(), self.temporary()
        join = self.block()
        ctran, value = self.value(start, node.left)
        current = Ctran()
        self.set_var(ctran, current, previous, value, node.left)
        for index, (operator, comparator) in enumerate(
            zip(node.operators, node.comparators)
        ):
            last = index == len(node.operators) - 1
            ctran, left = Ctran(), Lvar()
            self.ref(current, ctran, left, previous, node, final=True)
            current, right = self.value(ctran, comparator)
            if not last:
                ctran = Ctran()
                self.set_var(current, ctran, previous, right, comparator)
                current, right = Ctran(), Lvar()
                self.ref(ctran, current, right, previous, comparator)
            ctran, compared = Ctran(), Lvar()
            self.known(
                current, ctran, compared, fndb.COMPARE[operator], (left, right), node
            )
            if last:
                self.set_var(ctran, join.start, outcome, compared, node)
                break
            current = Ctran()
            self.set_var(ctran, current, outcome, compared, node)
            next_block = self.block()
            ctran, test = Ctran(), Lvar()
            self.ref(current, ctran, test, outcome, node)
            self.if_node(ctran, test, next_block, join, node)
            current = Ctran()
            self.discard(next_block.start, current, outcome.name, node)
        # A short-circuit can leave the middle operand in the temporary. Its
        # release is conditional because the successful final comparison moved
        # it. Assigning None releases either state without reading an unbound slot.
        ctran, none = Ctran(), Lvar()
        self.constant(join.start, ctran, none, ast.NoneLiteral(None))
        following = Ctran()
        self.set_var(ctran, following, previous, none, node)
        ctran = Ctran()
        self.discard(following, ctran, previous.name, node)
        self.ref(ctran, next_, result, outcome, node, final=True)

    def call(self, start, next_, result, node: ast.Call):
        keywords = set()
        for argument in node.arguments:
            if argument.kind not in (
                ast.ArgumentKind.POSITIONAL,
                ast.ArgumentKind.KEYWORD,
            ):
                self.error(
                    "call argument expansion is not supported yet",
                    argument,
                    unsupported=True,
                )
            if argument.kind == ast.ArgumentKind.KEYWORD:
                if argument.keyword in keywords:
                    self.error(
                        f"keyword argument repeated: {argument.keyword}", argument
                    )
                keywords.add(argument.keyword)
        node.function.accept(_CallTarget(self, start, next_, result, node))

    def import_module(self, start, name, node) -> tuple[Ctran, Lvar]:
        if self.converter.imports is None:
            self.error(
                "runtime module imports are not supported by standalone lowering",
                node,
                unsupported=True,
            )
        parent = None
        parts = name.split(".")
        current = start
        for index, part in enumerate(parts):
            qualified = ".".join(parts[: index + 1])
            descriptor = self.converter.imports.resolve(qualified, self.converter, node)
            ctran, imported = Ctran(), Lvar()
            self.known(
                current,
                ctran,
                imported,
                fndb.IMPORT,
                (),
                node,
                ImportInfo(qualified, descriptor.entry, str(descriptor.path)),
            )
            current = ctran
            if parent is not None:
                temporary = self.temporary()
                ctran = Ctran()
                self.set_var(current, ctran, temporary, imported, node)
                current, child = Ctran(), Lvar()
                self.ref(ctran, current, child, temporary, node)
                ctran = Ctran()
                self.known(
                    current,
                    ctran,
                    None,
                    fndb.SETATTR,
                    (parent, child),
                    node,
                    AttributeInfo(part, self.converter.location(node)),
                )
                current, imported = Ctran(), Lvar()
                self.ref(ctran, current, imported, temporary, node, final=True)
            parent = imported
        return current, parent

    def import_attribute(self, start, namespace: LambdaVar, item) -> Ctran:
        ctran, module = Ctran(), Lvar()
        self.ref(start, ctran, module, namespace, item)
        following, attribute = Ctran(), Lvar()
        self.known(
            ctran,
            following,
            attribute,
            fndb.GETATTR,
            (module,),
            item,
            AttributeInfo(item.name, self.converter.location(item)),
        )
        ctran = Ctran()
        self.store(following, ctran, item.alias or item.name, attribute, item)
        return ctran

    def _import_directive(self, node):
        if not self.is_module or self.depth != 1:
            self.error(
                "Mule compiler imports must be unconditional module-level statements",
                node,
                unsupported=True,
            )
        node.accept(_ImportBindings(self.converter, activate=True))


class _CleanupScope:
    def __init__(self, function, cleanup):
        self.function, self.cleanup = function, cleanup

    def __enter__(self):
        self.saved = self.function.lexenv
        self.function.lexenv = self.saved.derive(cleanup=self.cleanup)
        return self.cleanup

    def __exit__(self, *exc):
        self.function.lexenv = self.saved


class _LoopScope:
    def __init__(self, function, target):
        self.function, self.target = function, target

    def __enter__(self):
        self.saved = self.function.lexenv
        self.function.lexenv = self.saved.derive(loops=(*self.saved.loops, self.target))

    def __exit__(self, *exc):
        self.function.lexenv = self.saved


class _Expressions(ast.Visitor):
    """Expression translators: each delivers its value to the frame's result."""

    def __init__(self, function: FunctionConverter):
        self.function = function

    @property
    def frame(self):
        return self.function.frames[-1]

    def visit_Node(self, node):
        self.function.error(
            "syntax is not supported by the native compiler yet",
            node,
            unsupported=True,
        )

    def literal(self, node):
        start, next_, result = self.frame
        self.function.constant(start, next_, result, node)

    visit_IntegerLiteral = literal
    visit_FloatLiteral = literal
    visit_BooleanLiteral = literal
    visit_NoneLiteral = literal
    visit_TextLiteral = literal
    visit_BytesLiteral = literal

    def visit_Name(self, node):
        function = self.function
        if node.identifier in function.converter.compiler_bindings:
            function.error(
                f"compiler-only import '{node.identifier}' cannot be used as a runtime value",
                node,
                unsupported=True,
            )
        start, next_, result = self.frame
        function.load(start, next_, result, node.identifier, node)

    def visit_Subscript(self, node):
        function = self.function
        start, next_, result = self.frame
        ctran, value = function.value(start, node.value)
        ctran, index = function.value(ctran, node.index)
        function.protocol(ctran, next_, result, "getitem", (value, index), node)

    def visit_List(self, node):
        self.sequence(node, fndb.BUILD_LIST)

    def visit_Tuple(self, node):
        self.sequence(node, fndb.BUILD_TUPLE)

    def sequence(self, node, info):
        function = self.function
        start, next_, result = self.frame
        function.known(start, next_, result, info, tuple(node.elements), node)

    def visit_FormattedString(self, node):
        function = self.function
        start, next_, result = self.frame
        if not node.parts:
            function.constant(start, next_, result, ast.TextLiteral(""))
            return
        if len(node.parts) == 1:
            function.convert(start, next_, result, node.parts[0])
            return
        # Every part is evaluated before any concatenation, as the previous
        # lowering did; concatenation then folds left.
        parts = []
        current = start
        for part in node.parts:
            current, value = function.value(current, part)
            parts.append(value)
        joined = parts[0]
        for index, part in enumerate(parts[1:]):
            last = index == len(parts) - 2
            ctran = next_ if last else Ctran()
            value = result if last else Lvar()
            function.known(
                current,
                ctran,
                value,
                fndb.BINARY[ast.BinaryOperator.ADD],
                (joined, part),
                node,
            )
            current, joined = ctran, value

    def visit_Interpolation(self, node):
        function = self.function
        start, next_, result = self.frame
        if node.conversion not in (None, "s", "r"):
            function.error("unsupported f-string conversion", node, unsupported=True)
        ctran, value = function.value(start, node.expression)
        if node.format_spec is not None:
            ctran, specification = function.value(ctran, node.format_spec)
        else:
            following, specification = Ctran(), Lvar()
            function.constant(ctran, following, specification, ast.TextLiteral(""))
            ctran = following
        # Debug fields without an explicit format specification imply !r.
        conversion = node.conversion
        if node.expression_text is not None and node.format_spec is None:
            conversion = conversion or "r"
        function.known(
            ctran,
            next_,
            result,
            fndb.FORMAT,
            (value, specification),
            node,
            FormatInfo(conversion),
        )

    def visit_Attribute(self, node):
        function = self.function
        start, next_, result = self.frame
        ctran, receiver = function.value(start, node.value)
        function.known(
            ctran,
            next_,
            result,
            fndb.GETATTR,
            (receiver,),
            node,
            AttributeInfo(node.attribute, function.converter.location(node)),
        )

    def visit_Binary(self, node):
        function = self.function
        start, next_, result = self.frame
        ctran, left = function.value(start, node.left)
        ctran, right = function.value(ctran, node.right)
        function.known(
            ctran, next_, result, fndb.BINARY[node.operator], (left, right), node
        )

    def visit_Unary(self, node):
        function = self.function
        start, next_, result = self.frame
        ctran, operand = function.value(start, node.operand)
        function.known(
            ctran, next_, result, fndb.UNARY[node.operator], (operand,), node
        )

    def visit_Call(self, node):
        start, next_, result = self.frame
        self.function.call(start, next_, result, node)

    def visit_NamedExpression(self, node):
        function = self.function
        start, next_, result = self.frame
        temporary = function.temporary()
        ctran, value = function.value(start, node.value)
        following = Ctran()
        function.set_var(ctran, following, temporary, value, node)
        ctran, stored = Ctran(), Lvar()
        function.ref(following, ctran, stored, temporary, node)
        following = Ctran()
        function.store(ctran, following, node.target.identifier, stored, node.target)
        function.ref(following, next_, result, temporary, node, final=True)

    def visit_Boolean(self, node):
        start, next_, result = self.frame
        self.function.boolean(start, next_, result, node.operator, node.operands, node)

    def visit_Conditional(self, node):
        function = self.function
        start, next_, result = self.frame
        then, other, join = function.block(), function.block(), function.block()
        temporary = function.temporary()
        function.condition(start, node.condition, then, other)
        ctran, value = function.value(then.start, node.then)
        function.set_var(ctran, join.start, temporary, value, node.then)
        ctran, value = function.value(other.start, node.otherwise)
        function.set_var(ctran, join.start, temporary, value, node.otherwise)
        function.ref(join.start, next_, result, temporary, node, final=True)

    def visit_Comparison(self, node):
        start, next_, result = self.frame
        self.function.comparison(start, next_, result, node)


class _Statements(ast.Visitor):
    """Statement translators. Each links between the frame's start and next;
    one that transfers control elsewhere leaves next unused."""

    def __init__(self, function: FunctionConverter):
        self.function = function

    @property
    def frame(self):
        return self.function.frames[-1]

    def visit_Node(self, node):
        self.function.error(
            "syntax is not supported by the native compiler yet",
            node,
            unsupported=True,
        )

    def visit_Pass(self, node):
        return None

    def visit_Global(self, node):
        return None

    def visit_Import(self, node):
        function = self.function
        start, next_, _ = self.frame
        current = start
        for item in node.items:
            if item.name == "mule":
                function._import_directive(ast.Import([item], span=node.span))
                continue
            current, imported = function.import_module(current, item.name, node)
            if "." in item.name and item.alias is None:
                for use in imported.uses:
                    use.lvar = None
                imported.uses = []
                current, imported = function.import_module(
                    current, item.name.split(".")[0], node
                )
            ctran = Ctran()
            function.store(
                current, ctran, item.alias or item.name.split(".")[0], imported, node
            )
            current = ctran
        if current is not start:
            function.flow(current, next_)

    def visit_ImportFrom(self, node):
        function = self.function
        start, next_, _ = self.frame
        if node.level == 0 and node.module in ("mule", "__future__"):
            function._import_directive(node)
            return
        if any(item.name == "*" for item in node.items):
            function.error(
                "wildcard runtime imports are not supported yet",
                node,
                unsupported=True,
            )
        if function.converter.imports is None:
            function.error(
                "runtime module imports are not supported by standalone lowering",
                node,
                unsupported=True,
            )
        imports = function.converter.imports
        module_name = imports.import_name(function.converter, node)
        namespace = function.temporary()
        current, module = function.import_module(start, module_name, node)
        ctran = Ctran()
        function.set_var(current, ctran, namespace, module, node)
        current = ctran
        for item in node.items:
            child = module_name + "." + item.name
            if imports.path_for(child) is not None:
                exists, absent, joined = (
                    function.block(),
                    function.block(),
                    function.block(),
                )
                ctran, loaded = Ctran(), Lvar()
                function.ref(current, ctran, loaded, namespace, node)
                following, present = Ctran(), Lvar()
                function.known(
                    ctran,
                    following,
                    present,
                    fndb.HAS_BINDING,
                    (loaded,),
                    node,
                    item.name,
                )
                function.if_node(following, present, exists, absent, node)
                current, imported = function.import_module(absent.start, child, item)
                ctran = Ctran()
                function.store(current, ctran, item.alias or item.name, imported, item)
                function.jump(ctran, joined)
                ctran = function.import_attribute(exists.start, namespace, item)
                function.jump(ctran, joined)
                current = joined.start
            else:
                current = function.import_attribute(current, namespace, item)
        function.discard(current, next_, namespace.name, node)

    def visit_Assert(self, node):
        function = self.function
        start, next_, _ = self.frame
        passed, failed = function.block(next_), function.block()
        function.condition(start, node.condition, passed, failed)
        if node.message is not None:
            ctran, message = function.value(failed.start, node.message)
        else:
            ctran, message = Ctran(), Lvar()
            function.constant(failed.start, ctran, message, ast.TextLiteral(""))
        following = Ctran()
        function.known(ctran, following, None, fndb.ASSERTION_FAILURE, (message,), node)
        # The failure operation always branches to owned-reference cleanup.
        function.jump(following, passed)

    def visit_AnnotatedAssignment(self, node):
        start, next_, _ = self.frame
        node.target.accept(_AnnotationTarget(self.function, node, start, next_))

    def visit_ExpressionStatement(self, node):
        start, next_, _ = self.frame
        self.function.convert(start, next_, None, node.expression)

    def visit_Assignment(self, node):
        function = self.function
        start, next_, _ = self.frame
        ctran, value = function.value(start, node.value)
        if len(node.targets) == 1:
            node.targets[0].accept(_AssignmentTarget(function, value, ctran, next_))
            return
        temporary = function.temporary()
        current = Ctran()
        function.set_var(ctran, current, temporary, value, node)
        for index, target in enumerate(node.targets):
            last = index == len(node.targets) - 1
            ctran, loaded = Ctran(), Lvar()
            function.ref(current, ctran, loaded, temporary, node, final=last)
            current = next_ if last else Ctran()
            target.accept(_AssignmentTarget(function, loaded, ctran, current))

    def visit_AugmentedAssignment(self, node):
        start, next_, _ = self.frame
        node.target.accept(_AugmentedTarget(self.function, node, start, next_))

    def visit_If(self, node):
        function = self.function
        start, next_, _ = self.frame
        then, other, join = function.block(), function.block(), function.block(next_)
        function.condition(start, node.condition, then, other)
        frontier = function.convert_body(then.start, node.body)
        if frontier is not None:
            function.jump(frontier, join)
        frontier = function.convert_body(other.start, node.else_body)
        if frontier is not None:
            function.jump(frontier, join)

    def visit_Raise(self, node):
        function = self.function
        start, _, _ = self.frame
        if node.cause is not None:
            function.error(
                "exception chaining is not supported yet", node, unsupported=True
            )
        if node.exception is None:
            self.terminal(start, fndb.RERAISE, (), node)
            return
        ctran, value = function.value(start, node.exception)
        self.terminal(ctran, fndb.RAISE, (value,), node)

    def terminal(self, start, info, operands, node):
        """Emit a call that never returns; it ends its block with no successors."""
        self.function.known(start, None, None, info, operands, node)

    def visit_Try(self, node):
        function = self.function
        start, next_, _ = self.frame
        if node.finally_body or node.exception_groups:
            function.error(
                "finally and exception groups are not supported yet",
                node,
                unsupported=True,
            )
        outer = function.lexenv.cleanup
        dispatch, success, done = function.block(), function.block(), function.block()
        region = Cleanup(CleanupKind.HANDLER_REGION, target=dispatch, parent=outer)
        with function.with_cleanup(region):
            # A handler covers only the body: its blocks start here.
            body = function.block()
            function.jump(start, body)
            frontier = function.convert_body(body.start, node.body)
            if frontier is not None:
                function.jump(frontier, success)
        frontier = function.convert_body(success.start, node.else_body)
        if frontier is not None:
            function.jump(frontier, done)
        ctran, caught = Ctran(), Lvar()
        function.known(dispatch.start, ctran, caught, fndb.CATCH_EXCEPTION, (), node)
        exception = function.temporary()
        current = Ctran()
        function.set_var(ctran, current, exception, caught, node)
        for handler in node.handlers:
            body, unmatched = function.block(), function.block()
            if handler.exception is None:
                function.jump(current, body)
            else:
                ctran, expected = function.value(current, handler.exception)
                following, raised = Ctran(), Lvar()
                function.ref(ctran, following, raised, exception, handler)
                ctran, matches = Ctran(), Lvar()
                function.protocol(
                    following,
                    ctran,
                    matches,
                    "exception_matches",
                    (raised, expected),
                    handler,
                )
                function.if_node(ctran, matches, body, unmatched, handler)
            ctran, raised = Ctran(), Lvar()
            function.ref(body.start, ctran, raised, exception, handler)
            entered = Ctran()
            function.known(ctran, entered, None, fndb.ENTER_HANDLER, (raised,), handler)
            unwind = function.block()
            active = Cleanup(
                CleanupKind.HANDLER_BODY,
                target=unwind,
                binding=handler.binding,
                parent=outer,
            )
            with function.with_cleanup(active):
                # The handler body's own error destination starts a new block.
                inner = function.block()
                function.jump(entered, inner)
                current = inner.start
                if handler.binding is not None:
                    function.converter.reserved(handler.binding, handler)
                    ctran, raised = Ctran(), Lvar()
                    function.ref(current, ctran, raised, exception, handler)
                    current = Ctran()
                    function.store(ctran, current, handler.binding, raised, handler)
                frontier = function.convert_body(current, handler.body)
            if frontier is not None:
                # Leaving the handler happens outside its own protection.
                leaving = function.block()
                function.jump(frontier, leaving)
                ctran = function.leave_handler_bodies(leaving.start, [active], handler)
                function.jump(ctran, done)
            current = unwind.start
            if handler.binding is not None:
                ctran = Ctran()
                function.set_var(
                    current, ctran, function.variable(handler.binding), None, handler
                )
                current = ctran
            ctran = Ctran()
            function.known(current, ctran, None, fndb.LEAVE_HANDLER, (), handler)
            self.terminal(ctran, fndb.UNWIND, (), handler)
            current = unmatched.start
        # No handler matched: reinstall the original caught exception.
        ctran, value = Ctran(), Lvar()
        function.ref(current, ctran, value, exception, node, final=True)
        self.terminal(ctran, fndb.RAISE, (value,), node)
        ctran = Ctran()
        function.set_var(done.start, ctran, exception, None, node)
        function.flow(ctran, next_)

    def visit_For(self, node):
        function = self.function
        start, next_, _ = self.frame
        if node.is_async:
            function.error(
                "async iteration is not supported yet", node, unsupported=True
            )
        candidate = node.iterable.accept(_RangeCandidate())
        callee = None
        current = start
        if candidate is not None:
            callee = function.temporary()
            ctran, callable_ = function.value(current, candidate.function)
            current = Ctran()
            function.set_var(ctran, current, callee, callable_, candidate.function)
            arguments = []
            for argument in candidate.arguments:
                current, value = function.value(current, argument.value)
                arguments.append(value)
            ctran, loaded = Ctran(), Lvar()
            function.ref(current, ctran, loaded, callee, candidate)
            current, value = Ctran(), Lvar()
            function.full_call(
                ctran,
                current,
                value,
                loaded,
                tuple(arguments),
                candidate,
                CallInfo((), function.converter.location(candidate)),
            )
        else:
            current, value = function.value(current, node.iterable)
        iterator = function.temporary()
        ctran, iterated = Ctran(), Lvar()
        function.protocol(current, ctran, iterated, "iterate", (value,), node)
        current = Ctran()
        function.set_var(ctran, current, iterator, iterated, node)
        generic, range_done = function.block(), function.block()
        if callee is not None:
            state = RangeState(*_range_interval(candidate))
            range_test, range_body = function.block(), function.block()
            ctran, loaded = Ctran(), Lvar()
            function.ref(current, ctran, loaded, callee, candidate, final=True)
            following, iterator_value = Ctran(), Lvar()
            function.ref(ctran, following, iterator_value, iterator, node)
            ctran, accepted = Ctran(), Lvar()
            function.known(
                following,
                ctran,
                accepted,
                fndb.RANGE_PREPARE,
                (loaded, iterator_value),
                node,
                state,
            )
            function.if_node(ctran, accepted, range_test, generic, node)
            range_else = function.block()
            ctran, available = Ctran(), Lvar()
            function.known(
                range_test.start,
                ctran,
                available,
                fndb.RANGE_AVAILABLE,
                (),
                node,
                state,
            )
            # Break skips else; exhaustion runs it.
            function.if_node(ctran, available, range_body, range_else, node)
            ctran, item = Ctran(), Lvar()
            function.known(
                range_body.start, ctran, item, fndb.RANGE_ADVANCE, (), node, state
            )
            following = Ctran()
            node.target.accept(_AssignmentTarget(function, item, ctran, following))
            with function.with_loop(
                LoopTarget(range_test, range_done, function.lexenv.cleanup)
            ):
                frontier = function.convert_body(following, node.body)
            if frontier is not None:
                function.jump(frontier, range_test)
            frontier = function.convert_body(range_else.start, node.else_body)
            if frontier is not None:
                function.jump(frontier, range_done)
        else:
            function.jump(current, generic)
        test, loop, exhausted, done = (
            function.block(),
            function.block(),
            function.block(),
            function.block(),
        )
        failed, finished = function.block(), function.block()
        function.jump(generic.start, test)
        outer = function.lexenv.cleanup
        region = Cleanup(CleanupKind.HANDLER_REGION, target=exhausted, parent=outer)
        with function.with_cleanup(region):
            ctran, iterator_value = Ctran(), Lvar()
            function.ref(test.start, ctran, iterator_value, iterator, node)
            following, item = Ctran(), Lvar()
            function.protocol(
                ctran, following, item, "next_item", (iterator_value,), node
            )
            temporary = function.temporary()
            ctran = Ctran()
            function.set_var(following, ctran, temporary, item, node)
        function.jump(ctran, loop)
        ctran, loaded = Ctran(), Lvar()
        function.ref(loop.start, ctran, loaded, temporary, node, final=True)
        following = Ctran()
        node.target.accept(_AssignmentTarget(function, loaded, ctran, following))
        with function.with_loop(LoopTarget(test, done, function.lexenv.cleanup)):
            frontier = function.convert_body(following, node.body)
        if frontier is not None:
            function.jump(frontier, test)
        ctran, caught = Ctran(), Lvar()
        function.known(exhausted.start, ctran, caught, fndb.CATCH_EXCEPTION, (), node)
        exception = function.temporary()
        following = Ctran()
        function.set_var(ctran, following, exception, caught, node)
        ctran, raised = Ctran(), Lvar()
        function.ref(following, ctran, raised, exception, node)
        following, matches = Ctran(), Lvar()
        function.protocol(
            ctran, following, matches, "iteration_exhausted", (raised,), node
        )
        function.if_node(following, matches, finished, failed, node)
        ctran, value = Ctran(), Lvar()
        function.ref(failed.start, ctran, value, exception, node, final=True)
        self.terminal(ctran, fndb.RAISE, (value,), node)
        ctran = Ctran()
        function.discard(finished.start, ctran, exception.name, node)
        frontier = function.convert_body(ctran, node.else_body)
        if frontier is not None:
            function.jump(frontier, done)
        function.jump(done.start, range_done)
        function.discard(range_done.start, next_, iterator.name, node)

    def visit_While(self, node):
        function = self.function
        start, next_, _ = self.frame
        test, loop, other, done = (
            function.block(),
            function.block(),
            function.block(),
            function.block(next_),
        )
        function.jump(start, test)
        function.condition(test.start, node.condition, loop, other)
        with function.with_loop(LoopTarget(test, done, function.lexenv.cleanup)):
            frontier = function.convert_body(loop.start, node.body)
        if frontier is not None:
            function.jump(frontier, test)
        frontier = function.convert_body(other.start, node.else_body)
        if frontier is not None:
            function.jump(frontier, done)

    def visit_Break(self, node):
        function = self.function
        start, _, _ = self.frame
        if not function.lexenv.loops:
            function.error("'break' outside loop", node)
        target = function.lexenv.loops[-1]
        ctran = function.leave_handlers(start, target.cleanup, node)
        function.jump(ctran, target.break_block)

    def visit_Continue(self, node):
        function = self.function
        start, _, _ = self.frame
        if not function.lexenv.loops:
            function.error("'continue' outside loop", node)
        target = function.lexenv.loops[-1]
        ctran = function.leave_handlers(start, target.cleanup, node)
        function.jump(ctran, target.continue_block)

    def visit_Return(self, node):
        function = self.function
        start, _, _ = self.frame
        if function.is_module:
            function.error("'return' outside function", node)
        lambda_ = function.lambda_
        result = lambda_.return_.result
        annotation = lambda_.signature.return_annotation
        ctran = Ctran()
        delivered = Lvar() if annotation is not None else result
        if node.value is None:
            function.constant(start, ctran, delivered, ast.NoneLiteral(None))
        else:
            function.convert(start, ctran, delivered, node.value)
        if annotation is not None:
            following = Ctran()
            function.cast(ctran, following, result, delivered, annotation, node)
            ctran = following
        ctran = function.leave_handlers(ctran, None, node)
        function.jump(ctran, lambda_.return_.block)

    def visit_FunctionDefinition(self, node):
        function = self.function
        start, next_, _ = self.frame
        function.converter.reserved(node.name, node)
        lambda_ = function.converter.function(node, function.lambda_.policy)
        current = start
        defaults = []
        for parameter in node.parameters:
            if parameter.default is not None:
                current, value = function.value(current, parameter.default)
                defaults.append(value)
        ctran, created = function.make_function(
            current,
            lambda_,
            node.name,
            defaults,
            tuple(
                lambda_.signature.annotations[index]
                for index in lambda_.signature.default_parameters
            ),
            node,
        )
        function.store(ctran, next_, node.name, created, node)

    def visit_ClassDefinition(self, node):
        function = self.function
        start, next_, _ = self.frame
        converter = function.converter
        converter.reserved(node.name, node)
        if node.type_parameters:
            function.error(
                "generic classes are not supported yet", node, unsupported=True
            )
        policy = converter.policy(node.decorators, function.lambda_.policy)
        if len(node.arguments) > 1 or any(
            argument.kind != ast.ArgumentKind.POSITIONAL for argument in node.arguments
        ):
            function.error(
                "classes support at most one positional base", node, unsupported=True
            )
        body = _ClassBody(function)
        for statement in node.body:
            statement.accept(body)
        class_id = f"{converter.prefix}mule_cls_{converter.class_definitions}"
        converter.class_definitions += 1
        native = (
            bool(body.fields)
            and body.native_eligible
            and policy.speed == 3
            and any(
                decorator.accept(_NativeClassOptIn()) for decorator in node.decorators
            )
        )
        if native:
            if node.arguments:
                function.error(
                    "typed native classes cannot have bases", node, unsupported=True
                )
            if not any(method.name == "__init__" for method in body.methods):
                function.error(
                    "typed native classes require __init__", node, unsupported=True
                )
            converter.class_fields[class_id] = body.fields
        # Base expressions are evaluated before any method definitions, as in Python.
        current = start
        operands = []
        if node.arguments:
            current, base = function.value(current, node.arguments[0].value)
            operands.append(base)
        fields = dict.fromkeys(body.fields if native else ())
        methods = {}
        for method in body.methods:
            if not method.parameters:
                # Decorator rejection precedes the ordinary receiver-arity error.
                converter.policy(method.decorators, policy)
                function.error("instance methods require a receiver parameter", method)
            if native:
                body.validate_method(method)
            lambda_ = converter.function(
                method, policy, owner_class=class_id if native else None
            )
            receiver = method.parameters[0].name
            collector = _ReceiverFields(
                receiver, fields, declaration=body if native else None
            )
            for statement in method.body:
                statement.accept(collector)
            defaults = []
            for parameter in method.parameters:
                if parameter.default is not None:
                    current, value = function.value(current, parameter.default)
                    defaults.append(value)
            current, created = function.make_function(
                current,
                lambda_,
                method.name,
                defaults,
                tuple(
                    lambda_.signature.annotations[index]
                    for index in lambda_.signature.default_parameters
                ),
                method,
            )
            # Repeated definitions preserve Python's last-definition-wins behavior.
            previous = methods.get(method.name)
            if previous is not None:
                for use in previous.uses:
                    use.lvar = None
                previous.uses = []
            methods[method.name] = created
        info = ClassInfo(
            node.name,
            bool(node.arguments),
            tuple(fields),
            tuple(methods),
            tuple(contract for contract, declaration in body.fields.values())
            if native
            else (),
            policy,
            converter.location(node),
            class_id if native else None,
            tuple(body.annotation_expressions.items()),
        )
        ctran, created = Ctran(), Lvar()
        function.known(
            current,
            ctran,
            created,
            fndb.MAKE_CLASS,
            (*operands, *methods.values()),
            node,
            info,
        )
        function.store(ctran, next_, node.name, created, node)


def _make_function(
    self, start, lambda_, public_name, defaults, default_contracts, node
):
    """Reference the converted lambda and create its runtime function object."""
    enclose = Enclose(self.lexenv, node, [lambda_])
    link_node_to_previous_ctran(enclose, start)
    ctran = Ctran()
    use_ctran(enclose, ctran)
    following, fun = Ctran(), Lvar()
    self.ref(ctran, following, fun, lambda_, node)
    ctran, created = Ctran(), Lvar()
    self.known(
        following,
        ctran,
        created,
        fndb.MAKE_FUNCTION,
        (fun, *defaults),
        node,
        FunctionInfo(public_name, default_contracts, self.converter.location(node)),
    )
    return ctran, created


FunctionConverter.make_function = _make_function


class _RangeName(ast.Visitor):
    def visit_Name(self, node):
        return node.identifier == "range"


class _RangeCandidate(ast.Visitor):
    def visit_Call(self, node):
        if (
            node.function.accept(_RangeName())
            and 1 <= len(node.arguments) <= 3
            and all(
                argument.kind == ast.ArgumentKind.POSITIONAL
                for argument in node.arguments
            )
        ):
            return node


class _IntegerConstant(ast.Visitor):
    def visit_IntegerLiteral(self, node):
        return node.value

    def visit_Unary(self, node):
        value = node.operand.accept(self)
        if value is not None:
            if node.operator == ast.UnaryOperator.NEGATIVE:
                return -value
            if node.operator == ast.UnaryOperator.POSITIVE:
                return value


def _range_interval(candidate):
    from .ctype import FIXNUM_MAX, FIXNUM_MIN

    values = [
        argument.value.accept(_IntegerConstant()) for argument in candidate.arguments
    ]
    if all(value is not None for value in values):
        if len(values) == 1:
            start, stop, step = 0, values[0], 1
        elif len(values) == 2:
            start, stop, step = values[0], values[1], 1
        else:
            start, stop, step = values
        if all(FIXNUM_MIN <= value <= FIXNUM_MAX for value in (start, stop, step)):
            if step > 0 and start < stop:
                return start, stop - 1
            if step < 0 and start > stop:
                return stop + 1, start
    return FIXNUM_MIN, FIXNUM_MAX


class _AssignmentTarget(ast.Visitor):
    def __init__(self, function, value: Lvar, start, next_):
        self.function = function
        self.value = value
        self.start = start
        self.next = next_

    def visit_Node(self, node):
        self.function.error(
            "only identifier and attribute assignment targets are supported yet",
            node,
            unsupported=True,
        )

    def visit_Subscript(self, node):
        function = self.function
        ctran, receiver = function.value(self.start, node.value)
        ctran, index = function.value(ctran, node.index)
        function.protocol(
            ctran, self.next, None, "setitem", (receiver, index, self.value), node
        )

    def visit_Tuple(self, node):
        self.unpack(node)

    def visit_List(self, node):
        self.unpack(node)

    def unpack(self, node):
        function = self.function
        ctran, count = Ctran(), Lvar()
        function.constant(
            self.start, ctran, count, ast.IntegerLiteral(len(node.elements))
        )
        following, values = Ctran(), Lvar()
        function.protocol(ctran, following, values, "unpack", (self.value, count), node)
        temporary = function.temporary()
        current = Ctran()
        function.set_var(following, current, temporary, values, node)
        for index, target in enumerate(node.elements):
            ctran, loaded = Ctran(), Lvar()
            function.ref(current, ctran, loaded, temporary, node)
            following, position = Ctran(), Lvar()
            function.constant(ctran, following, position, ast.IntegerLiteral(index))
            ctran, element = Ctran(), Lvar()
            function.protocol(
                following, ctran, element, "getitem", (loaded, position), node
            )
            current = Ctran()
            target.accept(_AssignmentTarget(function, element, ctran, current))
        function.discard(current, self.next, temporary.name, node)

    def visit_Name(self, node):
        self.function.store(self.start, self.next, node.identifier, self.value, node)

    def visit_Attribute(self, node):
        function = self.function
        ctran, receiver = function.value(self.start, node.value)
        value = self.value
        owner = function.lambda_.owner_class
        if owner is not None and node.value.accept(
            _ReceiverName(function.lambda_.parameters[0])
        ):
            declaration = function.converter.class_fields[owner].get(node.attribute)
            if declaration is not None:
                following, checked = Ctran(), Lvar()
                function.cast(ctran, following, checked, value, declaration[0], node)
                ctran, value = following, checked
        function.known(
            ctran,
            self.next,
            None,
            fndb.SETATTR,
            (receiver, value),
            node,
            AttributeInfo(node.attribute, function.converter.location(node)),
        )


class _AnnotationTarget(ast.Visitor):
    def __init__(self, function, assignment, start, next_):
        self.function = function
        self.assignment = assignment
        self.start = start
        self.next = next_

    def visit_Node(self, node):
        if self.assignment.value is not None:
            ctran, value = self.function.value(self.start, self.assignment.value)
            node.accept(_AssignmentTarget(self.function, value, ctran, self.next))

    def visit_Attribute(self, node):
        if self.assignment.value is not None:
            self.visit_Node(node)
        else:
            self.function.convert(self.start, self.next, None, node.value)

    def visit_Subscript(self, node):
        if self.assignment.value is not None:
            self.visit_Node(node)
        else:
            ctran = Ctran()
            self.function.convert(self.start, ctran, None, node.value)
            node.index.accept(_AnnotationIndex(self.function, ctran, self.next))

    def visit_Name(self, node):
        function = self.function
        converter = function.converter
        converter.reserved(node.identifier, node)
        function.lambda_.annotation_expressions[node.identifier] = (
            self.assignment.annotation
        )
        contract = converter.annotation(self.assignment.annotation, function)
        annotations = (
            function.annotations
            if function.local(node.identifier)
            else converter.module_annotations
        )
        if contract is not None:
            previous = annotations.get(node.identifier)
            if previous is not None and previous[0] is not contract:
                function.error(
                    f"conflicting annotation for '{node.identifier}'", self.assignment
                )
            annotations[node.identifier] = (contract, self.assignment)
            if function.local(node.identifier):
                function.lambda_.local_annotations[node.identifier] = contract
        if self.assignment.value is not None:
            ctran, value = function.value(self.start, self.assignment.value)
            function.store(ctran, self.next, node.identifier, value, node)


class _AnnotationIndex(ast.Visitor):
    def __init__(self, function, start, next_):
        self.function = function
        self.start = start
        self.next = next_

    def visit_Node(self, node):
        self.function.convert(self.start, self.next, None, node)

    def visit_Slice(self, node):
        current = self.start
        bounds = list(node.children())
        if not bounds:
            self.function.flow(current, self.next)
            return
        for index, bound in enumerate(bounds):
            last = index == len(bounds) - 1
            ctran = self.next if last else Ctran()
            self.function.convert(current, ctran, None, bound)
            current = ctran


class _CompilerBinding:
    def annotation(self, converter, node):
        return None

    def directive(self, converter, node):
        converter.error(
            "only imported mule.optimize decorators are supported",
            node,
            unsupported=True,
        )

    def member(self, attribute, converter, node):
        return _OPAQUE_BINDING


class _NumericBinding(_CompilerBinding):
    def __init__(self, contract):
        self.contract = contract

    def annotation(self, converter, node):
        return self.contract


class _OptimizeBinding(_CompilerBinding):
    def directive(self, converter, node):
        return None


class _ModuleBinding(_CompilerBinding):
    def member(self, attribute, converter, node):
        binding = _EXPORT_BINDINGS.get(attribute)
        if binding is None:
            return _OPAQUE_BINDING
        return binding


_EXPORT_BINDINGS = {
    name: _NumericBinding(contract) for name, contract in EXPORTS.items()
}
_EXPORT_BINDINGS["optimize"] = _OptimizeBinding()
_MODULE_BINDING = _ModuleBinding()
_OPAQUE_BINDING = _CompilerBinding()
_BUILTIN_BINDINGS = {
    name: _NumericBinding(contract) for name, contract in BUILTINS.items()
}


class _ImportBindings(ast.Visitor):
    def __init__(self, converter, *, activate=False):
        self.converter = converter
        self.activate = activate

    def bind(self, name, binding, node):
        previous = self.converter.compiler_bindings.get(name)
        if previous is not None and previous is not binding:
            self.converter.error(f"conflicting compiler import '{name}'", node)
        self.converter.compiler_bindings[name] = binding
        if self.activate:
            self.converter.active_compiler_bindings[name] = binding

    def visit_Import(self, node):
        for item in node.items:
            if item.name != "mule":
                continue
            self.bind(item.alias or item.name, _MODULE_BINDING, item)

    def visit_ImportFrom(self, node):
        if (
            node.level == 0
            and node.module == "__future__"
            and all(
                item.name == "annotations" and item.alias is None for item in node.items
            )
        ):
            return
        if node.module == "__future__" and node.level == 0:
            self.converter.error(
                "unsupported __future__ compiler directive", node, unsupported=True
            )
        if node.module != "mule" or node.level:
            return
        for item in node.items:
            binding = _EXPORT_BINDINGS.get(item.name)
            if binding is None:
                self.converter.error(
                    f"unknown Mule compiler export '{item.name}'",
                    item,
                    unsupported=True,
                )
            self.bind(item.alias or item.name, binding, item)


class _AnnotationReference(ast.Visitor):
    def __init__(self, converter, scope=None):
        self.converter = converter
        self.scope = scope

    def visit_Node(self, node):
        return _OPAQUE_BINDING

    def visit_Name(self, node):
        name = node.identifier
        binding = self.converter.active_compiler_bindings.get(name)
        if binding is not None:
            return binding
        if name in self.converter.module_annotation_shadows or (
            self.scope is not None and self.scope.local(name)
        ):
            return _OPAQUE_BINDING
        return _BUILTIN_BINDINGS.get(name, _OPAQUE_BINDING)

    def visit_Attribute(self, node):
        return node.value.accept(self).member(node.attribute, self.converter, node)


class _PolicyDecorator(ast.Visitor):
    def __init__(self, converter, policy):
        self.converter = converter
        self.policy = policy

    def visit_Node(self, node):
        self.converter.error(
            "only imported mule.optimize decorators are supported",
            node,
            unsupported=True,
        )

    def visit_Call(self, node):
        node.function.accept(_AnnotationReference(self.converter)).directive(
            self.converter, node.function
        )
        changes = {}
        for argument in node.arguments:
            if argument.kind != ast.ArgumentKind.KEYWORD:
                self.converter.error(
                    "optimize accepts only named policy levels", argument
                )
            if argument.keyword not in ("speed", "safety", "debug"):
                self.converter.error(
                    f"unknown optimization policy '{argument.keyword}'", argument
                )
            if argument.keyword in changes:
                self.converter.error(
                    f"duplicate optimization policy '{argument.keyword}'", argument
                )
            changes[argument.keyword] = argument.value.accept(
                _PolicyLevel(self.converter)
            )
        return replace(self.policy, **changes)


class _PolicyLevel(ast.Visitor):
    def __init__(self, converter):
        self.converter = converter

    def visit_Node(self, node):
        self.converter.error(
            "optimization levels must be integer literals from 0 through 3", node
        )

    def visit_IntegerLiteral(self, node):
        if not 0 <= node.value <= 3:
            self.visit_Node(node)
        return node.value


class _NativeClassOptIn(ast.Visitor):
    def visit_Call(self, node):
        return any(
            argument.kind == ast.ArgumentKind.KEYWORD
            and argument.keyword == "speed"
            and argument.value.accept(_NativeClassSpeed())
            for argument in node.arguments
        )


class _NativeClassSpeed(ast.Visitor):
    def visit_IntegerLiteral(self, node):
        return node.value == 3


class _AugmentedTarget(ast.Visitor):
    def __init__(self, function, assignment, start, next_):
        self.function = function
        self.assignment = assignment
        self.start = start
        self.next = next_

    def visit_Node(self, node):
        self.function.error(
            "only identifier and attribute assignment targets are supported yet",
            node,
            unsupported=True,
        )

    def binary(self, start, left: Lvar, reuse_local=None) -> tuple[Ctran, Lvar]:
        function = self.function
        ctran, right = function.value(start, self.assignment.value)
        following, result = Ctran(), Lvar()
        function.known(
            ctran,
            following,
            result,
            fndb.INPLACE_BINARY[self.assignment.operator],
            (left, right),
            self.assignment,
            InplaceInfo(reuse_local),
        )
        return following, result

    def visit_Subscript(self, node):
        function = self.function
        receiver, index = function.temporary(), function.temporary()
        ctran, value = function.value(self.start, node.value)
        following = Ctran()
        function.set_var(ctran, following, receiver, value, node.value)
        ctran, value = function.value(following, node.index)
        following = Ctran()
        function.set_var(ctran, following, index, value, node.index)
        ctran, loaded = Ctran(), Lvar()
        function.ref(following, ctran, loaded, receiver, node)
        following, position = Ctran(), Lvar()
        function.ref(ctran, following, position, index, node)
        ctran, left = Ctran(), Lvar()
        function.protocol(following, ctran, left, "getitem", (loaded, position), node)
        ctran, result = self.binary(ctran, left)
        following, loaded = Ctran(), Lvar()
        function.ref(ctran, following, loaded, receiver, node, final=True)
        ctran, position = Ctran(), Lvar()
        function.ref(following, ctran, position, index, node, final=True)
        function.protocol(
            ctran, self.next, None, "setitem", (loaded, position, result), node
        )

    def visit_Name(self, node):
        function = self.function
        ctran, left = Ctran(), Lvar()
        function.load(self.start, ctran, left, node.identifier, node)
        reuse = (
            node.identifier
            if function.local(node.identifier)
            and function.annotation_for(node.identifier) is None
            else None
        )
        ctran, result = self.binary(ctran, left, reuse)
        function.store(ctran, self.next, node.identifier, result, node)

    def visit_Attribute(self, node):
        function = self.function
        temporary = function.temporary()
        ctran, value = function.value(self.start, node.value)
        following = Ctran()
        function.set_var(ctran, following, temporary, value, node.value)
        ctran, receiver = Ctran(), Lvar()
        function.ref(following, ctran, receiver, temporary, node)
        following, left = Ctran(), Lvar()
        function.known(
            ctran,
            following,
            left,
            fndb.GETATTR,
            (receiver,),
            node,
            AttributeInfo(node.attribute, function.converter.location(node)),
        )
        ctran, result = self.binary(following, left)
        owner = function.lambda_.owner_class
        if owner is not None and node.value.accept(
            _ReceiverName(function.lambda_.parameters[0])
        ):
            declaration = function.converter.class_fields[owner].get(node.attribute)
            if declaration is not None:
                following, checked = Ctran(), Lvar()
                function.cast(ctran, following, checked, result, declaration[0], node)
                ctran, result = following, checked
        following, receiver = Ctran(), Lvar()
        function.ref(ctran, following, receiver, temporary, node, final=True)
        function.known(
            following,
            self.next,
            None,
            fndb.SETATTR,
            (receiver, result),
            node,
            AttributeInfo(node.attribute, function.converter.location(node)),
        )


class _CallTarget(ast.Visitor):
    def __init__(self, function, start, next_, result, call: ast.Call):
        self.function = function
        self.start = start
        self.next = next_
        self.result = result
        self.call = call

    def keywords(self):
        names = tuple(argument.keyword for argument in self.call.arguments)
        return names if any(name is not None for name in names) else ()

    def arguments(self):
        return tuple(argument.value for argument in self.call.arguments)

    def visit_Node(self, node):
        function = self.function
        ctran, callee = function.value(self.start, node)
        function.full_call(
            ctran,
            self.next,
            self.result,
            callee,
            self.arguments(),
            node,
            CallInfo(self.keywords(), function.converter.location(node)),
        )

    def visit_Attribute(self, node):
        function = self.function
        ctran, receiver = function.value(self.start, node.value)
        following, resolved = Ctran(), Lvar()
        function.known(
            ctran,
            following,
            resolved,
            fndb.RESOLVE_METHOD,
            (receiver,),
            node,
            AttributeInfo(node.attribute, function.converter.location(node)),
        )
        function.known(
            following,
            self.next,
            self.result,
            fndb.CALL_RESOLVED,
            (resolved, *self.arguments()),
            node,
            CallInfo(self.keywords(), None),
        )


class _ClassBody(ast.Visitor):
    def __init__(self, function):
        self.function = function
        self.methods = []
        self.fields = {}
        self.annotation_expressions = {}
        self.native_eligible = True

    def visit_Node(self, node):
        self.function.error(
            "class bodies support only instance methods, bare typed native fields, and pass",
            node,
            unsupported=True,
        )

    def visit_FunctionDefinition(self, node):
        self.methods.append(node)

    def visit_Pass(self, node):
        pass

    def visit_AnnotatedAssignment(self, node):
        node.target.accept(_ClassFieldDeclaration(self, node))

    def validate_method(self, method):
        if method.name in self.fields:
            self.function.error("typed native fields cannot share method names", method)
        if method.name in (
            "__getattr__",
            "__getattribute__",
            "__setattr__",
            "__delattr__",
            "__new__",
            "__del__",
        ):
            self.function.error(
                "dynamic object hooks are not supported on typed native classes",
                method,
                unsupported=True,
            )


class _ClassFieldDeclaration(ast.Visitor):
    def __init__(self, body, assignment):
        self.body = body
        self.assignment = assignment

    def visit_Node(self, node):
        self.body.function.error(
            "typed native field declarations require bare identifiers",
            node,
            unsupported=True,
        )

    def visit_Name(self, node):
        if self.assignment.value is not None:
            self.body.function.error(
                "typed native field declarations cannot have default values",
                self.assignment,
                unsupported=True,
            )
        if node.identifier in ("__dict__", "__slots__", "__class__"):
            self.body.function.error(
                "reserved object attributes cannot be typed native fields", node
            )
        self.body.annotation_expressions[node.identifier] = self.assignment.annotation
        contract = self.body.function.converter.annotation(self.assignment.annotation)
        if contract is not EXPORTS["int64"]:
            self.body.native_eligible = False
            return
        self.body.fields[node.identifier] = (contract, self.assignment)


class _ReceiverFields(ast.Visitor):
    def __init__(self, receiver, fields, *, declaration=None):
        self.targets = _ReceiverFieldTarget(receiver, fields, declaration)

    def visit_Node(self, node):
        for target in node.assigned_targets():
            target.accept(self.targets)
        for child in node.children():
            child.accept(self)

    def visit_FunctionDefinition(self, node):
        pass

    def visit_ClassDefinition(self, node):
        pass

    def visit_AnnotatedAssignment(self, node):
        if node.value is not None:
            self.visit_Node(node)


class _ReceiverFieldTarget(ast.Visitor):
    def __init__(self, receiver, fields, declaration):
        self.receiver = _ReceiverName(receiver)
        self.fields = fields
        self.declaration = declaration

    def visit_Attribute(self, node):
        if node.value.accept(self.receiver):
            if self.declaration is not None and node.attribute not in self.fields:
                self.declaration.function.error(
                    f"undeclared typed native field '{node.attribute}'", node
                )
            self.fields[node.attribute] = None


class _ReceiverName(ast.Visitor):
    def __init__(self, name):
        self.name = name

    def visit_Name(self, node):
        return node.identifier == self.name


class _ImportNames(ast.Visitor):
    def visit_Import(self, node):
        for item in node.items:
            self.imported_name(item.alias or item.name.split(".")[0], item)

    def visit_ImportFrom(self, node):
        for item in node.items:
            self.imported_name(item.alias or item.name, item)


class _AnnotationShadows(_ImportNames):
    """Runtime bindings prevent treating the same name as a builtin contract."""

    def __init__(self):
        self.names = set()

    def imported_name(self, name, node):
        self.names.add(name)

    def visit_Node(self, node):
        for target in node.assigned_targets():
            target.accept(_AnnotationShadowTarget(self.names))
        for child in node.children():
            child.accept(self)

    def visit_FunctionDefinition(self, node):
        self.names.add(node.name)

    def visit_ClassDefinition(self, node):
        self.names.add(node.name)

    def visit_AnnotatedAssignment(self, node):
        node.target.accept(_AnnotationShadowTarget(self.names))
        if node.value is not None:
            node.value.accept(self)


class _AnnotationShadowTarget(ast.Visitor):
    def __init__(self, names):
        self.names = names

    def visit_Name(self, node):
        self.names.add(node.identifier)


class _ScopeVisitor(_ImportNames):
    def __init__(self, function):
        self.function = function
        self.seen = set(function.lambda_.parameters)

    def imported_name(self, name, node):
        self.seen.add(name)

    def visit_Node(self, node):
        for child in node.children():
            child.accept(self)

    def visit_Name(self, node):
        self.seen.add(node.identifier)

    def visit_AnnotatedAssignment(self, node):
        node.target.accept(self)
        if node.value is not None:
            node.value.accept(self)

    def visit_FunctionDefinition(self, node):
        if not self.function.is_module:
            self.function.error(
                "nested functions are not supported yet", node, unsupported=True
            )
        self.seen.add(node.name)

    def visit_ClassDefinition(self, node):
        if not self.function.is_module:
            self.function.error(
                "nested classes are not supported yet", node, unsupported=True
            )
        for argument in node.arguments:
            argument.accept(self)
        self.seen.add(node.name)

    def visit_Global(self, node):
        for name in node.identifiers:
            if name not in self.function.globals and name in self.seen:
                self.function.error(
                    f"name '{name}' is used or assigned before global declaration", node
                )
            self.function.globals.add(name)

    def visit_Nonlocal(self, node):
        self.function.error(
            "nonlocal bindings are not supported yet", node, unsupported=True
        )


class _LocalBindings(_ImportNames):
    def __init__(self, function):
        self.function = function
        self.target_visitor = _LocalTarget(function)

    def imported_name(self, name, node):
        ast.Name(name, span=node.span).accept(self.target_visitor)

    def visit_Node(self, node):
        for target in node.assigned_targets():
            target.accept(self.target_visitor)
        for child in node.children():
            child.accept(self)

    def visit_FunctionDefinition(self, node):
        pass

    def visit_ClassDefinition(self, node):
        pass

    def visit_ExceptionHandler(self, node):
        if node.binding is not None:
            ast.Name(node.binding, span=node.span).accept(self.target_visitor)
        self.visit_Node(node)

    def visit_AnnotatedAssignment(self, node):
        node.target.accept(self.target_visitor)
        node.target.accept(self)
        if node.value is not None:
            node.value.accept(self)


class _LocalTarget(ast.Visitor):
    def __init__(self, function):
        self.function = function

    def visit_Node(self, node):
        for child in node.children():
            child.accept(self)

    def visit_Subscript(self, node):
        pass

    def visit_Attribute(self, node):
        pass

    def visit_Name(self, node):
        self.function.converter.reserved(node.identifier, node)
        if node.identifier not in self.function.globals:
            self.function.declare_local(node.identifier)


class _ConditionVisitor(ast.Visitor):
    def __init__(self, function, start, true: Block, false: Block):
        self.function = function
        self.start = start
        self.true, self.false = true, false

    def visit_Node(self, node):
        ctran, value = self.function.value(self.start, node)
        self.function.if_node(ctran, value, self.true, self.false, node)

    def visit_Boolean(self, node):
        function = self.function
        current = self.start
        for operand in node.operands[:-1]:
            next_block = function.block()
            yes, no = (
                (next_block, self.false)
                if node.operator == ast.BooleanOperator.AND
                else (self.true, next_block)
            )
            function.condition(current, operand, yes, no)
            current = next_block.start
        function.condition(current, node.operands[-1], self.true, self.false)

    def visit_Unary(self, node):
        if node.operator == ast.UnaryOperator.NOT:
            self.function.condition(self.start, node.operand, self.false, self.true)
        else:
            self.visit_Node(node)

    def visit_Conditional(self, node):
        function = self.function
        yes_block, no_block = function.block(), function.block()
        function.condition(self.start, node.condition, yes_block, no_block)
        function.condition(yes_block.start, node.then, self.true, self.false)
        function.condition(no_block.start, node.otherwise, self.true, self.false)

    def visit_Comparison(self, node):
        if len(node.operators) > 1:
            self.function.comparison_condition(self.start, node, self.true, self.false)
        else:
            self.visit_Node(node)


class _ValueConditionVisitor(ast.Visitor):
    def __init__(self, function, start, destination, true: Block, false: Block):
        self.function = function
        self.start = start
        self.destination = destination
        self.true, self.false = true, false

    def visit_Node(self, node):
        function = self.function
        ctran, value = function.value(self.start, node)
        following = Ctran()
        function.set_var(ctran, following, self.destination, value, node)
        ctran, test = Ctran(), Lvar()
        function.ref(following, ctran, test, self.destination, node)
        function.if_node(ctran, test, self.true, self.false, node)

    def visit_Boolean(self, node):
        function = self.function
        current = self.start
        for operand in node.operands[:-1]:
            next_block = function.block()
            yes, no = (
                (next_block, self.false)
                if node.operator == ast.BooleanOperator.AND
                else (self.true, next_block)
            )
            function.value_condition(current, operand, self.destination, yes, no)
            ctran = Ctran()
            function.discard(next_block.start, ctran, self.destination.name, operand)
            current = ctran
        function.value_condition(
            current, node.operands[-1], self.destination, self.true, self.false
        )


class _GlobalAnnotationStores:
    """Apply module contracts to writers converted before their declarations."""

    def __init__(self, converter, lambda_):
        self.converter = converter
        self.lambda_ = lambda_

    def apply(self, blocks):
        for block in blocks:
            for node in list(block.nodes()):
                if not isinstance(node, Set) or not isinstance(node.var, GlobalVar):
                    continue
                if node.value is None:
                    continue
                annotation = self.converter.module_annotations.get(node.var.name)
                if annotation is None:
                    continue
                contract, declaration = annotation
                producer = node.value.uses[0] if len(node.value.uses) == 1 else None
                if isinstance(producer, Cast) and producer.contract is contract:
                    continue
                location = (
                    self.converter.location(node.source)
                    if node.source is not None
                    else self.converter.location(declaration)
                )
                insert_cast_before(
                    node,
                    node.value,
                    lambda value, node=node, contract=contract, location=location: Cast(
                        node.lexenv,
                        node.source,
                        value,
                        contract,
                        location,
                        self.lambda_.policy.safety,
                    ),
                )


__all__ = [
    "AttributeInfo",
    "CallInfo",
    "ClassInfo",
    "Converter",
    "FormatInfo",
    "FunctionConverter",
    "FunctionInfo",
    "ImportInfo",
    "InplaceInfo",
    "Namespace",
    "RangeState",
    "UnsupportedSyntax",
]
