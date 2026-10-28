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

import struct
from dataclasses import dataclass, replace
from pathlib import Path
from typing import NoReturn

from mule import ast

from . import fndb
from .annotation_types import (
    BUILTINS,
    DECLARATIONS,
    EXPORTS,
    ClassContract,
    DictContract,
    ListContract,
    NoneContract,
    SetContract,
    TupleContract,
    union_contract,
)
from .ir1util import (
    aver,
    change_block_successor,
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
    Component,
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
    LeafVisitor,
    Lexenv,
    LoopTarget,
    Lvar,
    NodeVisitor,
    Ref,
    Set,
    linked,
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
    docstring: ast.Expression | None = None


@dataclass(frozen=True, slots=True)
class ClassInfo:
    public_name: str
    base_count: int
    fields: tuple[str, ...]
    field_annotations: tuple
    policy: OptimizationPolicy
    location: tuple | None
    # The definition's identity, shared with the stores into its namespace.
    identity: str
    # The identity again when the class has a typed native layout, else None.
    class_id: str | None
    annotation_expressions: tuple
    # Class keywords (a metaclass among them) may change what calling the
    # class constructs.
    has_keywords: bool = False


@dataclass(frozen=True, slots=True)
class ClassBindingInfo:
    """A name in a class body's namespace (``%class_load``/``%class_store``)."""

    name: str
    identity: str
    location: tuple | None
    # A handler-exit unbinding tolerates a body that already deleted the name.
    missing_ok: bool = False


@dataclass(slots=True, eq=False)
class ComprehensionScope:
    """An inlined comprehension's hidden variables, by source name."""

    variables: dict[str, LambdaVar]
    # Class bodies already being converted when the comprehension started;
    # the comprehension's names skip the innermost of them.
    class_depth: int


@dataclass(slots=True, eq=False)
class ClassScope:
    """A class body being converted inline in its enclosing function.

    Python executes the body with its own namespace: names it binds are read
    from that namespace first and then from the module globals and builtins,
    never from enclosing function scopes; other names resolve as they would
    in the enclosing function. Functions defined in the body skip the class
    scope entirely, and see the class itself only through ``__class__``.
    """

    owner: FunctionConverter
    namespace: LambdaVar
    identity: str
    bound: set[str]
    globals: set[str]
    # Inherited by the methods; ``optimize`` on the class sets it.
    policy: OptimizationPolicy
    # The identity when the class has a typed native layout, else None.
    native_id: str | None
    qualname: str
    # The ``__class__`` cell, created when a method needs it.
    cell: LambdaVar | None = None
    annotation_namespace_used: bool = False

    def class_cell(self) -> LambdaVar:
        if self.cell is None:
            self.cell = self.owner.temporary()
        return self.cell


@dataclass(frozen=True, slots=True)
class ImportInfo:
    module_name: str
    initializer: str
    filename: str | None
    native: bool = False
    missing: bool = False
    namespace_paths: tuple[str, ...] = ()

    @property
    def package_paths(self):
        if self.namespace_paths:
            return self.namespace_paths
        if self.package_directory is not None:
            return (self.package_directory,)
        return ()

    @property
    def package_name(self):
        if self.package_paths:
            return self.module_name
        return self.module_name.rpartition(".")[0]

    @property
    def package_directory(self):
        if self.filename is not None and Path(self.filename).name == "__init__.py":
            return str(Path(self.filename).parent)
        return None


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
    arguments: tuple = ()
    direct: bool = False
    step_sign: int = 0
    fused: bool = False
    callee_leaf: object = None

    def clone(self, map_lvar):
        return RangeState(
            self.minimum,
            self.maximum,
            tuple(map_lvar(arg) for arg in self.arguments),
            self.direct,
            self.step_sign,
            self.fused,
            self.callee_leaf,
        )


class _ConstantKey(ast.Visitor):
    """Canonical similarity key for IR1 constant coalescing (SBCL's
    ``find-constant`` over ``*ir1-namespace*``)."""

    def visit_Node(self, node):
        return None

    def visit_NoneLiteral(self, node: ast.NoneLiteral):
        return ("none",)

    def visit_BooleanLiteral(self, node: ast.BooleanLiteral):
        return ("bool", node.value)

    def visit_IntegerLiteral(self, node: ast.IntegerLiteral):
        return ("int", node.value)

    def visit_FloatLiteral(self, node: ast.FloatLiteral):
        return ("float", struct.pack("=d", node.value))

    def visit_TextLiteral(self, node: ast.TextLiteral):
        return ("str", node.value)

    def visit_BytesLiteral(self, node: ast.BytesLiteral):
        return ("bytes", node.value)

    def visit_EllipsisLiteral(self, node: ast.EllipsisLiteral):
        return ("ellipsis",)

    def visit_Tuple(self, node: ast.Tuple):
        items = []
        for element in node.elements:
            key = element.accept(self)
            if key is None:
                return None
            items.append(key)
        return ("tuple", tuple(items))


_CONSTANT_KEY = _ConstantKey()


def find_constant_leaf(constants: dict[tuple, Constant], literal) -> Constant:
    key = literal.accept(_CONSTANT_KEY)
    if key is None:
        return Constant(literal)
    leaf = constants.get(key)
    if leaf is None:
        leaf = constants[key] = Constant(literal)
    return leaf


class Namespace:
    """Per-compilation leaf tables: free (global) variables, known functions,
    and constants (SBCL's ``*ir1-namespace*``)."""

    def __init__(self):
        self.free_vars: dict[tuple[str, str], GlobalVar] = {}
        self.known: dict[str, KnownFunction] = {}
        self.constants: dict[tuple, Constant] = {}

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

    def find_constant(self, literal) -> Constant:
        return find_constant_leaf(self.constants, literal)


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
        primitives=None,
        primitive_data=None,
    ):
        self.source = source.replace("\r\n", "\n").replace("\r", "\n")
        self.source_lines = self.source.splitlines(keepends=True)
        self.filename = filename
        self.imports = imports
        self.prefix = prefix
        self.namespace = namespace
        self.component = component
        self.ir1_namespace = ir1_namespace or Namespace()
        if self.component is not None:
            self.component.constants = self.ir1_namespace.constants
        # Compiled stdlib protocols name the backend's physical primitives by
        # underscored global names; those resolve to known functions, and
        # ``primitive_data`` supplies a primitive's compile-time operand.
        self.primitives: dict = primitives or {}
        self.primitive_data: dict = primitive_data or {}
        self.functions: list[Lambda] = []
        self.function_names: dict[str, int] = {}
        self.function_converters: dict[Lambda, FunctionConverter] = {}
        self.compiler_bindings = {}
        self.active_compiler_bindings = {}
        self.module_annotations = {}
        self.future_annotations = False
        self.named_constants = {"__debug__": ast.BooleanLiteral(True)}
        # The module's ``__name__`` as a literal while the module never
        # rebinds it, so ``if __name__ == "__main__":`` folds at compile time.
        self.module_name_literal: ast.TextLiteral | None = None
        self.module_file_literal: ast.TextLiteral | None = None
        self.class_definitions = 0
        self.class_identities: dict[int, str] = {}
        self.declared_class_bindings: dict[str, _CompilerBinding] = {}
        self.declared_class_match_info: dict[
            str, tuple[tuple[str, ...] | None, frozenset[str]]
        ] = {}
        self.class_fields = {}
        self.module_annotation_shadows = set()
        self.interface_bindings = {}
        # Set once the module's lambdas have their own components. Functions
        # defined later, by cleanup code converted during environment
        # analysis, collect in fresh initial components to split then.
        self.split = False
        self.converting_late = False
        self.late: list[tuple[Component, list[Lambda]]] = []

    def class_identity(self, node: ast.ClassDefinition) -> str:
        identity = self.class_identities.get(id(node))
        if identity is None:
            if self.prefix == "mule_sequences_" and node.name == "bytearray":
                identity = "mule_sequences_bytearray"
            elif self.namespace == "$mule.dict" and node.name in ("set", "frozenset"):
                identity = f"mule_dict_{node.name}"
            else:
                identity = f"{self.prefix}mule_cls_{self.class_definitions}"
            self.class_definitions += 1
            self.class_identities[id(node)] = identity
        return identity

    def location(self, node):
        span = node.span
        start = span.start if span else ast.Position(1, 0)
        end = span.end if span else ast.Position(1, 1)
        lines = self.source_lines
        line = lines[start.line - 1] if start.line <= len(lines) else None
        return (
            self.filename,
            start.line,
            start.column + 1,
            line,
            end.line,
            end.column + 1,
        )

    def source_text(self, node):
        """The exact source text a node's span covers."""
        start, end = node.span.start, node.span.end
        lines = self.source_lines
        if start.line == end.line:
            return lines[start.line - 1][start.column : end.column]
        parts = [lines[start.line - 1][start.column :]]
        line = start.line
        while line < end.line - 1:
            parts.append(lines[line])
            line += 1
        parts.append(lines[end.line - 1][: end.column])
        return "".join(parts)

    def error(self, message: str, node: ast.Node, *, unsupported=False) -> NoReturn:
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

    def declared_contract(self, expression, scope=None):
        return expression.accept(
            _DeclaredAnnotationReference(self, scope)
        ).declared_contract(self, expression)

    def policy(self, decorators, inherited):
        """Apply the definition's ``mule_types.optimize`` directives. They are
        compile-time declarations, not runtime decorators."""
        policy = inherited
        for decorator in decorators:
            policy = decorator.accept(_DecoratorBinding(self)).policy(
                self, decorator, policy
            )
        return policy

    def runtime_decorators(self, decorators):
        return [
            runtime
            for decorator in decorators
            for runtime in decorator.accept(_DecoratorBinding(self)).runtime_decorators(
                decorator
            )
        ]

    def constant_binding(self, name, node):
        if name in self.named_constants:
            self.error(f"cannot assign to {name}", node)

    def reserved(self, name, node):
        self.constant_binding(name, node)
        if name in self.compiler_bindings:
            self.error(f"cannot shadow compiler-only import '{name}'", node)

    def global_var(self, name) -> GlobalVar:
        return self.ir1_namespace.global_var(self.namespace, name)

    def known(self, info) -> KnownFunction:
        return self.ir1_namespace.known_function(info)

    def find_constant(self, literal) -> Constant:
        return self.ir1_namespace.find_constant(literal)

    def function(
        self,
        definition,
        policy,
        *,
        owner_class=None,
        parent=None,
        defining_class=None,
        qualname=None,
        body_kind=None,
        annotation_scope=False,
        source_identity=None,
    ) -> Lambda:
        # ir1tran_lambda imports FunctionConverter from this module.
        from .ir1tran_lambda import convert_function

        cache = parent._function_definitions if parent is not None else None
        scopes = []
        ancestor = parent
        while ancestor is not None:
            if ancestor.comprehension_scopes:
                cache = None
                break
            scopes.extend(ancestor.class_scopes)
            ancestor = ancestor.parent
        bindings = tuple(sorted(self.active_compiler_bindings.items()))
        key = (
            id(definition if source_identity is None else source_identity),
            id(policy),
            owner_class,
            id(defining_class),
            qualname,
            body_kind,
            annotation_scope,
            tuple(id(scope) for scope in scopes),
            tuple((name, id(binding)) for name, binding in bindings),
        )
        if cache is not None and key in cache:
            return cache[key][-1]

        if self.split and not self.converting_late:
            # Functions nested inside share the fresh component.
            self.converting_late = True
            saved, self.component = self.component, Component("late")
            created = len(self.functions)
            try:
                lambda_ = convert_function(
                    self,
                    definition,
                    policy,
                    owner_class=owner_class,
                    parent=parent,
                    defining_class=defining_class,
                    qualname=qualname,
                    body_kind=body_kind,
                    annotation_scope=annotation_scope,
                )
            finally:
                late, self.component = self.component, saved
                self.component.mutates_module_namespace |= late.mutates_module_namespace
                late.mutates_module_namespace = self.component.mutates_module_namespace
                self.converting_late = False
            self.late.append((late, self.functions[created:]))
        else:
            lambda_ = convert_function(
                self,
                definition,
                policy,
                owner_class=owner_class,
                parent=parent,
                defining_class=defining_class,
                qualname=qualname,
                body_kind=body_kind,
                annotation_scope=annotation_scope,
            )
        if cache is not None:
            # Retain identity-keyed context objects for this conversion's lifetime.
            cache[key] = (
                definition,
                source_identity,
                policy,
                defining_class,
                tuple(scopes),
                bindings,
                lambda_,
            )
        return lambda_


class _IsGlobalVar(LeafVisitor):
    def visit_Leaf(self, leaf):
        return False

    def visit_GlobalVar(self, leaf: GlobalVar):
        return True


class _IsLiteral(ast.Visitor):
    def visit_Node(self, node):
        return False

    def visit_Literal(self, node):
        return True

    visit_IntegerLiteral = visit_Literal
    visit_FloatLiteral = visit_Literal
    visit_ComplexLiteral = visit_Literal
    visit_BooleanLiteral = visit_Literal
    visit_NoneLiteral = visit_Literal
    visit_TextLiteral = visit_Literal
    visit_BytesLiteral = visit_Literal
    visit_EllipsisLiteral = visit_Literal


_IS_GLOBAL_VAR = _IsGlobalVar()
_IS_LITERAL = _IsLiteral()


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
        parent=None,
        defining_class: ClassScope | None = None,
        qualname: str | None = None,
        body_kind: str | None = None,
        annotation_scope: bool = False,
    ):
        self.converter = converter
        # A hidden PEP 695 scope, invisible in qualified names.
        self.annotation_scope = annotation_scope
        # "generator", "coroutine" or "async_generator" for the body such a
        # function runs on its own stack, else None.
        self.body_kind = body_kind
        self.lambda_ = lambda_
        self.is_module = is_module
        self.origin = origin
        # The enclosing function's converter, through which free variables
        # resolve to their home lambda's variables (Python's nested scopes).
        self.parent = parent
        # The class whose body defines this function, for ``__class__``.
        self.defining_class = defining_class
        # Class bodies being converted inline here, innermost last.
        self.class_scopes: list[ClassScope] = []
        # Comprehensions being converted inline here, innermost last.
        self.comprehension_scopes: list[ComprehensionScope] = []
        self.qualname = qualname
        self.nonlocals: set[str] = set()
        self.depth = 0
        self.annotations = {}
        self.hidden = 0
        self.globals: set[str] = set()
        self.locals: dict[str, LambdaVar] = {}
        self._cleanup_edges: dict[Block, Block] = {}
        self._function_definitions = {}
        for var in lambda_.vars:
            self.locals[var.name] = var
        self._bindings(statements)
        if not is_module:
            for var, kind, expr in zip(
                lambda_.vars,
                lambda_.signature.kinds,
                lambda_.signature.annotation_expressions,
            ):
                if expr is not None and var.name not in lambda_.local_annotations:
                    declared = converter.declared_contract(expr, parent)
                    if declared is not None:
                        if kind == ast.ParameterKind.VAR_POSITIONAL:
                            declared = TupleContract("tuple", None, (declared,))
                        elif kind == ast.ParameterKind.VAR_KEYWORD:
                            declared = DictContract(
                                "dict", DECLARATIONS["str"], declared
                            )
                        lambda_.local_annotations[var.name] = declared
        self.lexenv = Lexenv(vars=self.locals, lambda_=lambda_, policy=lambda_.policy)
        self.expressions: _Expressions | None = None
        self.statements_visitor: _Statements | None = None
        self.frames: list[tuple] = []
        converter.function_converters[lambda_] = self

    def finish_conversion(self):
        """Release source identities after late cleanup conversion finishes."""
        self._function_definitions.clear()

    @property
    def component(self):
        return self.converter.component

    def error(self, message: str, node: ast.Node, *, unsupported=False) -> NoReturn:
        self.converter.error(message, node, unsupported=unsupported)

    def _bindings(self, statements):
        """Classify the whole function, including assignments after an early read."""
        scope = _ScopeVisitor(self)
        for statement in statements:
            statement.accept(scope)
        self.seen_names = scope.seen
        if not self.is_module:
            collector = _LocalBindings(self)
            for statement in statements:
                statement.accept(collector)

    def local_snapshot_entries(self) -> list[tuple[str, LambdaVar]]:
        entries: dict[str, LambdaVar] = {}
        if not self.is_module:
            for name, var in self.locals.items():
                if not var.hidden and not name.startswith("$"):
                    entries[name] = var
            for name in sorted(self.seen_names):
                if (
                    name not in entries
                    and name not in self.globals
                    and not name.startswith("$")
                ):
                    free = self.enclosing_local(name)
                    if free is not None:
                        entries[name] = free
        for comp in self.comprehension_scopes:
            for name, var in comp.variables.items():
                if not name.startswith("$"):
                    entries[name] = var
        return list(entries.items())

    def snapshot_current_locals(self, ctran: Ctran, node) -> tuple[Ctran, Lvar]:
        if self.class_scopes and (
            not self.comprehension_scopes
            or self.comprehension_scopes[-1].class_depth < len(self.class_scopes)
        ):
            after_ns, namespace = Ctran(), Lvar()
            self.ref(
                ctran,
                after_ns,
                namespace,
                self.class_scopes[-1].namespace,
                node,
            )
            after_snapshot, snapshot = Ctran(), Lvar()
            self.protocol(
                after_ns,
                after_snapshot,
                snapshot,
                "globals_dict",
                (namespace,),
                node,
            )
            return after_snapshot, snapshot
        if self.is_module and not self.comprehension_scopes:
            after_globals, namespace = Ctran(), Lvar()
            self.known(ctran, after_globals, namespace, fndb.CURRENT_GLOBALS, (), node)
            after_snapshot, snapshot = Ctran(), Lvar()
            self.protocol(
                after_globals,
                after_snapshot,
                snapshot,
                "globals_dict",
                (namespace,),
                node,
            )
            return after_snapshot, snapshot
        entries = self.local_snapshot_entries()
        name_lvars = []
        value_lvars = []
        for name, var in entries:
            after_name, name_lvar = Ctran(), Lvar()
            self.constant(ctran, after_name, name_lvar, ast.TextLiteral(name))
            ctran = after_name
            name_lvars.append(name_lvar)
            after_val, val_lvar = Ctran(), Lvar()
            self.ref(
                ctran,
                after_val,
                val_lvar,
                var,
                node,
                allow_unbound=True,
            )
            ctran = after_val
            value_lvars.append(val_lvar)
        after_names, names_tuple = Ctran(), Lvar()
        self.known(
            ctran,
            after_names,
            names_tuple,
            fndb.BUILD_TUPLE,
            tuple(name_lvars),
            node,
        )
        after_values, values_tuple = Ctran(), Lvar()
        self.known(
            after_names,
            after_values,
            values_tuple,
            fndb.BUILD_TUPLE,
            tuple(value_lvars),
            node,
        )
        after_snapshot, snapshot = Ctran(), Lvar()
        self.protocol(
            after_values,
            after_snapshot,
            snapshot,
            "snapshot_locals",
            (names_tuple, values_tuple),
            node,
        )
        return after_snapshot, snapshot

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

    def temporary(self, *, expression=False) -> LambdaVar:
        name = f"$merge{self.hidden}"
        self.hidden += 1
        var = self.declare_local(name, hidden=True)
        var.expression_temporary = expression
        return var

    def comprehension_variable(self, name: str) -> LambdaVar | None:
        for scope in reversed(self.comprehension_scopes):
            var = scope.variables.get(name)
            if var is not None:
                return var
        return None

    def variable(self, name: str):
        var = self.comprehension_variable(name)
        if var is not None:
            return var
        if self.class_scopes and name in self.class_scopes[-1].globals:
            return self.converter.global_var(name)
        var = self.locals.get(name)
        if var is not None:
            return var
        if name not in self.globals:
            free = self.enclosing_local(name)
            if free is not None:
                return free
            if name == "__class__":
                cell = self.class_cell()
                if cell is not None:
                    return cell
        primitive = self.converter.primitives.get(name)
        # Explicit module bindings shadow the implicit physical primitives,
        # including in functions that read those bindings as globals.
        if (
            primitive is not None
            and name not in self.converter.module_annotation_shadows
        ):
            return self.converter.known(primitive)
        return self.converter.global_var(name)

    def enclosing_local(self, name: str):
        """The nearest enclosing function's variable bound to ``name``, if
        any. Class bodies are not function scopes and are skipped."""
        scope = self.parent
        while scope is not None:
            # An inlined comprehension's targets belong to the function
            # converting it, including the module body.
            var = scope.comprehension_variable(name)
            if var is not None:
                return var
            if scope.is_module or name in scope.globals:
                return None
            var = scope.locals.get(name)
            if var is not None:
                return var
            scope = scope.parent
        return None

    def class_cell(self) -> LambdaVar | None:
        """The ``__class__`` cell of the nearest class body enclosing this
        function, as Python provides it to methods and their nested functions."""
        scope = self
        while scope is not None and not scope.is_module:
            if scope.defining_class is not None:
                return scope.defining_class.class_cell()
            scope = scope.parent
        return None

    def qualified(self, name) -> str:
        """The ``__qualname__`` of a definition named ``name`` made here."""
        if self.annotation_scope and not self.class_scopes:
            return linked(self.parent, self, "parent").qualified(name)
        if self.class_scopes:
            return self.class_scopes[-1].qualname + "." + name
        if self.is_module:
            return name
        return linked(self.qualname, self, "qualname") + ".<locals>." + name

    def class_binding(self, name) -> ClassScope | None:
        """The class body whose namespace holds ``name``, if it is bound there."""
        if self.comprehension_variable(name) is not None:
            return None
        if self.comprehension_scopes and self.comprehension_scopes[
            -1
        ].class_depth == len(self.class_scopes):
            return None
        if self.class_scopes and name in self.class_scopes[-1].bound:
            return self.class_scopes[-1]
        if (
            self.annotation_scope
            and self.defining_class is not None
            and name in self.defining_class.bound
            and name not in self.locals
        ):
            self.defining_class.annotation_namespace_used = True
            return self.defining_class
        return None

    def annotation_for(self, name):
        if self.comprehension_variable(name) is not None:
            return None
        if self.local(name):
            return self.annotations.get(name)
        if name not in self.globals and self.enclosing_local(name) is not None:
            return None
        return self.converter.module_annotations.get(name)

    # Node construction primitives

    def block(self, ctran: Ctran | None = None) -> Block:
        ctran = ctran or Ctran()
        return ctran_starts_block(ctran, self.component)

    def ref(
        self, start, next_, result, leaf, source, *, final=False, allow_unbound=False
    ) -> Ref:
        node = Ref(self.lexenv, source, leaf, final=final, allow_unbound=allow_unbound)
        leaf.refs.append(node)
        link_node_to_previous_ctran(node, start)
        use_continuation(node, next_, result)
        return node

    def constant(self, start, next_, result, literal) -> Ref:
        return self.ref(
            start, next_, result, self.converter.find_constant(literal), literal
        )

    def load(self, start, next_, result, name, source, *, final=False):
        scope = self.class_binding(name) or self.class_reader(name)
        if scope is not None:
            load = _ClassNamespaceLoad(self, start, next_, result, scope, name, source)
            if name in scope.bound:
                return load.namespace()
            return self.variable(name).accept(load)
        var = self.variable(name)
        literal = self.converter.module_name_literal
        if literal is not None and name == "__name__" and var.accept(_IS_GLOBAL_VAR):
            return self.constant(
                start, next_, result, replace(literal, span=source.span)
            )
        file_literal = self.converter.module_file_literal
        if (
            file_literal is not None
            and name == "__file__"
            and var.accept(_IS_GLOBAL_VAR)
        ):
            return self.constant(
                start, next_, result, replace(file_literal, span=source.span)
            )
        return self.ref(start, next_, result, var, source, final=final)

    def class_reader(self, name) -> ClassScope | None:
        """The class body that reads ``name`` from its namespace first, as
        every name a class body does not declare global (LOAD_NAME)."""
        if not self.class_scopes or self.comprehension_variable(name) is not None:
            return None
        if self.comprehension_scopes and self.comprehension_scopes[
            -1
        ].class_depth == len(self.class_scopes):
            return None
        scope = self.class_scopes[-1]
        if name in scope.globals or name.startswith("$"):
            return None
        return scope

    def class_namespace_operation(
        self, start, next_, result, info, scope, name, operands, source, **options
    ):
        ctran, namespace = Ctran(), Lvar()
        self.ref(start, ctran, namespace, scope.namespace, source)
        return self.known(
            ctran,
            next_,
            result,
            info,
            (namespace, *operands),
            source,
            ClassBindingInfo(
                name,
                scope.identity,
                self.converter.location(source) if source is not None else None,
                **options,
            ),
        )

    def set_var(
        self, start, next_, var, value: Lvar | None, source, *, missing_ok: bool = False
    ) -> Set:
        node = Set(self.lexenv, source, var, value, missing_ok=missing_ok)
        if value is not None:
            value.dest = node
        var.sets.append(node)
        link_node_to_previous_ctran(node, start)
        use_ctran(node, next_)
        return node

    def cast(
        self,
        start,
        next_,
        result,
        value: Lvar,
        contract,
        node,
        safety=None,
        declaration=None,
    ):
        cast = Cast(
            self.lexenv,
            node,
            value,
            contract,
            self.converter.location(node),
            self.lambda_.policy.safety if safety is None else safety,
            declaration,
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
        """Emit a call through an evaluated callable value. A callee that is
        a reference to a known function (a compiled protocol's primitive)
        makes the call known, as ``ir1-convert-combination`` does."""
        node = Combination(self.lexenv, source, callee)
        callee.dest = node
        node.info = data
        info = callee.uses[0].accept(_KnownCallee()) if len(callee.uses) == 1 else None
        if info is not None:
            node.kind = CallKind.KNOWN
            node.fun_info = info
            node.info = self.converter.primitive_data.get(info.name)
        self._combination_args(node, start, next_, result, operands)
        return node

    def _combination_args(self, node, start, next_, result, operands):
        args = []
        for operand in operands:
            start, lvar = operand.accept(_CombinationArg(self, node, start))
            args.append(lvar)
        node.args = args
        link_node_to_previous_ctran(node, start)
        if next_ is None:
            aver(node.fun_info is not None and node.fun_info.never_returns)
            node.block.last = node
            use_lvar(node, result)
        else:
            use_continuation(node, next_, result)

    def protocol(self, start, next_, result, name, operands, source, data=None):
        return self.known(
            start, next_, result, fndb.PROTOCOLS[name], operands, source, data
        )

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
            previous = linked(frontier.use, frontier, "use")
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
        self.flow(frontier, linked(target.start, target, "start"))

    # Conversion drivers

    def convert(self, start, next_, result, expression) -> None:
        self.frames.append((start, next_, result))
        try:
            visitor = self.expressions or _Expressions(self)
            expression.accept(visitor)
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
            visitor = self.statements_visitor or _Statements(self)
            statement.accept(visitor)
        finally:
            self.frames.pop()

    def convert_body(self, start: Ctran, statements) -> Ctran | None:
        """Convert statements in sequence from ``start``. Returns the open
        frontier, or ``None`` when the final statement transferred control.
        Code after a transfer is still converted, into an unreachable block, so
        unsupported syntax there is rejected."""
        if self.depth == 0:
            self.expressions = _Expressions(self)
            self.statements_visitor = _Statements(self)
        self.depth += 1
        try:
            current = start
            terminated = False
            for statement in statements:
                if terminated:
                    block = self.block(current)
                    current = linked(block.start, block, "start")
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
            if self.depth == 0:
                self.expressions = None
                self.statements_visitor = None

    def leave_handlers(self, start: Ctran, stop: Cleanup | None, source) -> Ctran:
        """Emit the handler-exit actions for leaving every handler body between
        the current cleanup and ``stop``."""
        leaving = []
        cleanup = self.lexenv.cleanup
        while cleanup is not stop:
            if cleanup is None:
                raise ValueError(f"{stop!r} does not enclose the current cleanup")
            leaving.append(cleanup)
            cleanup = cleanup.parent
        return self.leave_handler_bodies(start, leaving, source)

    def leave_handler_bodies(self, start: Ctran, cleanups, source) -> Ctran:
        for cleanup in cleanups:
            if cleanup.kind is not CleanupKind.HANDLER_BODY:
                continue
            if cleanup.binding is not None:
                following = Ctran()
                saved, self.class_scopes = self.class_scopes, list(cleanup.class_scopes)
                try:
                    self.unbind(
                        start, following, cleanup.binding, source, missing_ok=True
                    )
                finally:
                    self.class_scopes = saved
                start = following
            if cleanup.info is not False:
                following = Ctran()
                self.known(start, following, None, fndb.LEAVE_HANDLER, (), source)
                start = following
        return start

    def with_cleanup(self, cleanup: Cleanup | None):
        return _CleanupScope(self, cleanup)

    def with_loop(self, target: LoopTarget):
        return _LoopScope(self, target)

    def call_info(self, source):
        return CallInfo(
            (), self.converter.location(source) if source is not None else None
        )

    def insert_cleanup_block(self, pred: Block, succ: Block, cleanup: Cleanup) -> Block:
        """Interpose an empty block on the edge ``pred -> succ`` for code that
        leaves ``cleanup`` (``insert-cleanup-code``)."""
        # After conversion the lambda owns its component; the conversion-time
        # initial component has been split away.
        component = linked(self.lambda_.component, self.lambda_, "component")
        block = ctran_starts_block(Ctran(), component)
        change_block_successor(pred, succ, block)
        self._cleanup_edges[block] = succ
        component.reanalyze = True
        return block

    def in_cleanup_block(self, block: Block, cleanup: Cleanup):
        """Convert into ``block`` in the environment enclosing ``cleanup``."""
        return _CleanupBlockScope(self, block, cleanup)

    def finish_cleanup_block(self, frontier: Ctran, block: Block) -> None:
        """Connect the open frontier of the code converted into the inserted
        ``block`` to the edge's original successor. Cleanup code with its own
        control flow, such as a ``try`` inside a ``finally`` suite, leaves its
        frontier in a later block."""
        self.jump(frontier, self._cleanup_edges.pop(block))

    # Expression helpers shared by several translators

    def assert_type(self, start, next_, result, value, contract, node):
        return self.cast(start, next_, result, value, contract, node)

    def store(self, start, next_, name: str, value: Lvar, node=None) -> None:
        if node is not None:
            self.converter.reserved(name, node)
        scope = self.class_binding(name)
        if scope is not None:
            self.class_namespace_operation(
                start, next_, None, fndb.CLASS_STORE, scope, name, (value,), node
            )
            return
        annotation = self.annotation_for(name)
        if annotation is not None:
            contract, declaration = annotation
            source = (
                ("local", self.lambda_, name)
                if self.local(name)
                else ("global", self.converter.namespace, name)
            )
            ctran, checked = Ctran(), Lvar()
            self.cast(
                start,
                ctran,
                checked,
                value,
                contract,
                node or declaration,
                declaration=source,
            )
            start, value = ctran, checked
        self.set_var(start, next_, self.variable(name), value, node)

    def unbind(self, start, next_, name, source, *, missing_ok=False):
        """Remove a binding whose presence a preceding read established, or,
        with ``missing_ok``, a handler binding the body may have deleted."""
        scope = self.class_binding(name)
        if scope is not None:
            self.class_namespace_operation(
                start,
                next_,
                None,
                fndb.CLASS_DELETE,
                scope,
                name,
                (),
                source,
                missing_ok=missing_ok,
            )
            return
        self.set_var(
            start, next_, self.variable(name), None, source, missing_ok=missing_ok
        )

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
        current, left = self.value(start, node.left)
        for index, (operator, comparator) in enumerate(
            zip(node.operators, node.comparators)
        ):
            if index > 0:
                ctran, left = Ctran(), Lvar()
                self.ref(current, ctran, left, previous, node, final=True)
                current = ctran
            current, right = self.value(current, comparator)
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
        current, left = self.value(start, node.left)
        for index, (operator, comparator) in enumerate(
            zip(node.operators, node.comparators)
        ):
            last = index == len(node.operators) - 1
            if index > 0:
                ctran, left = Ctran(), Lvar()
                self.ref(current, ctran, left, previous, node, final=True)
                current = ctran
            current, right = self.value(current, comparator)
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
            next_block, failed = self.block(), self.block()
            ctran, test = Ctran(), Lvar()
            self.ref(current, ctran, test, outcome, node)
            self.if_node(ctran, test, next_block, failed, node)
            ctran = Ctran()
            self.discard(failed.start, ctran, previous.name, node)
            self.jump(ctran, join)
            current = Ctran()
            self.discard(next_block.start, current, outcome.name, node)
        self.ref(join.start, next_, result, outcome, node, final=True)

    def call(self, start, next_, result, node: ast.Call):
        keywords = set()
        if any(
            argument.kind in (ast.ArgumentKind.STARRED, ast.ArgumentKind.DOUBLE_STARRED)
            for argument in node.arguments
        ):
            self.expanded_call(start, next_, result, node)
            return
        for argument in node.arguments:
            if argument.kind == ast.ArgumentKind.KEYWORD:
                if argument.keyword in keywords:
                    self.error(
                        f"keyword argument repeated: {argument.keyword}", argument
                    )
                keywords.add(argument.keyword)
        node.function.accept(_CallTarget(self, start, next_, result, node))

    def keyword_dict(self, start, target, arguments, node) -> Ctran:
        """Collect ``name=value`` and ``**mapping`` arguments, in source order,
        into a new dict held by ``target``; duplicate names raise TypeError."""
        ctran, value = self.value(start, ast.Dict([], span=node.span))
        current = Ctran()
        self.set_var(ctran, current, target, value, node)
        for argument in arguments:
            ctran, held = Ctran(), Lvar()
            self.ref(current, ctran, held, target, argument)
            following = Ctran()
            if argument.kind == ast.ArgumentKind.KEYWORD:
                operands = (
                    held,
                    ast.TextLiteral(argument.keyword, span=argument.span),
                    argument.value,
                )
                self.protocol(ctran, following, None, "add_keyword", operands, argument)
            else:
                self.protocol(
                    ctran,
                    following,
                    None,
                    "merge_keywords",
                    (held, argument.value),
                    argument,
                )
            current = following
        return current

    def expanded_call(self, start, next_, result, node: ast.Call):
        """``f(a, *b, k=c, **d)``: the callee, then every argument in source
        order, collected into a positional list and a keyword dict that the
        compiled ``call_expanded`` protocol passes to the callee. Duplicate
        keywords from ``**`` mappings raise TypeError there, as in CPython.

        Without keyword arguments no dict is built, and a lone ``f(*b)``
        passes ``b`` itself to ``call_expanded_positional``, which reads an
        exact tuple as the argument tuple without copying it."""
        positional_only = all(
            argument.kind in (ast.ArgumentKind.POSITIONAL, ast.ArgumentKind.STARRED)
            for argument in node.arguments
        )
        if positional_only and len(node.arguments) == 1:
            ctran, callee = self.value(start, node.function)
            following, arguments = self.value(ctran, node.arguments[0].value)
            self.protocol(
                following,
                next_,
                result,
                "call_expanded_positional",
                (callee, arguments),
                node,
            )
            return
        callee, positional, keywords = (self.temporary() for _ in range(3))
        current = start
        for var, expression in (
            (callee, node.function),
            (positional, ast.List([], span=node.span)),
            (keywords, ast.Dict([], span=node.span)),
        ):
            if var is keywords and positional_only:
                continue
            ctran, value = self.value(current, expression)
            current = Ctran()
            self.set_var(ctran, current, var, value, node)
        ordered_arguments = [
            argument
            for argument in node.arguments
            if argument.kind in (ast.ArgumentKind.POSITIONAL, ast.ArgumentKind.STARRED)
        ] + [
            argument
            for argument in node.arguments
            if argument.kind
            not in (ast.ArgumentKind.POSITIONAL, ast.ArgumentKind.STARRED)
        ]
        for argument in ordered_arguments:
            if argument.kind in (ast.ArgumentKind.POSITIONAL, ast.ArgumentKind.STARRED):
                target, name = (
                    positional,
                    (
                        "append_argument"
                        if argument.kind == ast.ArgumentKind.POSITIONAL
                        else "extend_arguments"
                    ),
                )
                operands = []
            else:
                target = keywords
                if argument.kind == ast.ArgumentKind.KEYWORD:
                    name = "add_keyword"
                    keyword = linked(argument.keyword, argument, "keyword")
                    operands = [ast.TextLiteral(keyword, span=argument.span)]
                else:
                    name = "merge_keywords"
                    operands = []
            ctran, held = Ctran(), Lvar()
            self.ref(current, ctran, held, target, argument)
            following = Ctran()
            self.protocol(
                ctran,
                following,
                None,
                name,
                (held, *operands, argument.value),
                argument,
            )
            current = following
        operands = []
        for var in (
            (callee, positional) if positional_only else (callee, positional, keywords)
        ):
            ctran, value = Ctran(), Lvar()
            self.ref(current, ctran, value, var, node, final=True)
            current = ctran
            operands.append(value)
        name = "call_expanded_positional" if positional_only else "call_expanded"
        self.protocol(current, next_, result, name, tuple(operands), node)

    def import_module(self, start, name, node) -> tuple[Ctran, Lvar]:
        imports = self.converter.imports
        if imports is None:
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
            descriptor = imports.resolve(qualified, self.converter, node)
            ctran, imported = Ctran(), Lvar()
            self.known(
                current,
                ctran,
                imported,
                fndb.IMPORT,
                (),
                node,
                descriptor.import_info(),
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
        if parent is None:
            raise ValueError(f"cannot import an empty module name {name!r}")
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

    def evaluate_decorators(self, start, decorators):
        """Evaluate runtime decorator expressions top to bottom, before the
        definition they decorate. Returns the temporaries holding their values."""
        current = start
        held = []
        for decorator in decorators:
            ctran, value = self.value(current, decorator)
            temporary = self.temporary()
            current = Ctran()
            self.set_var(ctran, current, temporary, value, decorator)
            held.append(temporary)
        return current, held

    def apply_decorators(self, start, held, value, decorators):
        """Call the evaluated decorators on ``value``, innermost first."""
        if not held:
            return start, value
        result = self.temporary()
        current = Ctran()
        self.set_var(start, current, result, value, decorators[-1])
        for temporary, decorator in reversed(tuple(zip(held, decorators, strict=True))):
            ctran, callee = Ctran(), Lvar()
            self.ref(current, ctran, callee, temporary, decorator, final=True)
            following, argument = Ctran(), Lvar()
            self.ref(ctran, following, argument, result, decorator, final=True)
            ctran, decorated = Ctran(), Lvar()
            self.full_call(
                following,
                ctran,
                decorated,
                callee,
                (argument,),
                decorator,
                CallInfo((), self.converter.location(decorator)),
            )
            current = Ctran()
            self.set_var(ctran, current, result, decorated, decorator)
        ctran, value = Ctran(), Lvar()
        self.ref(current, ctran, value, result, decorators[0], final=True)
        return ctran, value

    def define_function(self, start, next_, result, lambda_, definition, public_name):
        """Evaluate the definition's defaults in this scope, left to right, then
        reference the converted lambda and create its runtime function object."""
        current = start
        defaults = []
        for parameter in definition.parameters:
            if parameter.default is not None:
                current, value = self.value(current, parameter.default)
                defaults.append(value)
        enclose = Enclose(self.lexenv, definition, [lambda_])
        link_node_to_previous_ctran(enclose, current)
        ctran = Ctran()
        use_ctran(enclose, ctran)
        following, fun = Ctran(), Lvar()
        self.ref(ctran, following, fun, lambda_, definition)
        default_contracts = tuple(
            lambda_.signature.annotations[index]
            for index in lambda_.signature.default_parameters
        )
        from .annotation_runtime import AnnotationThunk

        annotate = AnnotationThunk(
            self.converter, definition, _RuntimeAnnotation(self.converter)
        ).build()
        created = Lvar() if annotate is not None else result
        after = Ctran() if annotate is not None else next_
        self.known(
            following,
            after,
            created,
            fndb.MAKE_FUNCTION,
            (fun, *defaults),
            definition,
            FunctionInfo(
                public_name,
                default_contracts,
                self.converter.location(definition),
                _documentation(definition.body),
            ),
        )
        if annotate is None:
            return
        original = self.temporary()
        current = Ctran()
        self.set_var(after, current, original, created, definition)
        scope = self.class_scopes[-1] if self.class_scopes else self.defining_class
        thunk = self.converter.function(
            annotate,
            lambda_.policy,
            parent=self,
            defining_class=scope,
            qualname=f"{lambda_.code.qualname}.__annotate__",
            annotation_scope=True,
        )
        following, callback = Ctran(), Lvar()
        self.define_function(
            current, following, callback, thunk, annotate, "__annotate__"
        )
        current, owner = Ctran(), Lvar()
        self.ref(following, current, owner, original, definition)
        following = Ctran()
        self.known(
            current,
            following,
            None,
            fndb.SETATTR,
            (owner, callback),
            definition,
            AttributeInfo("__annotate__", self.converter.location(definition)),
        )
        self.ref(following, next_, result, original, definition, final=True)


class _CleanupScope:
    def __init__(self, function, cleanup):
        self.function, self.cleanup = function, cleanup

    def __enter__(self):
        self.saved = self.function.lexenv
        self.function.lexenv = self.saved.derive(cleanup=self.cleanup)
        if self.cleanup is not None:
            self.cleanup.class_scopes = tuple(self.function.class_scopes)
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


class _CleanupBlockScope:
    """Converts cleanup code for leaving ``cleanup`` into ``block``, in the
    lexical environment of the cleanup's parent and with the loop targets in
    effect where the region was entered."""

    def __init__(self, function, block, cleanup):
        self.function, self.block, self.cleanup = function, block, cleanup

    def __enter__(self):
        self.saved = self.function.lexenv
        self.function.lexenv = self.saved.derive(
            cleanup=self.cleanup.parent, loops=self.cleanup.loops
        )
        self.saved_class_scopes = self.function.class_scopes
        self.function.class_scopes = list(self.cleanup.class_scopes)
        # Blocks the cleanup code creates belong to the lambda's component;
        # the conversion-time component has been split away by now.
        converter = self.function.converter
        self.saved_component = converter.component
        converter.component = self.function.lambda_.component
        return self.block.start

    def __exit__(self, *exc):
        self.function.lexenv = self.saved
        self.function.class_scopes = self.saved_class_scopes
        self.function.converter.component = self.saved_component


class _ClassNamespaceLoad(LeafVisitor):
    """A class body's read of a name: from its namespace, which falls back
    to globals and builtins at runtime, unless the name resolves to a
    variable of an enclosing function."""

    def __init__(self, function, start, next_, result, scope, name, source):
        self.function = function
        self.start = start
        self.next = next_
        self.result = result
        self.scope = scope
        self.name = name
        self.source = source

    def namespace(self):
        return self.function.class_namespace_operation(
            self.start,
            self.next,
            self.result,
            fndb.CLASS_LOAD,
            self.scope,
            self.name,
            (),
            self.source,
        )

    def visit_Leaf(self, leaf):
        return self.namespace()

    def visit_LambdaVar(self, leaf):
        # A free variable of an enclosing function: the namespace first,
        # then the closure (LOAD_FROM_DICT_OR_DEREF).
        function = self.function
        ctran, fallback = Ctran(), Lvar()
        function.ref(self.start, ctran, fallback, leaf, self.source)
        return function.class_namespace_operation(
            ctran,
            self.next,
            self.result,
            fndb.CLASS_LOAD_OR,
            self.scope,
            self.name,
            (fallback,),
            self.source,
        )


class _KnownCallee(NodeVisitor):
    """The function-database entry a callee's producer refers to, if any."""

    def visit_Ref(self, node):
        return node.leaf.accept(_KnownFunctionInfo())


class _KnownFunctionInfo(LeafVisitor):
    def visit_KnownFunction(self, leaf):
        return leaf.fun_info


class _CombinationArg(ast.Visitor):
    """Connects one operand to ``combination``: a computed value directly,
    an expression after converting it. Returns the following ctran and the
    argument lvar."""

    def __init__(self, function, combination, start):
        self.function = function
        self.combination = combination
        self.start = start

    def visit_Node(self, node):
        ctran, lvar = Ctran(), Lvar(self.combination)
        self.function.convert(self.start, ctran, lvar, node)
        return ctran, lvar

    def visit_Lvar(self, lvar):
        aver(lvar.dest is None)
        lvar.dest = self.combination
        return self.start, lvar


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
    visit_ComplexLiteral = literal
    visit_BooleanLiteral = literal
    visit_NoneLiteral = literal
    visit_TextLiteral = literal
    visit_BytesLiteral = literal

    def visit_EllipsisLiteral(self, node):
        start, next_, result = self.frame
        self.function.protocol(start, next_, result, "ellipsis_value", (), node)

    def visit_Name(self, node):
        function = self.function
        constant = function.converter.named_constants.get(node.identifier)
        if constant is not None:
            start, next_, result = self.frame
            function.constant(start, next_, result, replace(constant, span=node.span))
            return
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

    def visit_Slice(self, node):
        function = self.function
        start, next_, result = self.frame
        bounds = tuple(
            bound if bound is not None else ast.NoneLiteral(None, span=node.span)
            for bound in (node.lower, node.upper, node.step)
        )
        function.protocol(start, next_, result, "make_slice", bounds, node)

    def visit_Dict(self, node):
        """Keys and values are evaluated left to right, then inserted in order."""
        function = self.function
        start, next_, result = self.frame
        if any(entry.key is None for entry in node.entries):
            self.unpacking_dict(node)
            return
        flat = []
        for entry in node.entries:
            flat.extend((entry.key, entry.value))
        ctran, items = Ctran(), Lvar()
        function.known(start, ctran, items, fndb.BUILD_TUPLE, tuple(flat), node)
        function.protocol(ctran, next_, result, "build_dict", (items,), node)

    def visit_Set(self, node):
        self.display(node, self.set_display, "build_set")

    def set_display(self, node):
        function = self.function
        start, next_, result = self.frame
        ctran, items = Ctran(), Lvar()
        function.known(
            start, ctran, items, fndb.BUILD_TUPLE, tuple(node.elements), node
        )
        function.protocol(ctran, next_, result, "build_set", (items,), node)

    def visit_Yield(self, node):
        function = self.function
        start, next_, result = self.frame
        if function.is_module or function.class_scopes or function.comprehension_scopes:
            function.error("'yield' outside function", node)
        value = node.value if node.value is not None else ast.NoneLiteral(None)
        if function.body_kind == "async_generator":
            if node.from_iterable:
                function.error("'yield from' inside async function", node)
            function.protocol(
                start, next_, result, "async_generator_yield", (value,), node
            )
        elif node.from_iterable:
            function.protocol(start, next_, result, "yield_from", (value,), node)
        else:
            function.known(start, next_, result, fndb.YIELD, (value,), node)

    def visit_Await(self, node):
        function = self.function
        start, next_, result = self.frame
        if (
            function.body_kind not in ("coroutine", "async_generator")
            or function.class_scopes
        ):
            function.error("'await' outside async function", node)
        function.protocol(start, next_, result, "await_value", (node.value,), node)

    def generator_expression(self, node):
        """A generator expression is a generator function called with the
        iterator of its first iterable, which is obtained in the enclosing
        scope when the expression runs; CPython 3.14 still compiles it so."""
        function = self.function
        start, next_, result = self.frame
        span = node.span
        body: list[ast.Statement] = [
            ast.ExpressionStatement(ast.Yield(node.element, span=span), span=span)
        ]
        for index in range(len(node.clauses) - 1, -1, -1):
            clause = node.clauses[index]
            for condition in reversed(clause.conditions):
                body = [ast.If(condition, body, span=span)]
            source = ast.Name("$iterable", span=span) if index == 0 else clause.iterable
            body = [
                ast.For(
                    clause.target, source, body, is_async=clause.is_async, span=span
                )
            ]
        is_async = any(stmt.accept(_ContainsAsync()) for stmt in body)
        # Assignment expressions bind in the enclosing scope (PEP 572).
        assigned = _comprehension_assignments(node)
        if assigned:
            declaration = ast.Global if function.is_module else ast.Nonlocal
            body = [declaration(assigned, span=span), *body]
        definition = ast.FunctionDefinition(
            "<genexpr>",
            [ast.Parameter("$iterable", ast.ParameterKind.POSITIONAL_ONLY, span=span)],
            body,
            is_async=is_async,
            span=span,
        )
        lambda_ = function.converter.function(
            definition, function.lambda_.policy, parent=function
        )
        ctran, created = Ctran(), Lvar()
        function.define_function(
            start, ctran, created, lambda_, definition, definition.name
        )
        ctran, iterable = function.value(ctran, node.clauses[0].iterable)
        if node.clauses[0].is_async:
            following, iterator = ctran, iterable
        else:
            following, iterator = Ctran(), Lvar()
            function.protocol(ctran, following, iterator, "iterate", (iterable,), node)
        function.full_call(
            following,
            next_,
            result,
            created,
            (iterator,),
            node,
            CallInfo((), function.converter.location(node)),
        )

    def visit_Comprehension(self, node):
        """A list, set or dict comprehension, inlined in the enclosing function
        as Python 3.12 and later do (PEP 709). The first iterable is evaluated
        in the enclosing scope. The clause targets are bound to hidden
        variables, so they neither see nor change enclosing bindings of the
        same names, and inside a class body the comprehension's other names
        skip the class namespace, as they did when comprehensions were
        functions. SBCL likewise expands iteration forms into its ordinary
        variable and loop constructs rather than adding a special form."""
        function = self.function
        start, next_, result = self.frame
        if node.accept(_ComprehensionAssignment()) and function.class_scopes:
            function.error(
                "assignment expression within a comprehension cannot be used in a class body",
                node,
            )
        if node.kind == ast.ComprehensionKind.GENERATOR:
            self.generator_expression(node)
            return
        span = node.span
        ctran, iterable = function.value(start, node.clauses[0].iterable)
        held = function.temporary()
        current = Ctran()
        function.set_var(ctran, current, held, iterable, node)
        targets = set()
        for clause in node.clauses:
            clause.target.accept(_BoundNames(targets))
        variables = {name: function.temporary() for name in sorted(targets)}
        accumulator = function.temporary()
        variables["$iterable"], variables["$result"] = held, accumulator

        result_name = ast.Name("$result", span=span)
        if node.kind == ast.ComprehensionKind.DICT:
            initial = ast.Dict([], span=span)
            variables["$key"] = function.temporary()
            key_name = ast.Name("$key", span=span)
            body: list[ast.Statement] = [
                ast.Assignment([key_name], node.key, span=span),
                ast.Assignment(
                    [ast.Subscript(result_name, key_name, span=span)],
                    node.element,
                    span=span,
                ),
            ]
        else:
            if node.kind == ast.ComprehensionKind.LIST:
                initial, method = ast.List([], span=span), "append"
            else:
                initial, method = ast.Set([], span=span), "add"
            add = ast.ExpressionStatement(
                ast.Call(
                    ast.Attribute(result_name, method, span=span),
                    [ast.CallArgument(node.element, span=span)],
                    span=span,
                ),
                span=span,
            )
            body = [add]
        for index in range(len(node.clauses) - 1, -1, -1):
            clause = node.clauses[index]
            for condition in reversed(clause.conditions):
                body = [ast.If(condition, body, span=span)]
            source = ast.Name("$iterable", span=span) if index == 0 else clause.iterable
            body = [
                ast.For(
                    clause.target, source, body, is_async=clause.is_async, span=span
                )
            ]
        statements = [ast.Assignment([result_name], initial, span=span), *body]

        function.comprehension_scopes.append(
            ComprehensionScope(variables, len(function.class_scopes))
        )
        try:
            frontier = function.convert_body(current, statements)
        finally:
            function.comprehension_scopes.pop()
        ctran = Ctran()
        function.discard(frontier, ctran, held.name, node)
        function.ref(ctran, next_, result, accumulator, node, final=True)

    def visit_List(self, node):
        self.display(
            node, lambda display: self.sequence(display, fndb.BUILD_LIST), None
        )

    def visit_Tuple(self, node):
        self.display(
            node,
            lambda display: self.sequence(display, fndb.BUILD_TUPLE),
            "tuple_from_list",
        )

    def display(self, node, build, conversion):
        lowering = _DisplayLowering(self, build, conversion)
        for element in node.elements:
            element.accept(lowering)
        lowering.build(node)

    def unpacking_display(self, node, conversion):
        """A display with ``*iterable`` elements: the elements, in order, are
        collected into a new list, then converted to the display's type."""
        function = self.function
        start, next_, result = self.frame
        items = function.temporary()
        ctran, value = function.value(start, ast.List([], span=node.span))
        current = Ctran()
        function.set_var(ctran, current, items, value, node)
        for element in node.elements:
            ctran, held = Ctran(), Lvar()
            function.ref(current, ctran, held, items, element)
            following = Ctran()
            element.accept(_DisplayItem(function, ctran, following, held))
            current = following
        if conversion is None:
            function.ref(current, next_, result, items, node, final=True)
            return
        ctran, held = Ctran(), Lvar()
        function.ref(current, ctran, held, items, node, final=True)
        function.protocol(ctran, next_, result, conversion, (held,), node)

    def unpacking_dict(self, node):
        """A dict display with ``**mapping`` entries, merged in order, later
        keys replacing earlier ones."""
        function = self.function
        start, next_, result = self.frame
        mapping = function.temporary()
        ctran, value = function.value(start, ast.Dict([], span=node.span))
        current = Ctran()
        function.set_var(ctran, current, mapping, value, node)
        for entry in node.entries:
            ctran, held = Ctran(), Lvar()
            function.ref(current, ctran, held, mapping, entry)
            following = Ctran()
            if entry.key is None:
                function.protocol(
                    ctran, following, None, "dict_merge", (held, entry.value), entry
                )
            else:
                function.protocol(
                    ctran,
                    following,
                    None,
                    "setitem",
                    (held, entry.key, entry.value),
                    entry,
                )
            current = following
        function.ref(current, next_, result, mapping, node, final=True)

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

    def visit_TemplateString(self, node):
        """``t"..."``: the literal parts and an Interpolation per field,
        evaluated left to right, then combined into a Template."""
        function = self.function
        start, next_, result = self.frame
        current = start
        parts = []
        for part in node.parts:
            current, value = part.accept(_TemplatePart(function, current))
            parts.append(value)
        ctran, items = Ctran(), Lvar()
        function.known(current, ctran, items, fndb.BUILD_TUPLE, tuple(parts), node)
        function.protocol(ctran, next_, result, "build_template", (items,), node)

    def visit_Interpolation(self, node):
        function = self.function
        start, next_, result = self.frame
        if node.conversion not in (None, "s", "r", "a"):
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

    def visit_Lambda(self, node):
        """A lambda is an anonymous function whose body returns one expression,
        converted like ``def`` as SBCL converts ``lambda`` and named functions
        through the same ``ir1-convert-lambda`` path."""
        function = self.function
        start, next_, result = self.frame
        definition = ast.FunctionDefinition(
            "<lambda>",
            node.parameters,
            [ast.Return(node.body, span=node.body.span)],
            span=node.span,
        )
        lambda_ = function.converter.function(
            definition,
            function.lambda_.policy,
            parent=function,
            defining_class=function.defining_class
            if function.annotation_scope
            else None,
            annotation_scope=function.annotation_scope,
            source_identity=node,
        )
        function.define_function(
            start, next_, result, lambda_, definition, definition.name
        )


class _DisplayLowering(ast.Visitor):
    """How a display is built: by ``build`` over its elements, or, once an
    element is ``*iterable``, by collecting the elements into a list that
    ``conversion`` turns into the display's type."""

    def __init__(self, expressions: _Expressions, build, conversion):
        self.expressions = expressions
        self.build = build
        self.conversion = conversion

    def visit_Unpack(self, node):
        self.build = self.unpacking

    def unpacking(self, node):
        self.expressions.unpacking_display(node, self.conversion)


class _DisplayItem(ast.Visitor):
    """Adds one display element to the list in ``held``."""

    def __init__(self, function, start, next_, held):
        self.function = function
        self.start = start
        self.next = next_
        self.held = held

    def visit_Node(self, node):
        self.function.protocol(
            self.start, self.next, None, "append_argument", (self.held, node), node
        )

    def visit_Unpack(self, node):
        self.function.protocol(
            self.start,
            self.next,
            None,
            "extend_arguments",
            (self.held, node.value),
            node,
        )


class _TemplatePart(ast.Visitor):
    """Evaluates one part of a template string from ``start``: a literal
    part to its value, a field to an Interpolation. Returns the following
    ctran and the value."""

    def __init__(self, function, start):
        self.function = function
        self.start = start

    def visit_Node(self, node):
        return self.function.value(self.start, node)

    def visit_Interpolation(self, node):
        function = self.function
        current, value = function.value(self.start, node.expression)
        operands = [value]
        for operand in (
            ast.TextLiteral((node.expression_text or "").rstrip(), span=node.span),
            ast.TextLiteral(node.conversion, span=node.span)
            if node.conversion is not None
            else ast.NoneLiteral(None, span=node.span),
            node.format_spec
            if node.format_spec is not None
            else ast.TextLiteral("", span=node.span),
        ):
            current, value = function.value(current, operand)
            operands.append(value)
        ctran, interpolation = Ctran(), Lvar()
        function.protocol(
            current, ctran, interpolation, "make_interpolation", tuple(operands), node
        )
        return ctran, interpolation


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

    def visit_Match(self, node):
        start, next_, _ = self.frame
        self.expanded(
            start, next_, _MatchLowering(self.function, node.span).statements(node)
        )

    def visit_TypeAlias(self, node):
        """``type Name[...] = value``: a TypeAliasType evaluating its value
        lazily, inside an annotation scope holding its type parameters."""
        function = self.function
        start, next_, _ = self.frame
        span = node.span
        statements = _annotation_scope_body(node, span)
        create = ast.Call(
            ast.Attribute(
                ast.Attribute(
                    ast.Name("$typing", span=span), "TypeAliasType", span=span
                ),
                "_lazy",
                span=span,
            ),
            [
                ast.CallArgument(argument, span=span)
                for argument in (
                    ast.TextLiteral(node.name, span=span),
                    ast.Lambda([], node.value, span=span),
                    _type_parameter_tuple(node, span),
                    ast.Name("__name__", span=span),
                )
            ],
            span=span,
        )
        statements.append(ast.Return(create, span=span))
        scope = ast.FunctionDefinition(
            f"<generic parameters of {node.name}>", [], statements, span=span
        )
        lambda_ = function.converter.function(
            scope,
            function.lambda_.policy,
            parent=function,
            defining_class=function.class_scopes[-1] if function.class_scopes else None,
            annotation_scope=True,
        )
        ctran, created = Ctran(), Lvar()
        function.define_function(start, ctran, created, lambda_, scope, scope.name)
        following, result = Ctran(), Lvar()
        function.full_call(
            ctran,
            following,
            result,
            created,
            (),
            node,
            CallInfo((), function.converter.location(node)),
        )
        function.store(following, next_, node.name, result, node)

    def visit_Global(self, node):
        return None

    def visit_Nonlocal(self, node):
        return None

    def visit_Delete(self, node):
        function = self.function
        start, next_, _ = self.frame
        current = start
        for index, target in enumerate(node.targets):
            last = index == len(node.targets) - 1
            following = next_ if last else Ctran()
            target.accept(_DeleteTarget(function, current, following))
            current = following

    def visit_Import(self, node):
        function = self.function
        for item in node.items:
            function.converter.constant_binding(
                item.alias or item.name.split(".")[0], item
            )
        start, next_, _ = self.frame
        current = start
        for item in node.items:
            if item.name == "mule_types":
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
        for item in node.items:
            function.converter.constant_binding(item.alias or item.name, item)
        start, next_, _ = self.frame
        if node.level == 0 and node.module == "__future__":
            function._import_directive(node)
            return
        if node.level == 0 and node.module == "mule_types":
            directives = [item for item in node.items if item.name in _EXPORT_BINDINGS]
            runtime = [item for item in node.items if item.name not in _EXPORT_BINDINGS]
            if directives:
                function._import_directive(replace(node, items=directives))
            if not runtime:
                return
            node = replace(node, items=runtime)
        wildcard = any(item.name == "*" for item in node.items)
        if wildcard and (not function.is_module or function.class_scopes):
            function.error("import * only allowed at module level", node)
        if function.converter.imports is None:
            function.error(
                "runtime module imports are not supported by standalone lowering",
                node,
                unsupported=True,
            )
        imports = function.converter.imports
        module_name = imports.import_name(function.converter, node)
        if wildcard:
            function.component.mutates_module_namespace = True
            current, module = function.import_module(start, module_name, node)
            function.known(current, next_, None, fndb.IMPORT_STAR, (module,), node)
            return
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
            function.constant(
                failed.start, ctran, message, ast.NoneLiteral(None, span=node.span)
            )
        following = Ctran()
        function.known(
            ctran,
            following,
            None,
            fndb.ASSERTION_FAILURE,
            (message,),
            node,
            data=node.message is not None,
        )
        # The failure operation always branches to owned-reference cleanup.
        function.jump(following, passed)

    def visit_AnnotatedAssignment(self, node):
        start, next_, _ = self.frame
        node.target.accept(_AnnotationTarget(self.function, node, start, next_))

    def visit_ExpressionStatement(self, node):
        # A literal statement, such as a docstring, has no effect: Python's
        # compiler discards it, and converting it would build and release
        # the value on every execution.
        if node.expression.accept(_IS_LITERAL):
            return
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
        frontier = function.convert_body(linked(then.start, then, "start"), node.body)
        if frontier is not None:
            function.jump(frontier, join)
        frontier = function.convert_body(
            linked(other.start, other, "start"), node.else_body
        )
        if frontier is not None:
            function.jump(frontier, join)

    def visit_Raise(self, node):
        function = self.function
        start, _, _ = self.frame
        if node.exception is None:
            self.terminal(start, fndb.RERAISE, (), node)
            return
        ctran, value = function.value(start, node.exception)
        if node.cause is not None:
            # ``raise X from Y`` chains on the instance, so create it first.
            following, instance = Ctran(), Lvar()
            function.protocol(
                ctran, following, instance, "exception_instance", (value,), node
            )
            temporary = function.temporary()
            ctran = Ctran()
            function.set_var(following, ctran, temporary, instance, node)
            following, cause = function.value(ctran, node.cause)
            ctran, loaded = Ctran(), Lvar()
            function.ref(following, ctran, loaded, temporary, node)
            following = Ctran()
            function.protocol(
                ctran, following, None, "set_exception_cause", (loaded, cause), node
            )
            ctran, value = Ctran(), Lvar()
            function.ref(following, ctran, value, temporary, node, final=True)
        self.terminal(ctran, fndb.RAISE, (value,), node)

    def terminal(self, start, info, operands, node):
        """Emit a call that never returns; it ends its block with no successors."""
        self.function.known(start, None, None, info, operands, node)

    def visit_Try(self, node):
        function = self.function
        start, next_, _ = self.frame
        if node.finally_body:
            self.try_finally(start, next_, node)
            return
        if not node.handlers:
            frontier = function.convert_body(start, node.body)
            if frontier is not None:
                frontier = function.convert_body(frontier, node.else_body)
            if frontier is not None:
                function.flow(frontier, next_)
            return
        if node.exception_groups:
            self.try_except_star(start, next_, node)
            return
        self.try_handlers(start, next_, node)

    def try_finally(self, start, next_, node):
        """``try``/``finally``: the protected statement runs inside a FINALLY
        cleanup whose exits run the suite; its error dispatch runs the suite
        and re-raises unless the suite transfers control."""
        function = self.function
        outer = function.lexenv.cleanup
        dispatch, done = function.block(), function.block()
        region = Cleanup(
            CleanupKind.FINALLY,
            target=dispatch,
            parent=outer,
            info=node.finally_body,
            loops=function.lexenv.loops,
        )
        protected = ast.Try(
            node.body,
            node.handlers,
            node.else_body,
            [],
            exception_groups=node.exception_groups,
            span=node.span,
        )
        with function.with_cleanup(region):
            body = function.block()
            function.jump(start, body)
            following = Ctran()
            body_start = linked(body.start, body, "start")
            function.convert_statement(body_start, following, protected)
            if body_start.next is None:
                function.flow(body_start, following)
            if following.kind is not CtranKind.UNUSED:
                function.jump(following, done)
        ctran, caught = Ctran(), Lvar()
        function.known(dispatch.start, ctran, caught, fndb.CATCH_EXCEPTION, (), node)
        exception = function.temporary()
        current = Ctran()
        function.set_var(ctran, current, exception, caught, node)
        ctran, raised = Ctran(), Lvar()
        function.ref(current, ctran, raised, exception, node)
        entered = Ctran()
        function.known(ctran, entered, None, fndb.ENTER_HANDLER, (raised,), node)
        unwind, reraise = function.block(), function.block()
        active = Cleanup(
            CleanupKind.HANDLER_BODY,
            target=unwind,
            parent=outer,
            loops=function.lexenv.loops,
        )
        with function.with_cleanup(active):
            finally_block = function.block()
            function.jump(entered, finally_block)
            current = linked(finally_block.start, finally_block, "start")
            active.mess_up = current.next
            frontier = function.convert_body(current, node.finally_body)
            if frontier is not None:
                function.jump(frontier, reraise)
        ctran, value = Ctran(), Lvar()
        function.ref(reraise.start, ctran, value, exception, node, final=True)
        self.terminal(ctran, fndb.RESUME_EXCEPTION, (value,), node)
        ctran = Ctran()
        function.known(unwind.start, ctran, None, fndb.LEAVE_HANDLER, (), node)
        self.terminal(ctran, fndb.UNWIND, (), node)
        function.flow(linked(done.start, done, "start"), next_)

    def try_except_star(self, start, next_, node):
        function = self.function
        outer = function.lexenv.cleanup
        dispatch, success, done = function.block(), function.block(), function.block()
        region = Cleanup(CleanupKind.HANDLER_REGION, target=dispatch, parent=outer)
        with function.with_cleanup(region):
            body = function.block()
            function.jump(start, body)
            frontier = function.convert_body(
                linked(body.start, body, "start"), node.body
            )
            if frontier is not None:
                function.jump(frontier, success)
        frontier = function.convert_body(
            linked(success.start, success, "start"), node.else_body
        )
        if frontier is not None:
            function.jump(frontier, done)
        ctran, caught = Ctran(), Lvar()
        function.known(dispatch.start, ctran, caught, fndb.CATCH_EXCEPTION, (), node)
        following, initial_state = Ctran(), Lvar()
        function.protocol(
            ctran, following, initial_state, "except_star_start", (caught,), node
        )
        state = function.temporary()
        current = Ctran()
        function.set_var(following, current, state, initial_state, node)
        for handler in node.handlers:
            body, unmatched = function.block(), function.block()
            ctran, expected = function.value(current, handler.exception)
            following, state_val = Ctran(), Lvar()
            function.ref(ctran, following, state_val, state, handler)
            ctran, matches = Ctran(), Lvar()
            function.protocol(
                following,
                ctran,
                matches,
                "except_star_match",
                (state_val, expected),
                handler,
            )
            function.if_node(ctran, matches, body, unmatched, handler)
            ctran, state_val = Ctran(), Lvar()
            function.ref(body.start, ctran, state_val, state, handler)
            matched_ctran, matched_val = Ctran(), Lvar()
            function.protocol(
                ctran,
                matched_ctran,
                matched_val,
                "except_star_matched",
                (state_val,),
                handler,
            )
            entered = Ctran()
            function.known(
                matched_ctran,
                entered,
                None,
                fndb.ENTER_HANDLER,
                (matched_val,),
                handler,
            )
            unwind = function.block()
            active = Cleanup(
                CleanupKind.HANDLER_BODY,
                target=unwind,
                binding=handler.binding,
                parent=outer,
            )
            with function.with_cleanup(active):
                inner = function.block()
                function.jump(entered, inner)
                current = linked(inner.start, inner, "start")
                active.mess_up = current.next
                if handler.binding is not None:
                    function.converter.reserved(handler.binding, handler)
                    ctran, state_val = Ctran(), Lvar()
                    function.ref(current, ctran, state_val, state, handler)
                    matched_ctran, matched_val = Ctran(), Lvar()
                    function.protocol(
                        ctran,
                        matched_ctran,
                        matched_val,
                        "except_star_matched",
                        (state_val,),
                        handler,
                    )
                    current = Ctran()
                    function.store(
                        matched_ctran, current, handler.binding, matched_val, handler
                    )
                frontier = function.convert_body(current, handler.body)
                if frontier is not None:
                    function.jump(frontier, unmatched)
            current = linked(unwind.start, unwind, "start")
            ctran, handler_exc = Ctran(), Lvar()
            function.known(
                current, ctran, handler_exc, fndb.CATCH_EXCEPTION, (), handler
            )
            current = ctran
            if handler.binding is not None:
                ctran = Ctran()
                function.unbind(
                    current, ctran, handler.binding, handler, missing_ok=True
                )
                current = ctran
            ctran = Ctran()
            function.known(current, ctran, None, fndb.LEAVE_HANDLER, (), handler)
            following, state_val = Ctran(), Lvar()
            function.ref(ctran, following, state_val, state, handler)
            caught_ctran, discarded = Ctran(), Lvar()
            function.protocol(
                following,
                caught_ctran,
                discarded,
                "except_star_caught",
                (state_val, handler_exc),
                handler,
            )
            for use in discarded.uses:
                use.lvar = None
            discarded.uses = []
            function.jump(caught_ctran, unmatched)
            current = linked(unmatched.start, unmatched, "start")
        ctran, state_val = Ctran(), Lvar()
        function.ref(current, ctran, state_val, state, node, final=True)
        following, remaining = Ctran(), Lvar()
        function.protocol(
            ctran, following, remaining, "except_star_finish", (state_val,), node
        )
        unhandled = function.temporary()
        stored = Ctran()
        function.set_var(following, stored, unhandled, remaining, node)
        ctran, unhandled_val = Ctran(), Lvar()
        function.ref(stored, ctran, unhandled_val, unhandled, node)
        none_ctran, none_val = Ctran(), Lvar()
        function.constant(
            ctran, none_ctran, none_val, ast.NoneLiteral(None, span=node.span)
        )
        check_ctran, has_unhandled = Ctran(), Lvar()
        function.known(
            none_ctran,
            check_ctran,
            has_unhandled,
            fndb.COMPARE[ast.ComparisonOperator.IS_NOT],
            (unhandled_val, none_val),
            node,
        )
        reraise_block = function.block()
        function.if_node(check_ctran, has_unhandled, reraise_block, done, node)
        ctran, value = Ctran(), Lvar()
        function.ref(reraise_block.start, ctran, value, unhandled, node, final=True)
        self.terminal(ctran, fndb.RESUME_EXCEPTION, (value,), node)
        ctran = Ctran()
        function.set_var(done.start, ctran, state, None, node)
        end_ctran = Ctran()
        function.set_var(ctran, end_ctran, unhandled, None, node)
        function.flow(end_ctran, next_)

    def try_handlers(self, start, next_, node):
        function = self.function
        outer = function.lexenv.cleanup
        dispatch, success, done = function.block(), function.block(), function.block()
        region = Cleanup(CleanupKind.HANDLER_REGION, target=dispatch, parent=outer)
        with function.with_cleanup(region):
            # A handler covers only the body: its blocks start here.
            body = function.block()
            function.jump(start, body)
            frontier = function.convert_body(
                linked(body.start, body, "start"), node.body
            )
            if frontier is not None:
                function.jump(frontier, success)
        frontier = function.convert_body(
            linked(success.start, success, "start"), node.else_body
        )
        if frontier is not None:
            function.jump(frontier, done)
        ctran, caught = Ctran(), Lvar()
        function.known(dispatch.start, ctran, caught, fndb.CATCH_EXCEPTION, (), node)
        exception = function.temporary()
        current = Ctran()
        function.set_var(ctran, current, exception, caught, node)
        ctran, raised = Ctran(), Lvar()
        function.ref(current, ctran, raised, exception, node)
        entered = Ctran()
        function.known(ctran, entered, None, fndb.ENTER_HANDLER, (raised,), node)
        match_unwind, reraise = function.block(), function.block()
        unwinds = [
            function.block() if handler.binding is not None else None
            for handler in node.handlers
        ]
        match_active = Cleanup(
            CleanupKind.HANDLER_BODY,
            target=match_unwind,
            parent=outer,
            loops=function.lexenv.loops,
        )
        with function.with_cleanup(match_active):
            check_entry = function.block()
            function.jump(entered, check_entry)
            current = linked(check_entry.start, check_entry, "start")
            match_active.mess_up = current.next
            for handler, unwind in zip(node.handlers, unwinds, strict=True):
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
                if handler.binding is None:
                    frontier = function.convert_body(
                        linked(body.start, body, "start"), handler.body
                    )
                    if frontier is not None:
                        # An empty suite still needs an exit in the active cleanup,
                        # so environment analysis can emit the handler-leaving code.
                        function.jump(frontier, done)
                else:
                    active = Cleanup(
                        CleanupKind.HANDLER_BODY,
                        target=unwind,
                        binding=handler.binding,
                        parent=match_active,
                        loops=function.lexenv.loops,
                        info=False,
                    )
                    with function.with_cleanup(active):
                        # The handler body's own error destination starts a new block.
                        inner = function.block()
                        function.jump(linked(body.start, body, "start"), inner)
                        current = linked(inner.start, inner, "start")
                        active.mess_up = current.next
                        function.converter.reserved(handler.binding, handler)
                        ctran, raised = Ctran(), Lvar()
                        function.ref(current, ctran, raised, exception, handler)
                        current = Ctran()
                        function.store(ctran, current, handler.binding, raised, handler)
                        frontier = function.convert_body(current, handler.body)
                        if frontier is not None:
                            function.jump(frontier, done)
                current = linked(unmatched.start, unmatched, "start")
            function.jump(current, reraise)
        for handler, unwind in zip(node.handlers, unwinds, strict=True):
            if unwind is None:
                continue
            current = linked(unwind.start, unwind, "start")
            ctran = Ctran()
            function.unbind(current, ctran, handler.binding, handler, missing_ok=True)
            following = Ctran()
            function.known(ctran, following, None, fndb.LEAVE_HANDLER, (), handler)
            self.terminal(following, fndb.UNWIND, (), handler)
        # No handler matched: reinstall the original caught exception.
        ctran, value = Ctran(), Lvar()
        function.ref(reraise.start, ctran, value, exception, node, final=True)
        self.terminal(ctran, fndb.RESUME_EXCEPTION, (value,), node)
        ctran = Ctran()
        function.known(match_unwind.start, ctran, None, fndb.LEAVE_HANDLER, (), node)
        self.terminal(ctran, fndb.UNWIND, (), node)
        ctran = Ctran()
        function.set_var(done.start, ctran, exception, None, node)
        function.flow(ctran, next_)

    def visit_With(self, node):
        function = self.function
        start, next_, _ = self.frame
        if node.is_async:
            if (
                function.body_kind not in ("coroutine", "async_generator")
                or function.class_scopes
            ):
                function.error("'async with' outside async function", node)
            self.expanded(start, next_, _async_with(function, node))
            return
        self.with_items(start, next_, node, 0)

    def expanded(self, start, next_, statements):
        """Convert statements a construct expands into, in its place."""
        frontier = self.function.convert_body(start, statements)
        if frontier is not None:
            self.function.flow(frontier, next_)

    def with_items(self, start, next_, node, index):
        """Enter one ``with`` item; nested items wrap the body in turn."""
        function = self.function
        if index == len(node.items):
            frontier = function.convert_body(start, node.body)
            if frontier is not None:
                function.flow(frontier, next_)
            return
        item = node.items[index]
        outer = function.lexenv.cleanup
        manager, exit_method = function.temporary(), function.temporary()
        ctran, value = function.value(start, item.context)
        current = Ctran()
        function.set_var(ctran, current, manager, value, item)
        ctran, loaded = Ctran(), Lvar()
        function.ref(current, ctran, loaded, manager, item)
        following, bound_exit = Ctran(), Lvar()
        function.protocol(
            ctran,
            following,
            bound_exit,
            "context_exit_method",
            (loaded,),
            item,
        )
        current = Ctran()
        function.set_var(following, current, exit_method, bound_exit, item)
        ctran, loaded = Ctran(), Lvar()
        function.ref(current, ctran, loaded, manager, item)
        following, entered = Ctran(), Lvar()
        function.protocol(
            ctran,
            following,
            entered,
            "context_enter",
            (loaded,),
            item,
        )
        ctran = following
        if item.target is not None:
            current = Ctran()
            item.target.accept(_AssignmentTarget(function, entered, ctran, current))
        else:
            for use in entered.uses:
                use.lvar = None
            entered.uses = []
            current = ctran
        dispatch, done = function.block(), function.block()
        region = Cleanup(
            CleanupKind.WITH,
            target=dispatch,
            parent=outer,
            info=exit_method,
            loops=function.lexenv.loops,
        )
        with function.with_cleanup(region):
            body = function.block()
            function.jump(current, body)
            following = Ctran()
            body_start = linked(body.start, body, "start")
            self.with_items(body_start, following, node, index + 1)
            region.mess_up = body_start.next
            if body_start.next is None:
                function.flow(body_start, following)
            if following.kind is not CtranKind.UNUSED:
                function.jump(following, done)
        # An exception inside the body goes to __exit__; a true result suppresses it.
        ctran, caught = Ctran(), Lvar()
        function.known(dispatch.start, ctran, caught, fndb.CATCH_EXCEPTION, (), item)
        exception = function.temporary()
        following = Ctran()
        function.set_var(ctran, following, exception, caught, item)
        ctran, raised = Ctran(), Lvar()
        function.ref(following, ctran, raised, exception, item)
        entered = Ctran()
        function.known(ctran, entered, None, fndb.ENTER_HANDLER, (raised,), item)
        unwind = function.block()
        active = Cleanup(
            CleanupKind.HANDLER_BODY,
            target=unwind,
            parent=outer,
        )
        with function.with_cleanup(active):
            body = function.block()
            function.jump(entered, body)
            ctran, method = Ctran(), Lvar()
            function.ref(body.start, ctran, method, exit_method, item)
            following, raised = Ctran(), Lvar()
            function.ref(ctran, following, raised, exception, item)
            ctran, suppressed = Ctran(), Lvar()
            function.protocol(
                following, ctran, suppressed, "context_exit", (method, raised), item
            )
            reraise = function.block()
            function.if_node(ctran, suppressed, done, reraise, item)
            ctran, value = Ctran(), Lvar()
            function.ref(reraise.start, ctran, value, exception, item, final=True)
            self.terminal(ctran, fndb.RESUME_EXCEPTION, (value,), item)
        ctran = Ctran()
        function.known(unwind.start, ctran, None, fndb.LEAVE_HANDLER, (), item)
        self.terminal(ctran, fndb.UNWIND, (), item)
        function.flow(linked(done.start, done, "start"), next_)

    def visit_For(self, node):
        function = self.function
        start, next_, _ = self.frame
        if node.is_async:
            if (
                function.body_kind not in ("coroutine", "async_generator")
                or function.class_scopes
            ):
                function.error("'async for' outside async function", node)
            self.expanded(start, next_, _async_for(function, node))
            return
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
            state = RangeState(
                *_range_interval(candidate),
                tuple(arguments),
                step_sign=1 if len(arguments) <= 2 else 0,
            )
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
            frontier = function.convert_body(
                linked(range_else.start, range_else, "start"), node.else_body
            )
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
        failed, finished, else_block = (
            function.block(),
            function.block(),
            function.block(),
        )
        function.jump(linked(generic.start, generic, "start"), test)
        outer = function.lexenv.cleanup
        region = Cleanup(CleanupKind.HANDLER_REGION, target=exhausted, parent=outer)
        with function.with_cleanup(region):
            ctran, iterator_value = Ctran(), Lvar()
            function.ref(test.start, ctran, iterator_value, iterator, node)
            following, available = Ctran(), Lvar()
            function.known(
                ctran,
                following,
                available,
                fndb.SEQUENCE_AVAILABLE,
                (iterator_value,),
                node,
            )
            function.if_node(following, available, loop, else_block, node)
        ctran, dummy = Ctran(), Lvar()
        function.constant(loop.start, ctran, dummy, ast.NoneLiteral(None))
        following, item = Ctran(), Lvar()
        function.protocol(ctran, following, item, "next_item", (dummy,), node, iterator)
        assigned = Ctran()
        node.target.accept(_AssignmentTarget(function, item, following, assigned))
        with function.with_loop(LoopTarget(test, done, function.lexenv.cleanup)):
            frontier = function.convert_body(assigned, node.body)
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
        self.terminal(ctran, fndb.RESUME_EXCEPTION, (value,), node)
        ctran = Ctran()
        function.discard(finished.start, ctran, exception.name, node)
        function.jump(ctran, else_block)
        frontier = function.convert_body(
            linked(else_block.start, else_block, "start"), node.else_body
        )
        if frontier is not None:
            function.jump(frontier, done)
        function.jump(linked(done.start, done, "start"), range_done)
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
            frontier = function.convert_body(
                linked(loop.start, loop, "start"), node.body
            )
        if frontier is not None:
            function.jump(frontier, test)
        frontier = function.convert_body(
            linked(other.start, other, "start"), node.else_body
        )
        if frontier is not None:
            function.jump(frontier, done)

    def visit_Break(self, node):
        function = self.function
        start, _, _ = self.frame
        if not function.lexenv.loops:
            function.error("'break' outside loop", node)
        target = function.lexenv.loops[-1]
        function.jump(start, target.break_block)

    def visit_Continue(self, node):
        function = self.function
        start, _, _ = self.frame
        if not function.lexenv.loops:
            function.error("'continue' outside loop", node)
        target = function.lexenv.loops[-1]
        function.jump(start, target.continue_block)

    def visit_Return(self, node):
        function = self.function
        start, _, _ = self.frame
        if function.is_module:
            function.error("'return' outside function", node)
        lambda_ = function.lambda_
        return_ = linked(lambda_.return_, lambda_, "return")
        result = return_.result
        annotation = lambda_.signature.implementation_return_annotation
        ctran = Ctran()
        delivered = Lvar() if annotation is not None else result
        if node.value is None:
            function.constant(start, ctran, delivered, ast.NoneLiteral(None))
        else:
            function.convert(start, ctran, delivered, node.value)
        if annotation is not None:
            following = Ctran()
            function.cast(
                ctran,
                following,
                result,
                delivered,
                annotation,
                node,
                declaration=("return", lambda_),
            )
            ctran = following
        function.jump(ctran, return_.block)

    def visit_FunctionDefinition(self, node):
        function = self.function
        start, next_, _ = self.frame
        converter = function.converter
        converter.reserved(node.name, node)
        if node.type_parameters:
            self.generic_definition(node, self.generic_function(node))
            return
        scope = function.class_scopes[-1] if function.class_scopes else None
        if scope is None and function.annotation_scope:
            scope = function.defining_class
        decorators = converter.runtime_decorators(node.decorators)
        current, held = function.evaluate_decorators(start, decorators)
        lambda_ = converter.function(
            node,
            scope.policy if scope is not None else function.lambda_.policy,
            owner_class=scope.native_id if scope is not None else None,
            parent=function,
            defining_class=scope,
        )
        ctran, created = Ctran(), Lvar()
        function.define_function(current, ctran, created, lambda_, node, node.name)
        ctran, created = function.apply_decorators(ctran, held, created, decorators)
        function.store(ctran, next_, node.name, created, node)

    def visit_ClassDefinition(self, node):
        """Python's class statement: decorators and bases are evaluated, the
        body runs against a fresh namespace, and the class is built from that
        namespace (CPython's ``__build_class__``). SBCL's ``defclass`` likewise
        expands to a load-time ``%defclass`` call carrying the slot metadata;
        Mule additionally runs arbitrary body statements, inline in the
        enclosing function, with the class scope's name resolution."""
        function = self.function
        start, next_, _ = self.frame
        converter = function.converter
        converter.reserved(node.name, node)
        if node.type_parameters:
            self.generic_definition(node, self.generic_class(node))
            return
        outer = function.class_scopes[-1] if function.class_scopes else None
        policy = converter.policy(
            node.decorators,
            outer.policy if outer is not None else function.lambda_.policy,
        )
        positional = [
            argument
            for argument in node.arguments
            if argument.kind in (ast.ArgumentKind.POSITIONAL, ast.ArgumentKind.STARRED)
        ]
        keywords = [
            argument
            for argument in node.arguments
            if argument.kind
            in (ast.ArgumentKind.KEYWORD, ast.ArgumentKind.DOUBLE_STARRED)
        ]
        identity = converter.class_identity(node)
        native = _native_class_body(function, node, policy)
        body = node.body
        docstring = None
        if body and body[0].accept(_Docstring()):
            docstring, body = _documentation(body), body[1:]

        # Decorators, then the bases and keywords in source order, are
        # evaluated before the namespace is prepared and the body runs.
        decorators = converter.runtime_decorators(node.decorators)
        current, held = function.evaluate_decorators(start, decorators)
        span = node.span
        for argument in positional:
            if argument.kind == ast.ArgumentKind.STARRED:
                function.error(
                    "starred class bases are not supported yet",
                    argument,
                    unsupported=True,
                )
        bases, class_keywords = function.temporary(), function.temporary()
        ctran, value = function.value(
            current, ast.Tuple([argument.value for argument in positional], span=span)
        )
        current = Ctran()
        function.set_var(ctran, current, bases, value, node)
        if keywords:
            current = function.keyword_dict(current, class_keywords, keywords, node)
        else:
            ctran, value = Ctran(), Lvar()
            function.constant(current, ctran, value, ast.NoneLiteral(None, span=span))
            current = Ctran()
            function.set_var(ctran, current, class_keywords, value, node)

        namespace = function.temporary()
        operands = []
        for var in (bases, class_keywords):
            ctran, value = Ctran(), Lvar()
            function.ref(current, ctran, value, var, node)
            current = ctran
            operands.append(value)
        ctran, created = Ctran(), Lvar()
        function.known(
            current,
            ctran,
            created,
            fndb.CLASS_NAMESPACE,
            (*operands, ast.TextLiteral(node.name, span=span)),
            node,
        )
        current = Ctran()
        function.set_var(ctran, current, namespace, created, node)
        bindings = _ClassBindings()
        for statement in body:
            statement.accept(bindings)
        scope = ClassScope(
            function,
            namespace,
            identity,
            bindings.bound - bindings.globals - bindings.nonlocals,
            bindings.globals,
            policy,
            identity if native is not None else None,
            function.qualified(node.name),
        )
        # Only an actual leading docstring enters the class-body namespace.
        # type.__new__ supplies None later, after checking slot conflicts.
        initial = (
            ("__module__", ast.Name("__name__", span=node.span)),
            ("__qualname__", ast.TextLiteral(scope.qualname, span=node.span)),
        )
        if docstring is not None:
            initial += (("__doc__", docstring),)
        for name, expression in initial:
            if name == "__module__":
                ctran, value = Ctran(), Lvar()
                function.ref(
                    current, ctran, value, converter.global_var("__name__"), node
                )
            else:
                ctran, value = function.value(current, expression)
            current = Ctran()
            function.class_namespace_operation(
                ctran, current, None, fndb.CLASS_STORE, scope, name, (value,), node
            )

        fields = dict.fromkeys(native.fields if native is not None else ())
        # Names a literal __slots__ declares are instance fields, even when
        # only __new__ or code outside the class assigns them.
        for statement in body:
            fields.update(dict.fromkeys(statement.accept(_SlotNames())))
        for method in _ClassMethods.of(body):
            if not method.parameters:
                continue
            receiver = method.parameters[0]
            if receiver.kind == ast.ParameterKind.VAR_POSITIONAL:
                continue
            collector = _ReceiverFields(receiver.name, fields, declaration=native)
            for statement in method.body:
                statement.accept(collector)
        if native is not None:
            converter.class_fields[identity] = native.fields

        function.class_scopes.append(scope)
        try:
            frontier = function.convert_body(current, body)
        finally:
            function.class_scopes.pop()
        if frontier is None:
            return
        declarations = tuple(
            declaration
            for statement in body
            for declaration in statement.accept(_NamedAnnotations())
        )
        if declarations and "__annotations__" not in bindings.bound:
            if native is None and "__annotate__" not in bindings.bound:
                from .annotation_runtime import AnnotationThunk

                annotate = AnnotationThunk.from_declarations(
                    converter,
                    node.span,
                    declarations,
                    _RuntimeAnnotation(converter),
                ).build()
                if annotate is not None:
                    thunk = converter.function(
                        annotate,
                        policy,
                        parent=function,
                        defining_class=scope,
                        qualname=f"{scope.qualname}.__annotate__",
                        annotation_scope=True,
                    )
                    ctran, callback = Ctran(), Lvar()
                    function.define_function(
                        frontier, ctran, callback, thunk, annotate, "__annotate__"
                    )
                    frontier = Ctran()
                    function.class_namespace_operation(
                        ctran,
                        frontier,
                        None,
                        fndb.CLASS_STORE,
                        scope,
                        "__annotate__",
                        (callback,),
                        node,
                    )
            else:
                # Class annotations are deferred metadata (PEP 649): record
                # each annotation's source text in ``__annotations__`` so
                # runtime consumers such as dataclasses and annotationlib
                # can find the declared names without evaluating anything.
                entries = [
                    ast.DictEntry(
                        ast.TextLiteral(name, span=node.span),
                        ast.TextLiteral(
                            converter.source_text(annotation), span=annotation.span
                        ),
                        span=node.span,
                    )
                    for name, annotation in declarations
                ]
                ctran, annotations = function.value(
                    frontier, ast.Dict(entries, span=node.span)
                )
                frontier = Ctran()
                function.class_namespace_operation(
                    ctran,
                    frontier,
                    None,
                    fndb.CLASS_STORE,
                    scope,
                    "__annotations__",
                    (annotations,),
                    node,
                )
        operands = []
        for var in (bases, namespace, class_keywords):
            ctran, value = Ctran(), Lvar()
            function.ref(
                frontier,
                ctran,
                value,
                var,
                node,
                final=var is not namespace or not scope.annotation_namespace_used,
            )
            frontier = ctran
            operands.append(value)
        info = ClassInfo(
            node.name,
            len(positional),
            tuple(fields),
            tuple(contract for contract, declaration in native.fields.values())
            if native is not None
            else (),
            policy,
            converter.location(node),
            identity,
            identity if native is not None else None,
            declarations,
            bool(keywords or decorators),
        )
        following, created = Ctran(), Lvar()
        function.known(ctran, following, created, fndb.MAKE_CLASS, operands, node, info)
        if scope.annotation_namespace_used:
            klass = function.temporary()
            ctran = Ctran()
            function.set_var(following, ctran, klass, created, node)
            following, value = Ctran(), Lvar()
            function.ref(ctran, following, value, klass, node)
            ctran, live = Ctran(), Lvar()
            function.known(
                following,
                ctran,
                live,
                fndb.GETATTR,
                (value,),
                node,
                AttributeInfo("__dict__", converter.location(node)),
            )
            following = Ctran()
            function.set_var(ctran, following, namespace, live, node)
            ctran, created = Ctran(), Lvar()
            function.ref(following, ctran, created, klass, node, final=True)
            following = ctran
        if scope.cell is not None:
            # Methods using super() or __class__ see the finished class.
            klass = function.temporary()
            ctran = Ctran()
            function.set_var(following, ctran, klass, created, node)
            following, value = Ctran(), Lvar()
            function.ref(ctran, following, value, klass, node)
            ctran = Ctran()
            function.set_var(following, ctran, scope.cell, value, node)
            following, created = Ctran(), Lvar()
            function.ref(ctran, following, created, klass, node, final=True)
        ctran, created = function.apply_decorators(following, held, created, decorators)
        function.store(ctran, next_, node.name, created, node)

    def generic_definition(self, node, definition):
        """A PEP 695 generic function or class. As in CPython, a hidden
        annotation scope creates the type parameters, runs ``definition``
        (which defines the function or class and records them as
        ``__type_params__``), and returns it; decorators apply outside."""
        function = self.function
        start, next_, _ = self.frame
        converter = function.converter
        span = node.span
        statements = [
            *_annotation_scope_body(node, span),
            *definition,
            ast.Return(ast.Name(node.name, span=span), span=span),
        ]
        scope = ast.FunctionDefinition(
            f"<generic parameters of {node.name}>", [], statements, span=span
        )
        decorators = converter.runtime_decorators(node.decorators)
        current, held = function.evaluate_decorators(start, decorators)
        outer = (
            function.class_scopes[-1]
            if function.class_scopes
            else function.defining_class
        )
        lambda_ = converter.function(
            scope,
            function.lambda_.policy,
            parent=function,
            defining_class=outer,
            annotation_scope=True,
        )
        ctran, created = Ctran(), Lvar()
        function.define_function(current, ctran, created, lambda_, scope, scope.name)
        following, result = Ctran(), Lvar()
        function.full_call(
            ctran,
            following,
            result,
            created,
            (),
            node,
            CallInfo((), converter.location(node)),
        )
        ctran, result = function.apply_decorators(following, held, result, decorators)
        function.store(ctran, next_, node.name, result, node)

    def generic_function(self, node):
        """The generic function's definition inside its annotation scope."""
        span = node.span
        return [
            replace(node, type_parameters=[], decorators=[]),
            ast.Assignment(
                [
                    ast.Attribute(
                        ast.Name(node.name, span=span), "__type_params__", span=span
                    )
                ],
                _type_parameter_tuple(node, span),
                span=span,
            ),
        ]

    def generic_class(self, node):
        """The generic class's definition inside its annotation scope; the
        class also derives from ``Generic[...]``."""
        span = node.span
        positional = [
            argument
            for argument in node.arguments
            if argument.kind in (ast.ArgumentKind.POSITIONAL, ast.ArgumentKind.STARRED)
        ]
        keywords = [
            argument for argument in node.arguments if argument not in positional
        ]
        generic = ast.CallArgument(
            ast.Subscript(
                ast.Attribute(ast.Name("$typing", span=span), "Generic", span=span),
                _type_parameter_tuple(node, span, unpack=True),
                span=span,
            ),
            span=span,
        )
        body = list(node.body)
        offset = 1 if body and body[0].accept(_Docstring()) else 0
        body.insert(
            offset,
            ast.Assignment(
                [ast.Name("__type_params__", span=span)],
                _type_parameter_tuple(node, span),
                span=span,
            ),
        )
        return [
            replace(
                node,
                type_parameters=[],
                decorators=[],
                arguments=[*positional, generic, *keywords],
                body=body,
            )
        ]


class _MatchLowering:
    """A ``match`` statement as tests and assignments over hidden variables,
    the decision sequence CPython's compiler emits. Each pattern sets a flag
    and binds its names as it matches; later cases run only when earlier
    ones fail. The ``_match`` module supplies the sequence, mapping and class
    checks CPython performs natively."""

    def __init__(self, function, span):
        self.function = function
        self.span = span
        self.helpers = None

    def temporary(self):
        return self.function.temporary().name

    def name(self, identifier):
        return ast.Name(identifier, span=self.span)

    def assign(self, target, value):
        return ast.Assignment([self.name(target)], value, span=self.span)

    def constant(self, value):
        return ast.BooleanLiteral(value, span=self.span)

    def call(self, helper, *arguments):
        if self.helpers is None:
            self.helpers = self.temporary()
        return ast.Call(
            ast.Attribute(self.name(self.helpers), helper, span=self.span),
            [ast.CallArgument(argument, span=self.span) for argument in arguments],
            span=self.span,
        )

    def compare(self, left, operator, right):
        return ast.Comparison(left, [operator], [right], span=self.span)

    def when(self, flag, body):
        return ast.If(self.name(flag), body, span=self.span)

    def index(self, sequence, position):
        return ast.Subscript(
            self.name(sequence),
            ast.IntegerLiteral(position, span=self.span),
            span=self.span,
        )

    def unshadowed_global(self, name: str) -> bool:
        fn = self.function
        return (
            fn.class_binding(name) is None
            and fn.class_reader(name) is None
            and fn.variable(name).accept(_IS_GLOBAL_VAR)
        )

    def unshadowed_builtin(self, name: str) -> bool:
        shadows = self.function.converter.module_annotation_shadows
        return (
            self.function.lambda_.policy.safety < 3
            and "*" not in shadows
            and name not in shadows
            and self.unshadowed_global(name)
        )

    def statements(self, node):
        elements = node.subject.accept(_UNPACK_FREE_SEQUENCE_SUBJECT)
        if elements is not None and all(
            case.pattern.accept(_FixedTuplePattern(len(elements)))
            for case in node.cases
        ):
            subject_items = [self.temporary() for _ in elements]
            result = [
                *[
                    self.assign(target, element)
                    for target, element in zip(subject_items, elements, strict=True)
                ],
                *self.tuple_cases(node.cases, 0, subject_items),
            ]
        else:
            subject = self.temporary()
            result = [
                self.assign(subject, node.subject),
                *self.cases(node.cases, 0, subject),
            ]
        if self.helpers is not None:
            result.insert(
                0,
                ast.Import(
                    [ast.ImportItem("_match", self.helpers, span=self.span)],
                    span=self.span,
                ),
            )
        return result

    def tuple_cases(self, cases, position, subject_items):
        if position == len(cases):
            return []
        case = cases[position]
        flag = self.temporary()
        test = self.name(flag)
        if case.guard is not None:
            test = ast.Boolean(
                ast.BooleanOperator.AND, [test, case.guard], span=self.span
            )
        return [
            *self.tuple_pattern(case.pattern, subject_items, flag),
            ast.If(
                test,
                list(case.body) or [ast.Pass(span=self.span)],
                self.tuple_cases(cases, position + 1, subject_items),
                span=self.span,
            ),
        ]

    def tuple_pattern(self, pattern, subject_items, flag):
        return pattern.accept(_TuplePatternTest(self, subject_items, flag))

    def cases(self, cases, position, subject):
        if position == len(cases):
            return []
        case = cases[position]
        flag = self.temporary()
        test = self.name(flag)
        if case.guard is not None:
            test = ast.Boolean(
                ast.BooleanOperator.AND, [test, case.guard], span=self.span
            )
        return [
            *self.pattern(case.pattern, subject, flag),
            ast.If(
                test,
                list(case.body) or [ast.Pass(span=self.span)],
                self.cases(cases, position + 1, subject),
                span=self.span,
            ),
        ]

    def pattern(self, pattern, subject, flag):
        """Statements leaving ``flag`` true when ``pattern`` matches the
        value in the variable ``subject``."""
        return pattern.accept(_PatternTest(self, subject, flag))

    def direct_items(self, expressions, patterns, flag, *, flag_is_true: bool = False):
        """Match each pattern against the corresponding expression in order,
        stopping at the first failure without building an intermediate sequence."""
        result = []
        for expr, pattern in reversed(list(zip(expressions, patterns, strict=True))):
            is_capture, capture_name = pattern.accept(_CAPTURE_PATTERN_INFO)
            if is_capture:
                bind = [] if capture_name is None else [self.assign(capture_name, expr)]
                if result:
                    result = [*bind, *result]
                elif flag_is_true:
                    result = bind
                else:
                    result = [*bind, self.assign(flag, self.constant(True))]
            else:
                item = self.temporary()
                result = [
                    self.assign(item, expr),
                    *self.pattern(pattern, item, flag),
                    *([self.when(flag, result)] if result else []),
                ]
        return result

    def items(self, values, patterns, flag, positions):
        """Match each pattern against the item at its position, in order,
        stopping at the first failure."""
        return self.direct_items(
            [self.index(values, position) for position in positions],
            patterns,
            flag,
        )

    def sequence(self, pattern, subject, flag):
        values = self.temporary()
        elements = pattern.elements
        stars = _star_positions(elements)
        has_named_star = any(element.accept(_HAS_NAMED_STAR) for element in elements)
        helper_name = "sequence_list" if has_named_star else "sequence"
        if self.unshadowed_builtin("len"):
            length = ast.Call(
                self.name("len"),
                [ast.CallArgument(self.name(values), span=self.span)],
                span=self.span,
            )
        else:
            length = ast.Call(
                ast.Attribute(self.name(values), "__len__", span=self.span),
                span=self.span,
            )
        count = ast.IntegerLiteral(len(elements) - len(stars), span=self.span)
        size_check = self.compare(
            length,
            ast.ComparisonOperator.GREATER_EQUAL
            if stars
            else ast.ComparisonOperator.EQUAL,
            count,
        )
        split = _SequenceElements(self, values, elements, stars)
        patterns, positions, captures = split.patterns, split.positions, split.captures
        matched = [
            *captures,
            self.assign(flag, self.constant(True)),
            *self.direct_items(
                [self.index(values, position) for position in positions],
                patterns,
                flag,
                flag_is_true=True,
            ),
        ]
        return [
            self.assign(values, self.call(helper_name, self.name(subject))),
            self.assign(
                flag,
                self.compare(
                    self.name(values),
                    ast.ComparisonOperator.IS_NOT,
                    ast.NoneLiteral(None, span=self.span),
                ),
            ),
            self.when(flag, [self.assign(flag, size_check), self.when(flag, matched)]),
        ]

    def mapping(self, pattern, subject, flag):
        seen_literals = []
        key_visitor = _MappingLiteralKey()
        for entry in pattern.entries:
            is_literal, literal_value = entry.key.accept(key_visitor)
            if is_literal:
                if literal_value in seen_literals:
                    self.function.error(
                        f"mapping pattern checks duplicate key ({literal_value!r})",
                        entry.key,
                    )
                seen_literals.append(literal_value)
        values = self.temporary()
        keys = ast.Tuple([entry.key for entry in pattern.entries], span=self.span)
        rest = []
        if pattern.rest is not None:
            rest = [
                self.assign(
                    pattern.rest, self.call("mapping_rest", self.name(subject), keys)
                )
            ]
        inner = [
            self.assign(values, self.call("mapping_values", self.name(subject), keys)),
            self.assign(
                flag,
                self.compare(
                    self.name(values),
                    ast.ComparisonOperator.IS_NOT,
                    ast.NoneLiteral(None, span=self.span),
                ),
            ),
            self.when(
                flag,
                [
                    *self.items(
                        values,
                        [entry.pattern for entry in pattern.entries],
                        flag,
                        list(range(len(pattern.entries))),
                    ),
                    *([self.when(flag, rest)] if rest else []),
                ]
                if pattern.entries
                else rest or [ast.Pass(span=self.span)],
            ),
        ]
        return [
            self.assign(flag, self.call("mapping", self.name(subject))),
            self.when(flag, inner),
        ]

    def class_pattern(self, pattern, subject, flag):
        cls_name = pattern.class_name.accept(_BASE_CLASS_NAME)
        match_info = (
            self.function.converter.declared_class_match_info.get(cls_name)
            if cls_name is not None
            else None
        )
        if (
            match_info is not None
            and self.unshadowed_global(cls_name)
            and self.unshadowed_builtin("isinstance")
        ):
            match_args, guaranteed_fields = match_info
            pos_count = len(pattern.positional)
            if (match_args is not None or pos_count == 0) and (
                pos_count <= len(match_args or ())
            ):
                field_names = [
                    *(match_args or ())[:pos_count],
                    *(keyword.name for keyword in pattern.keywords),
                ]
                if len(set(field_names)) == len(field_names) and all(
                    field in guaranteed_fields for field in field_names
                ):
                    patterns = [
                        *pattern.positional,
                        *(keyword.pattern for keyword in pattern.keywords),
                    ]
                    active_pairs = [
                        (field, subpattern)
                        for field, subpattern in zip(field_names, patterns, strict=True)
                        if subpattern.accept(_CAPTURE_PATTERN_INFO) != (True, None)
                    ]
                    isinstance_call = ast.Call(
                        self.name("isinstance"),
                        [
                            ast.CallArgument(self.name(subject), span=self.span),
                            ast.CallArgument(
                                ast.Tuple([pattern.class_name], span=self.span),
                                span=self.span,
                            ),
                        ],
                        span=self.span,
                    )
                    sub_stmts = (
                        self.direct_items(
                            [
                                ast.Attribute(self.name(subject), field, span=self.span)
                                for field, _ in active_pairs
                            ],
                            [subpattern for _, subpattern in active_pairs],
                            flag,
                            flag_is_true=True,
                        )
                        if active_pairs
                        else []
                    )
                    return [
                        self.assign(flag, isinstance_call),
                        *([self.when(flag, sub_stmts)] if sub_stmts else []),
                    ]
        values = self.temporary()
        names = ast.Tuple(
            [
                ast.TextLiteral(keyword.name, span=self.span)
                for keyword in pattern.keywords
            ],
            span=self.span,
        )
        patterns = [
            *pattern.positional,
            *(keyword.pattern for keyword in pattern.keywords),
        ]
        return [
            self.assign(
                values,
                self.call(
                    "class_attributes",
                    self.name(subject),
                    pattern.class_name,
                    ast.IntegerLiteral(len(pattern.positional), span=self.span),
                    names,
                ),
            ),
            self.assign(
                flag,
                self.compare(
                    self.name(values),
                    ast.ComparisonOperator.IS_NOT,
                    ast.NoneLiteral(None, span=self.span),
                ),
            ),
            *(
                [
                    self.when(
                        flag,
                        self.items(values, patterns, flag, list(range(len(patterns)))),
                    )
                ]
                if patterns
                else []
            ),
        ]


class _MappingLiteralKey(ast.Visitor):
    """Extracts a compile-time literal key from a mapping pattern entry."""

    def visit_Node(self, node):
        return (False, None)

    def visit_NoneLiteral(self, node):
        return (True, None)

    def visit_BooleanLiteral(self, node):
        return (True, node.value)

    def visit_IntegerLiteral(self, node):
        return (True, node.value)

    def visit_FloatLiteral(self, node):
        return (True, node.value)

    def visit_ComplexLiteral(self, node):
        return (True, node.value)

    def visit_TextLiteral(self, node):
        return (True, node.value)

    def visit_BytesLiteral(self, node):
        return (True, node.value)

    def visit_Unary(self, node):
        if node.operator is ast.UnaryOperator.NEGATIVE:
            is_literal, value = node.operand.accept(self)
            if is_literal:
                return (True, -value)
        return (False, None)

    def visit_Binary(self, node):
        left_literal, left_value = node.left.accept(self)
        right_literal, right_value = node.right.accept(self)
        if left_literal and right_literal:
            if node.operator is ast.BinaryOperator.ADD:
                return (True, left_value + right_value)
            if node.operator is ast.BinaryOperator.SUB:
                return (True, left_value - right_value)
        return (False, None)


class _PatternTest(ast.Visitor):
    """One pattern's statements for ``_MatchLowering.pattern``."""

    def __init__(self, lowering: _MatchLowering, subject, flag):
        self.lowering = lowering
        self.subject = subject
        self.flag = flag

    def visit_Node(self, node):
        self.lowering.function.error("unsupported pattern", node, unsupported=True)

    def visit_CapturePattern(self, node):
        lowering, flag = self.lowering, self.flag
        bind = (
            []
            if node.name is None
            else [lowering.assign(node.name, lowering.name(self.subject))]
        )
        return [*bind, lowering.assign(flag, lowering.constant(True))]

    def visit_LiteralPattern(self, node):
        lowering = self.lowering
        operator = node.value.literal_pattern_operator()
        return [
            lowering.assign(
                self.flag,
                lowering.compare(lowering.name(self.subject), operator, node.value),
            )
        ]

    def visit_ValuePattern(self, node):
        lowering = self.lowering
        return [
            lowering.assign(
                self.flag,
                lowering.compare(
                    lowering.name(self.subject),
                    ast.ComparisonOperator.EQUAL,
                    node.value,
                ),
            )
        ]

    def visit_AsPattern(self, node):
        lowering, subject, flag = self.lowering, self.subject, self.flag
        return [
            *lowering.pattern(node.pattern, subject, flag),
            lowering.when(flag, [lowering.assign(node.name, lowering.name(subject))]),
        ]

    def visit_OrPattern(self, node):
        lowering, subject, flag = self.lowering, self.subject, self.flag
        result = list(lowering.pattern(node.alternatives[0], subject, flag))
        for alternative in node.alternatives[1:]:
            result.append(
                ast.If(
                    ast.Unary(
                        ast.UnaryOperator.NOT, lowering.name(flag), span=lowering.span
                    ),
                    lowering.pattern(alternative, subject, flag),
                    span=lowering.span,
                )
            )
        return result

    def visit_SequencePattern(self, node):
        return self.lowering.sequence(node, self.subject, self.flag)

    def visit_MappingPattern(self, node):
        return self.lowering.mapping(node, self.subject, self.flag)

    def visit_ClassPattern(self, node):
        return self.lowering.class_pattern(node, self.subject, self.flag)


class _CapturePatternInfo(ast.Visitor):
    def visit_Node(self, node):
        return (False, None)

    def visit_CapturePattern(self, node: ast.CapturePattern):
        return (True, node.name)


_CAPTURE_PATTERN_INFO = _CapturePatternInfo()


class _HasNamedStar(ast.Visitor):
    def visit_Node(self, node):
        return False

    def visit_StarPattern(self, node: ast.StarPattern):
        return node.name is not None


_HAS_NAMED_STAR = _HasNamedStar()


class _IsUnpackNode(ast.Visitor):
    def visit_Node(self, node):
        return False

    def visit_Unpack(self, node: ast.Unpack):
        return True


_IS_UNPACK_NODE = _IsUnpackNode()


class _UnpackFreeSequenceSubject(ast.Visitor):
    def visit_Node(self, node):
        return None

    def visit_Tuple(self, node: ast.Tuple):
        if any(element.accept(_IS_UNPACK_NODE) for element in node.elements):
            return None
        return list(node.elements)

    def visit_List(self, node: ast.List):
        if any(element.accept(_IS_UNPACK_NODE) for element in node.elements):
            return None
        return list(node.elements)


_UNPACK_FREE_SEQUENCE_SUBJECT = _UnpackFreeSequenceSubject()


class _FixedTuplePattern(ast.Visitor):
    def __init__(self, arity: int):
        self.arity = arity

    def visit_Node(self, node):
        return False

    def visit_CapturePattern(self, node: ast.CapturePattern):
        return node.name is None

    def visit_OrPattern(self, node: ast.OrPattern):
        return all(alt.accept(self) for alt in node.alternatives)

    def visit_SequencePattern(self, node: ast.SequencePattern):
        return len(node.elements) == self.arity and not _star_positions(node.elements)


class _TuplePatternTest(ast.Visitor):
    """Matches a fixed-arity sequence pattern directly against pre-evaluated
    tuple/list subject item temporaries."""

    def __init__(self, lowering: _MatchLowering, subject_items: list[str], flag: str):
        self.lowering = lowering
        self.subject_items = subject_items
        self.flag = flag

    def visit_Node(self, node):
        self.lowering.function.error("unsupported pattern", node, unsupported=True)

    def visit_CapturePattern(self, node: ast.CapturePattern):
        lowering = self.lowering
        return [lowering.assign(self.flag, lowering.constant(True))]

    def visit_OrPattern(self, node: ast.OrPattern):
        lowering, flag = self.lowering, self.flag
        result = list(node.alternatives[0].accept(self))
        for alternative in node.alternatives[1:]:
            result.append(
                ast.If(
                    ast.Unary(
                        ast.UnaryOperator.NOT, lowering.name(flag), span=lowering.span
                    ),
                    alternative.accept(self),
                    span=lowering.span,
                )
            )
        return result

    def visit_SequencePattern(self, node: ast.SequencePattern):
        lowering, flag = self.lowering, self.flag
        sub_stmts = lowering.direct_items(
            [lowering.name(item) for item in self.subject_items],
            node.elements,
            flag,
            flag_is_true=True,
        )
        return [
            lowering.assign(flag, lowering.constant(True)),
            *sub_stmts,
        ]


class _StarPositions(ast.Visitor):
    """Collects the indices of starred elements (``*target`` or ``*pattern``)."""

    def __init__(self):
        self.index = 0
        self.positions: list[int] = []

    def visit_Unpack(self, node):
        self.positions.append(self.index)

    def visit_StarPattern(self, node):
        self.positions.append(self.index)


def _star_positions(elements) -> list[int]:
    collector = _StarPositions()
    for index, element in enumerate(elements):
        collector.index = index
        element.accept(collector)
    return collector.positions


class _SequenceElements(ast.Visitor):
    """Splits a sequence pattern into the subpatterns matched by position
    (negative after the star pattern) and the star pattern's capture."""

    def __init__(self, lowering: _MatchLowering, values, elements, stars):
        self.lowering = lowering
        self.values = values
        self.count = len(elements)
        self.stars = stars
        self.patterns = []
        self.positions = []
        self.captures = []
        self.index = 0
        for index, element in enumerate(elements):
            self.index = index
            element.accept(self)

    def visit_Node(self, node):
        index, stars = self.index, self.stars
        self.patterns.append(node)
        self.positions.append(
            index if not stars or index < stars[0] else index - self.count
        )

    def visit_StarPattern(self, node):
        if node.name is None:
            return
        lowering, index = self.lowering, self.index
        span = lowering.span
        after = self.count - index - 1
        upper = (
            ast.Unary(
                ast.UnaryOperator.NEGATIVE,
                ast.IntegerLiteral(after, span=span),
                span=span,
            )
            if after
            else None
        )
        self.captures.append(
            lowering.assign(
                node.name,
                ast.Subscript(
                    lowering.name(self.values),
                    ast.Slice(ast.IntegerLiteral(index, span=span), upper, span=span),
                    span=span,
                ),
            )
        )


def _async_with(function, node):
    """``async with``, expanded as CPython executes it: await ``__aenter__``,
    run the body, and await ``__aexit__`` with the exception that ends the
    body (suppressing it when the result is true) or with Nones otherwise."""
    span = node.span
    item, rest = node.items[0], node.items[1:]
    body = list(node.body) if not rest else [replace(node, items=rest)]
    manager, enter_, exit_, value, error, handled = (
        function.temporary().name for _ in range(6)
    )

    def name(identifier):
        return ast.Name(identifier, span=span)

    def call(callee, *arguments):
        return ast.Call(
            callee,
            [ast.CallArgument(argument, span=span) for argument in arguments],
            span=span,
        )

    def assign(target, value):
        return ast.Assignment([target], value, span=span)

    none = ast.NoneLiteral(None, span=span)
    if item.target is not None:
        body.insert(0, assign(item.target, name(value)))
    exit_with_error = ast.Await(
        call(
            name(exit_),
            ast.Attribute(name(error), "__class__", span=span),
            name(error),
            ast.Attribute(name(error), "__traceback__", span=span),
        ),
        span=span,
    )
    return [
        assign(name(manager), item.context),
        ast.Try(
            [
                assign(
                    name(exit_), ast.Attribute(name(manager), "__aexit__", span=span)
                ),
                assign(
                    name(enter_), ast.Attribute(name(manager), "__aenter__", span=span)
                ),
            ],
            [
                ast.ExceptionHandler(
                    name("AttributeError"),
                    None,
                    [
                        ast.Raise(
                            call(
                                name("TypeError"),
                                ast.TextLiteral(
                                    "object does not support the asynchronous context manager protocol",
                                    span=span,
                                ),
                            ),
                            span=span,
                        )
                    ],
                    span=span,
                )
            ],
            span=span,
        ),
        assign(
            name(value),
            ast.Await(call(name(enter_)), span=span),
        ),
        assign(name(handled), ast.BooleanLiteral(False, span=span)),
        ast.Try(
            body,
            [
                ast.ExceptionHandler(
                    name("BaseException"),
                    error,
                    [
                        assign(name(handled), ast.BooleanLiteral(True, span=span)),
                        ast.If(
                            ast.Unary(
                                ast.UnaryOperator.NOT, exit_with_error, span=span
                            ),
                            [ast.Raise(span=span)],
                            span=span,
                        ),
                    ],
                    span=span,
                )
            ],
            finally_body=[
                ast.If(
                    ast.Unary(ast.UnaryOperator.NOT, name(handled), span=span),
                    [
                        ast.ExpressionStatement(
                            ast.Await(call(name(exit_), none, none, none), span=span),
                            span=span,
                        )
                    ],
                    span=span,
                )
            ],
            span=span,
        ),
    ]


def _async_for(function, node):
    """``async for``, expanded: await ``__anext__`` until it raises
    StopAsyncIteration, then run the ``else`` clause unless the body broke out."""
    span = node.span
    iterable, aiter_, iterator, anext_, value, exhausted = (
        function.temporary().name for _ in range(6)
    )

    def name(identifier):
        return ast.Name(identifier, span=span)

    def assign(target, value):
        return ast.Assignment([target], value, span=span)

    def type_error(message):
        return ast.Raise(
            ast.Call(
                name("TypeError"),
                [ast.CallArgument(ast.TextLiteral(message, span=span), span=span)],
                span=span,
            ),
            span=span,
        )

    loop = ast.While(
        ast.BooleanLiteral(True, span=span),
        [
            ast.Try(
                [
                    assign(
                        name(anext_),
                        ast.Attribute(name(iterator), "__anext__", span=span),
                    )
                ],
                [
                    ast.ExceptionHandler(
                        name("AttributeError"),
                        None,
                        [
                            type_error(
                                "'async for' received an object from __aiter__ that does not implement __anext__"
                            )
                        ],
                        span=span,
                    )
                ],
                span=span,
            ),
            ast.Try(
                [
                    assign(
                        name(value),
                        ast.Await(
                            ast.Call(name(anext_), span=span),
                            span=span,
                        ),
                    )
                ],
                [
                    ast.ExceptionHandler(
                        name("StopAsyncIteration"),
                        None,
                        [
                            assign(
                                name(exhausted), ast.BooleanLiteral(True, span=span)
                            ),
                            ast.Break(span=span),
                        ],
                        span=span,
                    )
                ],
                span=span,
            ),
            assign(node.target, name(value)),
            *node.body,
        ],
        span=span,
    )
    statements = [
        assign(name(iterable), node.iterable),
        ast.Try(
            [
                assign(
                    name(aiter_),
                    ast.Attribute(name(iterable), "__aiter__", span=span),
                )
            ],
            [
                ast.ExceptionHandler(
                    name("AttributeError"),
                    None,
                    [
                        type_error(
                            "'async for' requires an object with __aiter__ method"
                        )
                    ],
                    span=span,
                )
            ],
            span=span,
        ),
        assign(
            name(iterator),
            ast.Call(name(aiter_), span=span),
        ),
        assign(name(exhausted), ast.BooleanLiteral(False, span=span)),
        loop,
    ]
    if node.else_body:
        statements.append(ast.If(name(exhausted), list(node.else_body), span=span))
    return statements


_TYPE_PARAMETER_KINDS = {
    ast.TypeParameterKind.TYPE_VAR: "TypeVar",
    ast.TypeParameterKind.PARAM_SPEC: "ParamSpec",
    ast.TypeParameterKind.TYPE_VAR_TUPLE: "TypeVarTuple",
}


def _annotation_scope_body(node, span):
    """Statements creating ``node``'s type parameters, each lazily evaluating
    its bound, constraints and default, and the names they bind."""
    statements = [
        ast.Import([ast.ImportItem("_typing", "$typing", span=span)], span=span)
    ]

    def lazy(expression):
        if expression is None:
            return ast.NoneLiteral(None, span=span)
        from .annotation_runtime import _ANNOTATION_VALUE

        return ast.Lambda([], expression.accept(_ANNOTATION_VALUE), span=span)

    for parameter in node.type_parameters:
        constraints = (
            ast.Tuple(list(parameter.constraints), span=span)
            if parameter.constraints
            else None
        )
        create = ast.Call(
            ast.Attribute(ast.Name("$typing", span=span), "_type_parameter", span=span),
            [
                ast.CallArgument(argument, span=span)
                for argument in (
                    ast.TextLiteral(_TYPE_PARAMETER_KINDS[parameter.kind], span=span),
                    ast.TextLiteral(parameter.name, span=span),
                    lazy(parameter.bound),
                    lazy(constraints),
                    lazy(parameter.default),
                )
            ],
            span=span,
        )
        statements.append(
            ast.Assignment([ast.Name(parameter.name, span=span)], create, span=span)
        )
    return statements


def _type_parameter_tuple(node, span, *, unpack=False):
    return ast.Tuple(
        [
            ast.Unpack(ast.Name(parameter.name, span=span), span=span)
            if unpack and parameter.kind == ast.TypeParameterKind.TYPE_VAR_TUPLE
            else ast.Name(parameter.name, span=span)
            for parameter in node.type_parameters
        ],
        span=span,
    )


def _native_class_body(function, node, policy):
    """The validated body of a typed native class, or None for an ordinary
    class. A class requests the native layout with ``optimize(speed=3)`` and
    annotated declarations; its body is then restricted to a docstring,
    methods, bare ``mule_types.int64`` declarations and ``pass``."""
    requested = (
        policy.speed == 3
        and any(decorator.accept(_NativeClassOptIn()) for decorator in node.decorators)
        and any(statement.accept(_AnnotatedDeclaration()) for statement in node.body)
    )
    if not requested:
        return None
    body = _ClassBody(function)
    for index, statement in enumerate(node.body):
        if index == 0 and statement.accept(_Docstring()):
            continue
        statement.accept(body)
    if not body.fields or not body.native_eligible:
        return None
    if node.arguments:
        function.error("typed native classes cannot have bases", node, unsupported=True)
    if not any(method.name == "__init__" for method in body.methods):
        function.error("typed native classes require __init__", node, unsupported=True)
    for method in body.methods:
        if not method.parameters:
            function.error("instance methods require a receiver parameter", method)
        if function.converter.runtime_decorators(method.decorators):
            function.error(
                "typed native class methods cannot have runtime decorators",
                method,
                unsupported=True,
            )
        body.validate_method(method)
    return body


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
        starred = _star_positions(node.elements)
        if len(starred) > 1:
            function.error("multiple starred expressions in assignment", node)
        if starred:
            # ``a, *b, c = value``: the protocol returns the leading items,
            # a list of the middle ones and the trailing items.
            before = starred[0]
            after = len(node.elements) - before - 1
            ctran, low = Ctran(), Lvar()
            function.constant(self.start, ctran, low, ast.IntegerLiteral(before))
            following, high = Ctran(), Lvar()
            function.constant(ctran, following, high, ast.IntegerLiteral(after))
            ctran, values = Ctran(), Lvar()
            function.protocol(
                following,
                ctran,
                values,
                "unpack_starred",
                (self.value, low, high),
                node,
            )
            following = ctran
        else:
            ctran, count = Ctran(), Lvar()
            function.constant(
                self.start, ctran, count, ast.IntegerLiteral(len(node.elements))
            )
            following, values = Ctran(), Lvar()
            function.protocol(
                ctran, following, values, "unpack", (self.value, count), node
            )
        temporary = function.temporary(expression=True)
        current = Ctran()
        function.set_var(following, current, temporary, values, node)
        for index, target in enumerate(node.elements):
            ctran, loaded = Ctran(), Lvar()
            function.ref(current, ctran, loaded, temporary, node)
            following, position = Ctran(), Lvar()
            function.constant(ctran, following, position, ast.IntegerLiteral(index))
            ctran, element = Ctran(), Lvar()
            function.known(
                following, ctran, element, fndb.UNPACK_REF, (loaded, position), node
            )
            current = Ctran()
            target.accept(_UnpackedTarget(function, element, ctran, current))
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


class _UnpackedTarget(_AssignmentTarget):
    """An element of a target list, where ``*target`` receives the list of
    the items no other element takes."""

    def visit_Unpack(self, node):
        node.value.accept(
            _AssignmentTarget(self.function, self.value, self.start, self.next)
        )


class _DeleteTarget(ast.Visitor):
    """``del name``: a read establishes the binding exists, then it is unbound."""

    def __init__(self, function, start, next_):
        self.function = function
        self.start = start
        self.next = next_

    def visit_Node(self, node):
        self.function.error(
            "only identifier, attribute and subscript deletion targets are supported",
            node,
            unsupported=True,
        )

    def visit_Subscript(self, node):
        function = self.function
        ctran, value = function.value(self.start, node.value)
        ctran, index = function.value(ctran, node.index)
        function.protocol(ctran, self.next, None, "delitem", (value, index), node)

    def visit_Attribute(self, node):
        function = self.function
        ctran, value = function.value(self.start, node.value)
        following, name = Ctran(), Lvar()
        function.constant(ctran, following, name, ast.TextLiteral(node.attribute))
        function.protocol(
            following, self.next, None, "delete_attribute", (value, name), node
        )

    def visit_Tuple(self, node):
        if not node.elements:
            self.function.flow(self.start, self.next)
            return
        current = self.start
        for index, element in enumerate(node.elements):
            following = self.next if index == len(node.elements) - 1 else Ctran()
            element.accept(_DeleteTarget(self.function, current, following))
            current = following

    visit_List = visit_Tuple

    def visit_Name(self, node):
        function = self.function
        function.converter.reserved(node.identifier, node)
        if function.class_binding(node.identifier) is not None:
            # Deleting from a class namespace checks only that namespace.
            function.unbind(self.start, self.next, node.identifier, node)
            return
        var = function.variable(node.identifier)
        if var.accept(_IS_GLOBAL_VAR):
            function.set_var(self.start, self.next, var, None, node)
            return
        ctran = Ctran()
        function.ref(self.start, ctran, None, var, node)
        function.set_var(ctran, self.next, var, None, node)


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
        if function.class_binding(node.identifier) is not None:
            # Class variable annotations are deferred metadata, not contracts.
            if self.assignment.value is not None:
                ctran, value = function.value(self.start, self.assignment.value)
                function.store(ctran, self.next, node.identifier, value, node)
            return
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
        elif function.local(node.identifier):
            declared = converter.declared_contract(self.assignment.annotation, function)
            if (
                declared is not None
                and node.identifier not in function.lambda_.local_annotations
            ):
                function.lambda_.local_annotations[node.identifier] = declared
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
    def supports_runtime_annotation(self):
        return True

    def annotation(self, converter, node):
        return None

    def declared_contract(self, converter, node):
        return self.annotation(converter, node)

    def subscript_binding(self, element_binding):
        return _OPAQUE_BINDING

    def subscript_items(self) -> tuple[_CompilerBinding, ...]:
        return (self,)

    def is_ellipsis_binding(self) -> bool:
        return False

    def is_empty_tuple_binding(self) -> bool:
        return False

    def or_binding(self, other: _CompilerBinding) -> _CompilerBinding:
        left = self.declared_contract(None, None)
        right = other.declared_contract(None, None)
        if left is None or right is None:
            return _OPAQUE_BINDING
        return _DeclaredTypeBinding(union_contract(left, right))

    def policy(self, converter, decorator, policy):
        """The policy after ``decorator``, which calls this binding; only
        the ``mule_types.optimize`` directive changes it."""
        return policy

    def runtime_decorators(self, decorator):
        """What remains of ``decorator`` to apply at runtime."""
        return (decorator,)

    def directive(self, converter, node):
        converter.error(
            "only imported mule_types.optimize decorators are supported",
            node,
            unsupported=True,
        )

    def member(self, attribute, converter, node):
        return _OPAQUE_BINDING


class _NumericBinding(_CompilerBinding):
    def supports_runtime_annotation(self):
        return False

    def __init__(self, contract):
        self.contract = contract

    def annotation(self, converter, node):
        return self.contract


class _DeclaredTypeBinding(_CompilerBinding):
    def __init__(self, contract):
        self.contract = contract

    def declared_contract(self, converter, node):
        return self.contract


class _EllipsisBinding(_CompilerBinding):
    def is_ellipsis_binding(self) -> bool:
        return True


class _EmptyTupleBinding(_CompilerBinding):
    def is_empty_tuple_binding(self) -> bool:
        return True

    def subscript_items(self) -> tuple[_CompilerBinding, ...]:
        return ()


class _SubscriptItemsBinding(_CompilerBinding):
    def __init__(self, items: tuple[_CompilerBinding, ...]):
        self.items = items

    def subscript_items(self) -> tuple[_CompilerBinding, ...]:
        return self.items


class _ListTypeBinding(_CompilerBinding):
    def declared_contract(self, converter, node):
        return ListContract()

    def subscript_binding(self, element_binding):
        items = element_binding.subscript_items()
        if len(items) != 1 or items[0].is_ellipsis_binding():
            return _OPAQUE_BINDING
        element_contract = items[0].declared_contract(None, None)
        return _DeclaredTypeBinding(ListContract("list", element_contract))


class _TupleTypeBinding(_CompilerBinding):
    def declared_contract(self, converter, node):
        return TupleContract()

    def subscript_binding(self, element_binding):
        if element_binding.is_empty_tuple_binding():
            return _DeclaredTypeBinding(TupleContract("tuple", 0, ()))
        items = element_binding.subscript_items()
        if len(items) == 2 and items[1].is_ellipsis_binding():
            elem = items[0].declared_contract(None, None)
            return _DeclaredTypeBinding(TupleContract("tuple", None, (elem,)))
        if any(item.is_ellipsis_binding() for item in items):
            return _OPAQUE_BINDING
        elems = tuple(item.declared_contract(None, None) for item in items)
        return _DeclaredTypeBinding(TupleContract("tuple", len(elems), elems))


class _DictTypeBinding(_CompilerBinding):
    def declared_contract(self, converter, node):
        return DictContract()

    def subscript_binding(self, element_binding):
        items = element_binding.subscript_items()
        if len(items) != 2 or any(item.is_ellipsis_binding() for item in items):
            return _OPAQUE_BINDING
        key_contract = items[0].declared_contract(None, None)
        val_contract = items[1].declared_contract(None, None)
        return _DeclaredTypeBinding(DictContract("dict", key_contract, val_contract))


class _SetTypeBinding(_CompilerBinding):
    def __init__(self, name: str = "set", identity: str = "mule_dict_set"):
        self.name = name
        self.identity = identity

    def declared_contract(self, converter, node):
        return SetContract(self.name, self.identity)

    def subscript_binding(self, element_binding):
        items = element_binding.subscript_items()
        if len(items) != 1 or items[0].is_ellipsis_binding():
            return _OPAQUE_BINDING
        element_contract = items[0].declared_contract(None, None)
        return _DeclaredTypeBinding(
            SetContract(self.name, self.identity, element_contract)
        )


class _OptimizeBinding(_CompilerBinding):
    def directive(self, converter, node):
        return None

    def policy(self, converter, decorator, policy):
        return decorator.accept(_PolicyDecorator(converter, policy))

    def runtime_decorators(self, decorator):
        # A compile-time declaration, not a runtime decorator.
        return ()


class _ModuleBinding(_CompilerBinding):
    def supports_runtime_annotation(self):
        return False

    def member(self, attribute, converter, node):
        binding = _EXPORT_BINDINGS.get(attribute)
        if binding is None:
            return _OPAQUE_BINDING
        return binding


_EXPORT_BINDINGS: dict[str, _CompilerBinding] = {
    name: _NumericBinding(contract) for name, contract in EXPORTS.items()
}
_EXPORT_BINDINGS["optimize"] = _OptimizeBinding()
_MODULE_BINDING = _ModuleBinding()
_OPAQUE_BINDING = _CompilerBinding()
_ELLIPSIS_BINDING = _EllipsisBinding()
_EMPTY_TUPLE_BINDING = _EmptyTupleBinding()
_NONE_BINDING = _DeclaredTypeBinding(NoneContract())
_BUILTIN_BINDINGS = {
    name: _NumericBinding(contract) for name, contract in BUILTINS.items()
}
_DECLARATION_BINDINGS: dict[str, _CompilerBinding] = {
    **_BUILTIN_BINDINGS,
    "bool": _DeclaredTypeBinding(DECLARATIONS["bool"]),
    "str": _DeclaredTypeBinding(DECLARATIONS["str"]),
    "bytes": _DeclaredTypeBinding(DECLARATIONS["bytes"]),
    "list": _ListTypeBinding(),
    "tuple": _TupleTypeBinding(),
    "dict": _DictTypeBinding(),
    "set": _SetTypeBinding("set", "mule_dict_set"),
    "frozenset": _SetTypeBinding("frozenset", "mule_dict_frozenset"),
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
            if item.name != "mule_types":
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
            self.converter.future_annotations = True
            return
        if node.module == "__future__" and node.level == 0:
            self.converter.error(
                "unsupported __future__ compiler directive", node, unsupported=True
            )
        if node.module != "mule_types" or node.level:
            return
        for item in node.items:
            binding = _EXPORT_BINDINGS.get(item.name)
            if binding is None:
                continue
            self.bind(item.alias or item.name, binding, item)


class _RuntimeAnnotation(ast.Visitor):
    def __init__(self, converter):
        self.converter = converter

    def visit_Node(self, node):
        return all(child.accept(self) for child in node.children())

    def visit_Name(self, node):
        binding = self.converter.active_compiler_bindings.get(node.identifier)
        return binding is None or binding.supports_runtime_annotation()


class _IsTypingName(ast.Visitor):
    def visit_Node(self, node):
        return False

    def visit_Name(self, node):
        return node.identifier == "typing"


_IS_TYPING_NAME = _IsTypingName()


class _IsFinalReference(ast.Visitor):
    def visit_Node(self, node):
        return False

    def visit_Name(self, node):
        return node.identifier == "Final"

    def visit_Attribute(self, node):
        return node.attribute == "Final" and node.value.accept(_IS_TYPING_NAME)


_IS_FINAL_REFERENCE = _IsFinalReference()


class _IsFinalAnnotation(ast.Visitor):
    def visit_Node(self, node):
        return node.accept(_IS_FINAL_REFERENCE)

    def visit_Subscript(self, node):
        return node.value.accept(_IS_FINAL_REFERENCE)


_IS_FINAL_ANNOTATION = _IsFinalAnnotation()


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
        shadows = self.converter.module_annotation_shadows
        # A wildcard import can rebind any builtin name at runtime.
        if (
            name in shadows
            or "*" in shadows
            or (self.scope is not None and self.scope.local(name))
        ):
            return _OPAQUE_BINDING
        return _BUILTIN_BINDINGS.get(name, _OPAQUE_BINDING)

    def visit_Attribute(self, node):
        return node.value.accept(self).member(node.attribute, self.converter, node)

    def visit_Subscript(self, node):
        if node.value.accept(_IS_FINAL_REFERENCE):
            return node.index.accept(self)
        return _OPAQUE_BINDING


class _DeclaredAnnotationReference(_AnnotationReference):
    def visit_Name(self, node):
        name = node.identifier
        binding = self.converter.active_compiler_bindings.get(name)
        if binding is not None:
            return binding
        if self.scope is not None and (
            self.scope.local(name)
            or self.scope.enclosing_local(name) is not None
            or (self.scope.class_scopes and name in self.scope.class_scopes[-1].bound)
        ):
            return _OPAQUE_BINDING
        class_binding = self.converter.declared_class_bindings.get(name)
        if class_binding is not None:
            return class_binding
        shadows = self.converter.module_annotation_shadows
        if name in shadows or "*" in shadows:
            return _OPAQUE_BINDING
        return _DECLARATION_BINDINGS.get(name, _OPAQUE_BINDING)

    def visit_NoneLiteral(self, node):
        return _NONE_BINDING

    def visit_EllipsisLiteral(self, node):
        return _ELLIPSIS_BINDING

    def visit_Tuple(self, node):
        if not node.elements:
            return _EMPTY_TUPLE_BINDING
        return _SubscriptItemsBinding(tuple(el.accept(self) for el in node.elements))

    def visit_Subscript(self, node):
        if node.value.accept(_IS_FINAL_REFERENCE):
            return node.index.accept(self)
        return node.value.accept(self).subscript_binding(node.index.accept(self))

    def visit_Binary(self, node):
        if node.operator == ast.BinaryOperator.BIT_OR:
            return node.left.accept(self).or_binding(node.right.accept(self))
        return _OPAQUE_BINDING


# The module-level assignments that set a module's policy, and the quality
# each sets. Python forbids assigning ``__debug__``.
class _ModuleQuality:
    def __init__(self, name):
        self.name = name

    def level(self, converter, expression):
        return expression.accept(_PolicyLevel(converter))


class _BuiltinImplementationQuality(_ModuleQuality):
    def level(self, converter, expression):
        return expression.accept(_BuiltinImplementationLevel(converter))


class _BuiltinImplementationLevel(ast.Visitor):
    def __init__(self, converter):
        self.converter = converter

    def visit_Node(self, node):
        self.converter.error(
            "builtin implementation markers must be boolean literals", node
        )

    def visit_BooleanLiteral(self, node):
        return node.value


MODULE_POLICY_NAMES = {
    "__speed__": _ModuleQuality("speed"),
    "__safety__": _ModuleQuality("safety"),
    "__debug_level__": _ModuleQuality("debug"),
    "__mule_builtin_implementation__": _BuiltinImplementationQuality(
        "builtin_implementation"
    ),
}


class _ModulePolicyAssignment(ast.Visitor):
    """The quality and level a module-level policy assignment such as
    ``__safety__ = 2`` sets, or None for any other statement."""

    def __init__(self, converter):
        self.converter = converter

    def visit_Node(self, node):
        return None

    def visit_Assignment(self, node):
        qualities = [target.accept(_PolicyName()) for target in node.targets]
        if not any(qualities):
            return None
        if len(qualities) != 1:
            self.converter.error("a module policy must be assigned on its own", node)
        quality = qualities[0]
        return quality.name, quality.level(self.converter, node.value)


class _PolicyName(ast.Visitor):
    def visit_Node(self, node):
        return None

    def visit_Name(self, node):
        return MODULE_POLICY_NAMES.get(node.identifier)


class _ModulePrologue(ast.Visitor):
    """Whether a statement may precede a module's policy assignments: an
    import, or the docstring."""

    def visit_Node(self, node):
        return False

    def visit_Import(self, node):
        return True

    def visit_ImportFrom(self, node):
        return True


class _BuiltinsReceiver(ast.Visitor):
    def visit_Node(self, node):
        return False

    def visit_Name(self, node):
        return node.identifier in ("builtins", "__builtins__")


class _BuiltinsTarget(ast.Visitor):
    def visit_Node(self, node):
        return any(child.accept(self) for child in node.children())

    def visit_Attribute(self, node):
        return node.value.accept(_BuiltinsReceiver())


class _MutatesBuiltins(ast.Visitor):
    def visit_Node(self, node):
        if any(target.accept(_BuiltinsTarget()) for target in node.assigned_targets()):
            return True
        return any(child.accept(self) for child in node.children())


def module_policy(converter, statements):
    """The policy of the module's top-level code and the default for its
    definitions: the default policy with the levels the module assigns to
    ``__speed__``, ``__safety__`` and ``__debug_level__``, as SBCL's top-level
    ``declaim`` sets the policy for the rest of a file. The top-level code is
    one lambda with one policy, so the assignments must come before the
    module's other statements. They remain ordinary bindings at run time."""
    changes = {}
    prologue = True
    for index, statement in enumerate(statements):
        assignment = statement.accept(_ModulePolicyAssignment(converter))
        if assignment is not None:
            quality, level = assignment
            if not prologue:
                converter.error(
                    "a module policy assignment must precede the module's code",
                    statement,
                )
            if quality in changes:
                converter.error(f"duplicate module policy for {quality}", statement)
            changes[quality] = level
            continue
        docstring = index == 0 and statement.accept(_Docstring())
        if not docstring and not statement.accept(_ModulePrologue()):
            prologue = False
    if "safety" not in changes and any(
        statement.accept(_MutatesBuiltins()) for statement in statements
    ):
        changes["safety"] = 3
    return replace(OptimizationPolicy(), **changes)


class _PolicyDecorator(ast.Visitor):
    def __init__(self, converter, policy):
        self.converter = converter
        self.policy = policy

    def visit_Node(self, node):
        self.converter.error(
            "only imported mule_types.optimize decorators are supported",
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


class _DecoratorBinding(ast.Visitor):
    """The compiler binding a decorator calls, such as the imported
    ``mule_types.optimize`` directive; a decorator that is not a call calls none."""

    def __init__(self, converter):
        self.converter = converter

    def visit_Node(self, node):
        return _OPAQUE_BINDING

    def visit_Call(self, node):
        return node.function.accept(_AnnotationReference(self.converter))


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
            and (
                function.annotation_for(node.identifier) is None
                or function.lexenv.policy.safety == 0
            )
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
            self.call,
            CallInfo(self.keywords(), function.converter.location(self.call)),
        )

    def visit_Name(self, node):
        """``globals()`` and zero-argument ``super()`` receive an argument the
        source does not write: the calling function's globals namespace, or
        the defining class's ``__class__`` cell and the calling function's
        first argument, as CPython's compiler supplies them. The name itself
        is still looked up normally."""
        function = self.function
        if (
            node.identifier == "globals"
            and not self.call.arguments
            and not function.local("globals")
        ):
            ctran, callee = function.value(self.start, node)
            following, namespace = Ctran(), Lvar()
            function.known(ctran, following, namespace, fndb.CURRENT_GLOBALS, (), node)
            function.full_call(
                following,
                self.next,
                self.result,
                callee,
                (namespace,),
                self.call,
                CallInfo((), function.converter.location(self.call)),
            )
            return
        if (
            node.identifier in ("locals", "vars", "dir")
            and not self.call.arguments
            and not function.local(node.identifier)
            and function.comprehension_variable(node.identifier) is None
            and function.enclosing_local(node.identifier) is None
        ):
            ctran, callee = function.value(self.start, node)
            after_snapshot, snapshot = function.snapshot_current_locals(ctran, node)
            function.protocol(
                after_snapshot,
                self.next,
                self.result,
                "call_with_locals",
                (callee, snapshot),
                self.call,
            )
            return
        if (
            node.identifier in ("eval", "exec")
            and len(self.call.arguments) == 1
            and self.call.arguments[0].kind == ast.ArgumentKind.POSITIONAL
            and not function.local(node.identifier)
            and function.comprehension_variable(node.identifier) is None
            and function.enclosing_local(node.identifier) is None
        ):
            function.component.mutates_module_namespace = True
            ctran, callee = function.value(self.start, node)
            ctran, source_arg = function.value(ctran, self.call.arguments[0].value)
            after_globals, namespace = Ctran(), Lvar()
            function.known(
                ctran, after_globals, namespace, fndb.CURRENT_GLOBALS, (), node
            )
            after_gdict, globals_dict = Ctran(), Lvar()
            function.protocol(
                after_globals,
                after_gdict,
                globals_dict,
                "globals_dict",
                (namespace,),
                node,
            )
            after_ldict, locals_dict = function.snapshot_current_locals(
                after_gdict, node
            )
            function.protocol(
                after_ldict,
                self.next,
                self.result,
                "call_with_eval_context",
                (callee, source_arg, globals_dict, locals_dict),
                self.call,
            )
            return
        if node.identifier != "super" or self.call.arguments:
            return self.visit_Node(node)
        parameters = function.lambda_.vars
        if (
            function.defining_class is None
            or not parameters
            or function.lambda_.signature.kinds[0] == ast.ParameterKind.VAR_POSITIONAL
            or function.local("super")
        ):
            return self.visit_Node(node)
        cell = function.class_cell()
        ctran, callee = function.value(self.start, node)
        following, owner = Ctran(), Lvar()
        function.ref(ctran, following, owner, cell, node)
        ctran, receiver = Ctran(), Lvar()
        function.ref(following, ctran, receiver, parameters[0], node)
        function.full_call(
            ctran,
            self.next,
            self.result,
            callee,
            (owner, receiver),
            self.call,
            CallInfo((), function.converter.location(self.call)),
        )

    def visit_Attribute(self, node):
        function = self.function
        super_name = node.value.accept(_SuperCallee(function))
        if super_name is not None:
            cell = function.class_cell()
            ctran, callee = function.value(self.start, super_name)
            following, owner = Ctran(), Lvar()
            function.ref(ctran, following, owner, cell, node.value)
            ctran, receiver = Ctran(), Lvar()
            function.ref(
                following, ctran, receiver, function.lambda_.vars[0], node.value
            )
            following, resolved = Ctran(), Lvar()
            function.known(
                ctran,
                following,
                resolved,
                fndb.RESOLVE_SUPER_METHOD,
                (callee, owner, receiver),
                node,
                AttributeInfo(node.attribute, function.converter.location(node)),
            )
        else:
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
            self.call,
            CallInfo(self.keywords(), None),
        )


class _SuperCallee(ast.Visitor):
    """The ``super`` name node of a zero-argument ``super()`` receiver in a method."""

    def __init__(self, function):
        self.function = function
        self.in_call = False

    def visit_Node(self, node):
        return None

    def visit_Call(self, node: ast.Call):
        if self.in_call or node.arguments:
            return None
        self.in_call = True
        return node.function.accept(self)

    def visit_Name(self, node: ast.Name):
        if not self.in_call or node.identifier != "super":
            return None
        function = self.function
        parameters = function.lambda_.vars
        if (
            function.defining_class is None
            or not parameters
            or function.lambda_.signature.kinds[0] == ast.ParameterKind.VAR_POSITIONAL
            or function.local("super")
        ):
            return None
        return node


def _documentation(statements):
    return statements[0].accept(_Documentation()) if statements else None


class _Documentation(ast.Visitor):
    """Python 3.14's compile-time doc indentation, without changing the AST."""

    def visit_Node(self, node):
        return None

    def visit_ExpressionStatement(self, node):
        return node.expression.accept(self)

    def visit_TextLiteral(self, node):
        lines = node.value.expandtabs().split("\n")
        margin = min(
            (
                len(line) - len(line.lstrip(" "))
                for line in lines[1:]
                if line.strip(" ")
            ),
            default=0,
        )
        cleaned = "\n".join(
            [lines[0].lstrip(" "), *(line[margin:] for line in lines[1:])]
        )
        return ast.TextLiteral(cleaned, span=node.span)


class _Docstring(ast.Visitor):
    def visit_Node(self, node):
        return False

    def visit_ExpressionStatement(self, node):
        return node.expression.accept(self)

    def visit_TextLiteral(self, node):
        return True


class _AnnotatedDeclaration(ast.Visitor):
    """Whether a class body statement is an annotated declaration, which
    asks for a typed native layout under ``optimize(speed=3)``."""

    def visit_Node(self, node):
        return False

    def visit_AnnotatedAssignment(self, node):
        return True


class _NamedAnnotations(ast.Visitor):
    """The ``(name, annotation)`` pairs a class body statement records in
    ``__annotations__``: those of annotated simple names."""

    def visit_Node(self, node):
        return ()

    def visit_AnnotatedAssignment(self, node):
        return node.target.accept(_AnnotatedName(node.annotation))


class _AnnotatedName(ast.Visitor):
    def __init__(self, annotation):
        self.annotation = annotation

    def visit_Node(self, node):
        return ()

    def visit_Name(self, node):
        return ((node.identifier, self.annotation),)


class _ClassBody(ast.Visitor):
    def __init__(self, function):
        self.function = function
        self.methods = []
        self.fields = {}
        self.annotation_expressions = {}
        self.native_eligible = True

    def visit_Node(self, node):
        self.function.error(
            "class bodies support only a leading docstring, instance methods, "
            "bare typed native fields, and pass",
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

    def visit_Lambda(self, node):
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

    def visit_Tuple(self, node):
        for element in node.elements:
            element.accept(self)

    visit_List = visit_Tuple

    def visit_Unpack(self, node):
        node.value.accept(self)

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
    def imported_name(self, name, node):
        raise NotImplementedError

    def visit_Import(self, node):
        for item in node.items:
            self.imported_name(item.alias or item.name.split(".")[0], item)

    def visit_ImportFrom(self, node):
        for item in node.items:
            self.imported_name(item.alias or item.name, item)


class _ClassBindings(_ImportNames):
    """Names a class body binds in its namespace, and its declarations.

    Like a function scope, a binding anywhere in the body makes the name the
    namespace's, but unlike one, reading it before assignment falls back to
    the module globals instead of raising ``UnboundLocalError``."""

    def __init__(self):
        self.bound = set()
        self.globals = set()
        self.nonlocals = set()
        self.targets = _BoundNames(self.bound)

    def imported_name(self, name, node):
        self.bound.add(name)

    def visit_Node(self, node):
        for target in node.assigned_targets():
            target.accept(self.targets)
        for child in node.children():
            child.accept(self)

    def visit_MatchCase(self, node):
        self.bound.update(_pattern_names(node.pattern))
        self.visit_Node(node)

    def visit_Lambda(self, node):
        for parameter in node.parameters:
            if parameter.default is not None:
                parameter.default.accept(self)

    def visit_Comprehension(self, node):
        # Only the first iterable is evaluated in the enclosing scope.
        node.clauses[0].iterable.accept(self)

    def visit_FunctionDefinition(self, node):
        self.bound.add(node.name)
        for decorator in node.decorators:
            decorator.accept(self)
        for parameter in node.parameters:
            if parameter.default is not None:
                parameter.default.accept(self)

    def visit_ClassDefinition(self, node):
        self.bound.add(node.name)
        for child in (*node.decorators, *node.arguments):
            child.accept(self)

    def visit_TypeAlias(self, node):
        self.bound.add(node.name)

    def visit_ExceptionHandler(self, node):
        if node.binding is not None:
            self.bound.add(node.binding)
        self.visit_Node(node)

    def visit_Global(self, node):
        self.globals.update(node.identifiers)

    def visit_Nonlocal(self, node):
        self.nonlocals.update(node.identifiers)


class BindsName(ast.Visitor):
    """Whether any statement of a module, at any depth, may bind ``name`` in
    the module namespace: an assignment or deletion target, an import, a
    definition, an exception or pattern binding, or a ``global`` declaration
    in a nested scope that could store it. A binding in a class body is
    counted too, conservatively."""

    def __init__(self, name):
        self.name = name
        self.found = False
        self._bound_names = set()
        self._bound_visitor = _BoundNames(self._bound_names)

    def visit_Node(self, node):
        if self.found:
            return
        targets = node.assigned_targets()
        if targets:
            names = self._bound_names
            visitor = self._bound_visitor
            for target in targets:
                names.clear()
                target.accept(visitor)
                if self.name in names:
                    self.found = True
                    return
        for child in node.children():
            child.accept(self)
            if self.found:
                return

    def visit_FunctionDefinition(self, node):
        if node.name == self.name:
            self.found = True
            return
        self.visit_Node(node)

    def visit_ClassDefinition(self, node):
        if node.name == self.name:
            self.found = True
            return
        self.visit_Node(node)

    def visit_TypeAlias(self, node):
        if node.name == self.name:
            self.found = True
            return
        self.visit_Node(node)

    def visit_ExceptionHandler(self, node):
        if node.binding == self.name:
            self.found = True
            return
        self.visit_Node(node)

    def visit_MatchCase(self, node):
        if self.name in _pattern_names(node.pattern):
            self.found = True
            return
        self.visit_Node(node)

    def visit_Global(self, node):
        self.found |= self.name in node.identifiers

    def visit_Import(self, node):
        for item in node.items:
            if (item.alias or item.name.split(".")[0]) == self.name:
                self.found = True
                return

    def visit_ImportFrom(self, node):
        for item in node.items:
            if item.name == "*" or (item.alias or item.name) == self.name:
                self.found = True
                return


class _BoundNames(ast.Visitor):
    def __init__(self, names):
        self.names = names

    def visit_Node(self, node):
        for child in node.children():
            child.accept(self)

    def visit_Subscript(self, node):
        pass

    def visit_Attribute(self, node):
        pass

    def visit_Name(self, node):
        self.names.add(node.identifier)


class _SlotNames(ast.Visitor):
    """The instance fields a literal slot declaration supplies with
    string literals; other statements and computed values declare none."""

    def visit_Node(self, node):
        return ()

    def visit_Assignment(self, node):
        if not (
            len(node.targets) == 1
            and (
                node.targets[0].accept(_ReceiverName("__slots__"))
                or node.targets[0].accept(_ReceiverName("__mule_slots__"))
            )
        ):
            return ()
        return node.value.accept(_SlotDeclaration())


class _SlotName(ast.Visitor):
    """The field a ``__slots__`` element declares: a string literal other
    than the special ``__dict__`` and ``__weakref__`` slots."""

    def visit_Node(self, node):
        return ()

    def visit_TextLiteral(self, node):
        return () if node.value in ("__dict__", "__weakref__") else (node.value,)


class _SlotDeclaration(_SlotName):
    """The fields a ``__slots__`` value declares: one string, or a tuple or
    list of them."""

    def visit_Tuple(self, node):
        return self.elements(node)

    def visit_List(self, node):
        return self.elements(node)

    def elements(self, node):
        return tuple(
            name for element in node.elements for name in element.accept(_SlotName())
        )


class _ContainsAsync(ast.Visitor):
    """Whether evaluation in this scope can suspend, including eager
    comprehensions but excluding deferred lambda and generator bodies."""

    def visit_Node(self, node):
        return any(child.accept(self) for child in node.children())

    def visit_Await(self, node):
        return True

    def visit_For(self, node):
        return node.is_async or self.visit_Node(node)

    def visit_With(self, node):
        return node.is_async or self.visit_Node(node)

    def visit_ComprehensionClause(self, node):
        return node.is_async or self.visit_Node(node)

    def visit_Comprehension(self, node):
        if node.kind == ast.ComprehensionKind.GENERATOR:
            return node.clauses[0].iterable.accept(self)
        return self.visit_Node(node)

    def defaults(self, parameters):
        return any(
            parameter.default is not None and parameter.default.accept(self)
            for parameter in parameters
        )

    def visit_Lambda(self, node):
        return self.defaults(node.parameters)

    def visit_FunctionDefinition(self, node):
        return any(
            decorator.accept(self) for decorator in node.decorators
        ) or self.defaults(node.parameters)

    def visit_ClassDefinition(self, node):
        return any(child.accept(self) for child in (*node.decorators, *node.arguments))


class _ContainsYield(ast.Visitor):
    """Whether a function body yields, making the function a generator.
    Nested functions, lambdas and classes are separate scopes."""

    def visit_Node(self, node):
        for child in node.children():
            if child.accept(self):
                return True
        return False

    def visit_Yield(self, node):
        return True

    def visit_FunctionDefinition(self, node):
        return False

    def visit_Lambda(self, node):
        return False

    def visit_ClassDefinition(self, node):
        return False

    def visit_Comprehension(self, node):
        # Only the first iterable is evaluated in the enclosing function.
        return node.clauses[0].iterable.accept(self)


def _comprehension_assignments(node):
    """Names assignment expressions in a comprehension bind."""
    collector = _AssignmentExpressionNames()
    for child in node.children():
        child.accept(collector)
    return collector.names


class _AssignmentExpressionNames(ast.Visitor):
    """Names assignment expressions bind, outside nested scopes."""

    def __init__(self):
        self.names = []

    def visit_Node(self, node):
        for child in node.children():
            child.accept(self)

    def visit_NamedExpression(self, node):
        self.names.append(node.target.identifier)
        self.visit_Node(node)

    def visit_Lambda(self, node):
        pass

    def visit_FunctionDefinition(self, node):
        pass

    def visit_ClassDefinition(self, node):
        pass


class _ComprehensionAssignment(ast.Visitor):
    """Whether a comprehension contains an assignment expression, which binds
    in the enclosing scope rather than the comprehension's."""

    def visit_Node(self, node):
        for child in node.children():
            if child.accept(self):
                return True
        return False

    def visit_NamedExpression(self, node):
        return True

    def visit_Lambda(self, node):
        return False


def _pattern_names(pattern):
    """The names a pattern binds when it matches."""
    collector = _PatternNames()
    pattern.accept(collector)
    return collector.names


class _PatternNames(ast.Visitor):
    """Collects pattern bindings in source order; the expressions inside
    patterns bind nothing."""

    def __init__(self):
        self.names = []

    def visit_Node(self, node):
        raise ValueError(f"{type(node).__name__} node is not part of a pattern")

    def bind(self, name):
        if name:
            self.names.append(name)

    def visit_CapturePattern(self, node):
        self.bind(node.name)

    def visit_StarPattern(self, node):
        self.bind(node.name)

    def visit_AsPattern(self, node):
        self.bind(node.name)
        node.pattern.accept(self)

    def visit_LiteralPattern(self, node):
        pass

    def visit_ValuePattern(self, node):
        pass

    def visit_SequencePattern(self, node):
        for element in node.elements:
            element.accept(self)

    def visit_OrPattern(self, node):
        for alternative in node.alternatives:
            alternative.accept(self)

    def visit_MappingPattern(self, node):
        self.bind(node.rest)
        for entry in node.entries:
            entry.accept(self)

    def visit_PatternEntry(self, node):
        node.pattern.accept(self)

    def visit_ClassPattern(self, node):
        for positional in node.positional:
            positional.accept(self)
        for keyword in node.keywords:
            keyword.accept(self)

    def visit_KeywordPattern(self, node):
        node.pattern.accept(self)


class _ClassMethods(ast.Visitor):
    """Function definitions a class body executes directly, including those
    under its compound statements, but not inside nested scopes."""

    def __init__(self):
        self.methods = []

    @classmethod
    def of(cls, statements):
        collector = cls()
        for statement in statements:
            statement.accept(collector)
        return collector.methods

    def visit_Statement(self, node):
        self.visit_children(node)

    def visit_ExceptionHandler(self, node):
        self.visit_children(node)

    def visit_children(self, node):
        for child in node.children():
            child.accept(self)

    def visit_FunctionDefinition(self, node):
        self.methods.append(node)

    def visit_ClassDefinition(self, node):
        pass


class _StaticAnnotationReference(_AnnotationReference):
    def visit_Name(self, node):
        name = node.identifier
        binding = self.converter.compiler_bindings.get(name)
        if binding is not None:
            return binding
        return super().visit_Name(node)


class _StaticDecoratorBinding(ast.Visitor):
    def __init__(self, converter):
        self.converter = converter

    def visit_Node(self, node):
        return _OPAQUE_BINDING

    def visit_Call(self, node):
        return node.function.accept(_StaticAnnotationReference(self.converter))


class _BaseClassName(ast.Visitor):
    def visit_Node(self, node):
        return None

    def visit_Name(self, node: ast.Name):
        return node.identifier


_BASE_CLASS_NAME = _BaseClassName()


class _DefinesNew(ast.Visitor):
    def visit_Node(self, node):
        return False

    def visit_FunctionDefinition(self, node: ast.FunctionDefinition):
        return node.name == "__new__"


_DEFINES_NEW = _DefinesNew()


class _ClassHierarchyScan(ast.Visitor):
    def __init__(self):
        self.subclassed_names: set[str] = set()
        self.has_dynamic_base: bool = False
        self.has_delattr: bool = False
        self.mutated_match_args: bool = False

    def visit_Node(self, node):
        for child in node.children():
            child.accept(self)

    def visit_Delete(self, node: ast.Delete):
        for target in node.targets:
            if target.accept(_IS_ATTRIBUTE_NODE):
                self.has_delattr = True
        self.visit_Node(node)

    def visit_Call(self, node: ast.Call):
        callee_name = node.function.accept(_BASE_CLASS_NAME)
        if callee_name == "delattr":
            self.has_delattr = True
            self.mutated_match_args = True
        elif callee_name == "setattr":
            self.mutated_match_args = True
        self.visit_Node(node)

    def visit_Assignment(self, node: ast.Assignment):
        for target in node.targets:
            if target.accept(_TARGETS_MATCH_ARGS):
                self.mutated_match_args = True
        self.visit_Node(node)

    def visit_AnnotatedAssignment(self, node: ast.AnnotatedAssignment):
        if node.target.accept(_TARGETS_MATCH_ARGS):
            self.mutated_match_args = True
        self.visit_Node(node)

    def visit_AugmentedAssignment(self, node: ast.AugmentedAssignment):
        if node.target.accept(_TARGETS_MATCH_ARGS):
            self.mutated_match_args = True
        self.visit_Node(node)

    def visit_ClassDefinition(self, node: ast.ClassDefinition):
        for arg in node.arguments:
            if arg.kind == ast.ArgumentKind.POSITIONAL:
                base_name = arg.value.accept(_BASE_CLASS_NAME)
                if base_name is not None:
                    self.subclassed_names.add(base_name)
                else:
                    self.has_dynamic_base = True
            elif arg.kind == ast.ArgumentKind.STARRED:
                self.has_dynamic_base = True
        for child in node.body:
            child.accept(self)


class _IsAttributeNode(ast.Visitor):
    def visit_Node(self, node):
        return False

    def visit_Attribute(self, node: ast.Attribute):
        return True


_IS_ATTRIBUTE_NODE = _IsAttributeNode()


class _TargetsMatchArgs(ast.Visitor):
    def visit_Node(self, node):
        return False

    def visit_Attribute(self, node: ast.Attribute):
        return node.attribute == "__match_args__"

    def visit_Tuple(self, node: ast.Tuple):
        return any(el.accept(self) for el in node.elements)

    def visit_List(self, node: ast.List):
        return any(el.accept(self) for el in node.elements)


_TARGETS_MATCH_ARGS = _TargetsMatchArgs()


class _StringLiteralTuple(ast.Visitor):
    def visit_Node(self, node):
        return None

    def visit_Tuple(self, node: ast.Tuple):
        names: list[str] = []
        for element in node.elements:
            text = element.accept(_TEXT_LITERAL_VALUE)
            if text is None:
                return None
            names.append(text)
        return tuple(names)


class _TextLiteralValue(ast.Visitor):
    def visit_Node(self, node):
        return None

    def visit_TextLiteral(self, node: ast.TextLiteral):
        return node.value


_TEXT_LITERAL_VALUE = _TextLiteralValue()
_STRING_LITERAL_TUPLE = _StringLiteralTuple()


class _SelfAttributeAssignment(ast.Visitor):
    def __init__(self, self_name: str):
        self.self_name = self_name

    def visit_Node(self, node):
        return None

    def visit_Assignment(self, node: ast.Assignment):
        if len(node.targets) != 1:
            return None
        return node.targets[0].accept(self)

    def visit_AnnotatedAssignment(self, node: ast.AnnotatedAssignment):
        if node.value is None:
            return None
        return node.target.accept(self)

    def visit_Attribute(self, node: ast.Attribute):
        if node.value.accept(_BASE_CLASS_NAME) == self.self_name:
            return node.attribute
        return None


class _EarlyExitCheck(ast.Visitor):
    def __init__(self):
        self.has_early_exit = False

    def visit_Node(self, node):
        for child in node.children():
            child.accept(self)

    def visit_FunctionDefinition(self, node: ast.FunctionDefinition):
        pass

    def visit_ClassDefinition(self, node: ast.ClassDefinition):
        pass

    def visit_Return(self, node: ast.Return):
        self.has_early_exit = True

    def visit_Raise(self, node: ast.Raise):
        self.has_early_exit = True

    def visit_Yield(self, node: ast.Yield):
        self.has_early_exit = True

    def visit_YieldFrom(self, node: ast.YieldFrom):
        self.has_early_exit = True


class _DeclaredClassMatchExtractor(ast.Visitor):
    """Extracts static ``(__match_args__, guaranteed_fields)`` for a top-level
    class with no bases or decorators."""

    def __init__(self):
        self.valid = True
        self.has_match_args = False
        self.match_args: tuple[str, ...] | None = None
        self.slots: tuple[str, ...] | None = None
        self.init_fields: set[str] = set()
        self.class_attrs: set[str] = set()

    def visit_Node(self, node):
        self.valid = False

    def visit_Pass(self, node: ast.Pass):
        pass

    def visit_ExpressionStatement(self, node: ast.ExpressionStatement):
        if node.expression.accept(_TEXT_LITERAL_VALUE) is None:
            self.valid = False

    def visit_AnnotatedAssignment(self, node: ast.AnnotatedAssignment):
        name = node.target.accept(_BASE_CLASS_NAME)
        if name is None:
            self.valid = False
            return
        if node.value is not None:
            self._record_class_assignment(name, node.value)

    def visit_Assignment(self, node: ast.Assignment):
        if len(node.targets) != 1:
            self.valid = False
            return
        name = node.targets[0].accept(_BASE_CLASS_NAME)
        if name is None:
            self.valid = False
            return
        self._record_class_assignment(name, node.value)

    def _record_class_assignment(self, name: str, value: ast.Node):
        if name == "__match_args__":
            if self.has_match_args:
                self.valid = False
                return
            self.has_match_args = True
            parsed = value.accept(_STRING_LITERAL_TUPLE)
            if parsed is None:
                self.valid = False
            else:
                self.match_args = parsed
        elif name == "__slots__":
            parsed = value.accept(_STRING_LITERAL_TUPLE)
            if parsed is not None:
                self.slots = parsed
        else:
            self.valid = False

    def visit_FunctionDefinition(self, node: ast.FunctionDefinition):
        if node.decorators or node.name in (
            "__getattr__",
            "__getattribute__",
            "__setattr__",
            "__delattr__",
        ):
            self.valid = False
            return
        self.class_attrs.add(node.name)
        if node.name == "__init__":
            positional = [
                param
                for param in node.parameters
                if param.kind
                in (
                    ast.ParameterKind.POSITIONAL_ONLY,
                    ast.ParameterKind.POSITIONAL_OR_KEYWORD,
                )
            ]
            if not positional:
                return
            self_name = positional[0].name
            extractor = _SelfAttributeAssignment(self_name)
            exit_check = _EarlyExitCheck()
            for stmt in node.body:
                stmt.accept(exit_check)
                if exit_check.has_early_exit:
                    break
                attr_name = stmt.accept(extractor)
                if attr_name is not None:
                    self.init_fields.add(attr_name)


class _TopLevelDeclaredClasses(ast.Visitor):
    def __init__(
        self,
        converter: Converter,
        shadows: _AnnotationShadows,
        hierarchy: _ClassHierarchyScan,
    ):
        self.converter = converter
        self.shadows = shadows
        self.hierarchy = hierarchy

    def visit_Node(self, node):
        pass

    def visit_ClassDefinition(self, node: ast.ClassDefinition):
        if (
            node.type_parameters
            or "*" in self.shadows.names
            or self.shadows.counts.get(node.name, 0) != 1
            or self.hierarchy.has_dynamic_base
            or node.name in self.hierarchy.subclassed_names
            or any(stmt.accept(_DEFINES_NEW) for stmt in node.body)
            or any(
                decorator.accept(
                    _StaticDecoratorBinding(self.converter)
                ).runtime_decorators(decorator)
                for decorator in node.decorators
            )
            or any(
                arg.kind in (ast.ArgumentKind.KEYWORD, ast.ArgumentKind.DOUBLE_STARRED)
                for arg in node.arguments
            )
        ):
            return
        identity = self.converter.class_identity(node)
        if identity in ("mule_dict_set", "mule_dict_frozenset"):
            self.converter.declared_class_bindings[node.name] = _SetTypeBinding(
                node.name, identity
            )
        else:
            self.converter.declared_class_bindings[node.name] = _DeclaredTypeBinding(
                ClassContract(node.name, identity)
            )
        if (
            not node.arguments
            and not node.decorators
            and not self.hierarchy.has_delattr
            and not self.hierarchy.mutated_match_args
        ):
            extractor = _DeclaredClassMatchExtractor()
            for stmt in node.body:
                stmt.accept(extractor)
            if extractor.valid:
                guaranteed = extractor.init_fields - extractor.class_attrs
                if extractor.slots is not None:
                    guaranteed = guaranteed & set(extractor.slots)
                self.converter.declared_class_match_info[node.name] = (
                    extractor.match_args,
                    frozenset(guaranteed),
                )


class _GlobalDeclarations(ast.Visitor):
    def __init__(self, shadows: _AnnotationShadows):
        self.shadows = shadows

    def visit_Node(self, node):
        for child in node.children():
            child.accept(self)

    def visit_Global(self, node):
        for name in node.identifiers:
            self.shadows.bind(name)


class _AnnotationShadows(_ImportNames):
    """Runtime bindings prevent treating the same name as a builtin contract."""

    def __init__(self):
        self.names = set()
        self.counts: dict[str, int] = {}

    def bind(self, name: str) -> None:
        self.names.add(name)
        self.counts[name] = self.counts.get(name, 0) + 1

    def register_declared_classes(
        self, converter: Converter, statements: list[ast.Statement]
    ) -> None:
        hierarchy = _ClassHierarchyScan()
        for statement in statements:
            statement.accept(hierarchy)
        registrar = _TopLevelDeclaredClasses(converter, self, hierarchy)
        for statement in statements:
            statement.accept(registrar)

    def imported_name(self, name, node):
        self.bind(name)

    def visit_Node(self, node):
        for target in node.assigned_targets():
            target.accept(_AnnotationShadowTarget(self))
        for child in node.children():
            child.accept(self)

    def visit_Delete(self, node):
        for target in node.targets:
            target.accept(_AnnotationShadowTarget(self))
        self.visit_Node(node)

    def visit_ExceptionHandler(self, node):
        if node.binding is not None:
            self.bind(node.binding)
        self.visit_Node(node)

    def visit_MatchCase(self, node):
        for name in _pattern_names(node.pattern):
            self.bind(name)
        self.visit_Node(node)

    def visit_Comprehension(self, node):
        # Only the first iterable is evaluated in this scope.
        node.clauses[0].iterable.accept(self)

    def visit_Lambda(self, node):
        # Only defaults run in this scope; the body is another scope.
        for parameter in node.parameters:
            if parameter.default is not None:
                parameter.default.accept(self)

    def visit_FunctionDefinition(self, node):
        self.bind(node.name)
        globals_visitor = _GlobalDeclarations(self)
        for statement in node.body:
            statement.accept(globals_visitor)

    def visit_ClassDefinition(self, node):
        self.bind(node.name)
        globals_visitor = _GlobalDeclarations(self)
        for statement in node.body:
            statement.accept(globals_visitor)

    def visit_AnnotatedAssignment(self, node):
        node.target.accept(_AnnotationShadowTarget(self))
        if node.value is not None:
            node.value.accept(self)


class _AnnotationShadowTarget(ast.Visitor):
    def __init__(self, shadows: _AnnotationShadows):
        self.shadows = shadows

    def visit_Node(self, node):
        for child in node.children():
            child.accept(self)

    def visit_Subscript(self, node):
        pass

    def visit_Attribute(self, node):
        pass

    def visit_Name(self, node):
        self.shadows.bind(node.identifier)


class _ScopeVisitor(_ImportNames):
    def __init__(self, function):
        self.function = function
        self.seen = set(function.lambda_.parameters)
        self.all_seen = set(function.lambda_.parameters)

    def imported_name(self, name, node):
        self.seen.add(name)
        self.all_seen.add(name)

    def visit_Node(self, node):
        for child in node.children():
            child.accept(self)

    def visit_Name(self, node):
        self.seen.add(node.identifier)
        self.all_seen.add(node.identifier)

    def visit_AnnotatedAssignment(self, node):
        node.target.accept(self)
        if node.value is not None:
            node.value.accept(self)

    def visit_Comprehension(self, node):
        # Only the first iterable is evaluated in this scope.
        node.clauses[0].iterable.accept(self)
        if node.kind != ast.ComprehensionKind.GENERATOR:
            saved = set(self.seen)
            self.visit_Node(node)
            self.seen = saved

    def visit_Lambda(self, node):
        # Only defaults run in this scope; the body is another scope.
        for parameter in node.parameters:
            if parameter.default is not None:
                parameter.default.accept(self)

    def visit_FunctionDefinition(self, node):
        self.seen.add(node.name)
        self.all_seen.add(node.name)

    def visit_ClassDefinition(self, node):
        for child in (*node.decorators, *node.arguments):
            child.accept(self)
        self.seen.add(node.name)
        self.all_seen.add(node.name)

    def visit_TypeAlias(self, node):
        self.seen.add(node.name)
        self.all_seen.add(node.name)

    def visit_Global(self, node):
        for name in node.identifiers:
            if name not in self.function.globals and name in self.seen:
                self.function.error(
                    f"name '{name}' is used or assigned before global declaration", node
                )
            self.function.globals.add(name)

    def visit_Nonlocal(self, node):
        function = self.function
        if function.is_module:
            function.error("nonlocal declaration not allowed at module level", node)
        for name in node.identifiers:
            if name in function.globals:
                function.error(f"name '{name}' is nonlocal and global", node)
            if name not in function.nonlocals and name in self.seen:
                function.error(
                    f"name '{name}' is used or assigned before nonlocal declaration",
                    node,
                )
            var = function.enclosing_local(name)
            if var is None:
                function.error(f"no binding for nonlocal '{name}' found", node)
            if var not in function.lambda_.declared_free:
                function.lambda_.declared_free.append(var)
            function.nonlocals.add(name)


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

    def visit_Comprehension(self, node):
        # Only the first iterable is evaluated in this scope, but an
        # assignment expression inside binds here (PEP 572).
        node.clauses[0].iterable.accept(self)
        for name in _comprehension_assignments(node):
            ast.Name(name, span=node.span).accept(self.target_visitor)

    def visit_MatchCase(self, node):
        for name in _pattern_names(node.pattern):
            ast.Name(name, span=node.span).accept(self.target_visitor)
        self.visit_Node(node)

    def visit_Lambda(self, node):
        # Only defaults run in this scope; the body is another scope.
        for parameter in node.parameters:
            if parameter.default is not None:
                parameter.default.accept(self)

    def visit_FunctionDefinition(self, node):
        # The definition binds its name here; the body is another scope.
        ast.Name(node.name, span=node.span).accept(self.target_visitor)

    def visit_ClassDefinition(self, node):
        ast.Name(node.name, span=node.span).accept(self.target_visitor)

    def visit_TypeAlias(self, node):
        ast.Name(node.name, span=node.span).accept(self.target_visitor)

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
        function = self.function
        if (
            node.identifier not in function.globals
            and node.identifier not in function.nonlocals
        ):
            function.declare_local(node.identifier)


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


class _GlobalAnnotationStores(NodeVisitor):
    """Apply module contracts to writers converted before their declarations."""

    def __init__(self, converter, lambda_):
        self.converter = converter
        self.lambda_ = lambda_

    def apply(self, blocks):
        for block in blocks:
            for node in list(block.nodes()):
                node.accept(self)

    def visit_Set(self, node):
        node.var.accept(_GlobalAnnotationStore(self, node))


class _GlobalAnnotationStore(LeafVisitor):
    """Checks one ``Set`` against its variable's module contract; only
    globals have one."""

    def __init__(self, stores: _GlobalAnnotationStores, node: Set):
        self.converter = stores.converter
        self.lambda_ = stores.lambda_
        self.node = node

    def visit_GlobalVar(self, leaf):
        node = self.node
        if node.value is None:
            return
        annotation = self.converter.module_annotations.get(leaf.name)
        if annotation is None:
            return
        contract, declaration = annotation
        uses = node.value.uses
        if len(uses) == 1 and uses[0].accept(_ChecksContract(contract)):
            return
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
                declaration=("global", self.converter.namespace, leaf.name),
            ),
        )


class _ChecksContract(NodeVisitor):
    """Whether a value's producer already checks it against ``contract``."""

    def __init__(self, contract):
        self.contract = contract

    def visit_Node(self, node):
        return False

    def visit_Cast(self, node):
        return node.contract is self.contract


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
