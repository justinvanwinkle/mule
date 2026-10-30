"""IR1 translation metadata, module Converter, lexical/class scope bindings, and AST pre-scan visitors."""

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
from .final_functions import FinalFunctions
from .ir1util import (
    insert_cast_before,
)
from .node import (
    Block,
    Cast,
    Cleanup,
    CleanupKind,
    Component,
    Constant,
    Ctran,
    GlobalVar,
    KnownFunction,
    Lambda,
    LambdaVar,
    LeafVisitor,
    Lvar,
    NodeVisitor,
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
    enclosing_statement: ast.Statement | None = None
    enclosing_body: tuple[ast.Statement, ...] = ()
    generator_fusion: _GeneratorFusion | None = None
    outer_iteration: tuple[ast.For, _PreparedIteration] | None = None


class _PreparedIteration:
    """Preparation consumed by a physical loop or an inlined generator loop."""

    __slots__ = ()

    def convert(self, statements, start, next_, node):
        raise NotImplementedError("prepared iteration has no loop lowering")


@dataclass(slots=True)
class _ForIteration(_PreparedIteration):
    """Owned iterator and optional range-call evidence consumed by for lowering."""

    iterator: LambdaVar
    candidate: ast.Call | None
    callee: LambdaVar | None
    arguments: tuple[Lvar, ...]

    def convert(self, statements, start, next_, node):
        statements.for_iteration(start, next_, node, self)


@dataclass(slots=True)
class _FusedIteration(_PreparedIteration):
    """A generator body remains deferred after its outer iterator is obtained."""

    expression: ast.Comprehension
    outer: _PreparedIteration

    def convert(self, statements, start, next_, node):
        statements.for_generator(start, next_, node, self.expression, self.outer)


@dataclass(slots=True)
class _InlineBody(ast.Pass):
    """A conversion-time suite that remains visible to source-tree visitors."""

    body: list[ast.Statement]

    def children(self):
        yield from self.body


class _GeneratorFusion:
    """Retain a generator's exception boundary after its consumer is inlined."""

    def __init__(self, body, span):
        self.protected = _InlineBody([], span=span)
        self.consumer = _InlineBody(body, span=span)
        self.outer_cleanup: Cleanup | None = None

    def convert_statement(self, function, start, next_, statement) -> bool:
        if statement is self.protected:
            self.convert_body(function, start, next_)
            return True
        if statement is not self.consumer:
            return False
        # Yielded-value consumers execute in the caller, not the generator.
        # Start fresh blocks on both sides: error destinations belong to blocks.
        done = function.block()
        with function.with_cleanup(self.outer_cleanup):
            body = function.block()
            function.jump(start, body)
            frontier = function.convert_body(body.start, self.consumer.body)
            if frontier is not None:
                function.jump(frontier, done)
        function.flow(done.start, next_)
        return True

    def convert_body(self, function, start, next_):
        self.outer_cleanup = function.lexenv.cleanup
        dispatch, done = function.block(), function.block()
        region = Cleanup(
            CleanupKind.HANDLER_REGION,
            target=dispatch,
            parent=self.outer_cleanup,
        )
        with function.with_cleanup(region):
            body = function.block()
            function.jump(start, body)
            frontier = function.convert_body(body.start, self.protected.body)
            if frontier is not None:
                function.jump(frontier, done)
        caught_ctran, caught = Ctran(), Lvar()
        function.known(
            dispatch.start,
            caught_ctran,
            caught,
            fndb.CATCH_EXCEPTION,
            (),
            self.protected,
        )
        resumed, error = Ctran(), Lvar()
        function.protocol(
            caught_ctran,
            resumed,
            error,
            "generator_exception",
            (caught,),
            self.protected,
        )
        function.known(
            resumed, None, None, fndb.RESUME_EXCEPTION, (error,), self.protected
        )
        function.flow(done.start, next_)


@dataclass(slots=True, eq=False)
class ClassScope:
    """A class body being converted inline in its enclosing function.

    Python executes the body with its own namespace: names it binds are read
    from that namespace first and then from the module globals and builtins,
    never from enclosing function scopes; other names resolve as they would
    in the enclosing function. Functions defined in the body skip the class
    scope entirely, and see the class itself only through ``__class__``.
    """

    owner: object
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
        self.function_converters: dict[Lambda, object] = {}
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
        # Module bindings made by `from _mule_native import export`, by local
        # name, and how many times the module binds each name anywhere.
        self.native_imports: dict[str, str] = {}
        self.module_binding_counts: dict[str, int] = {}
        self.final_functions = FinalFunctions({})
        self.interface_bindings = {}
        # Set once the module's lambdas have their own components. Functions
        # defined later, by cleanup code converted during environment
        # analysis, collect in fresh initial components to split then.
        self.split = False
        self.converting_late = False
        self.late: list[tuple[Component, list[Lambda]]] = []

    def native_primitive(self, name: str, arity: int):
        """The primitive a positional call of global ``name`` with ``arity``
        arguments compiles to when the module binds the name exactly once, by
        importing a `_mule_native` export that wraps that primitive; otherwise
        None. Other uses of the name still read the imported function."""
        export = self.native_imports.get(name)
        if export is None or self.module_binding_counts.get(name) != 1:
            return None
        return self.primitives.get(f"_mule_native.{export}/{arity}")

    def class_identity(self, node: ast.ClassDefinition) -> str:
        identity = self.class_identities.get(id(node))
        if identity is None:
            if self.prefix == "mule_sequences_" and node.name == "bytearray":
                identity = "mule_sequences_bytearray"
            elif (
                self.primitives
                and self.namespace == "_dict"
                and node.name in ("set", "frozenset")
            ):
                # The compiled `_dict` protocol module defines the builtin
                # set classes; SetType contracts guard on these identities.
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


class _IsUnpackNode(ast.Visitor):
    def visit_Node(self, node):
        return False

    def visit_Unpack(self, node: ast.Unpack):
        return True


_IS_UNPACK_NODE = _IsUnpackNode()


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


class _FinalBinding(_CompilerBinding):
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
_EXPORT_BINDINGS["final"] = _FinalBinding()
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
        if node.module == "_mule_native" and not node.level:
            for item in node.items:
                self.converter.native_imports[item.alias or item.name] = item.name
            return
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
    """The compiler binding a decorator calls or names, such as the imported
    ``mule_types.optimize`` and ``mule_types.final`` directives."""

    def __init__(self, converter):
        self.converter = converter

    def visit_Node(self, node):
        return _OPAQUE_BINDING

    def visit_Name(self, node):
        binding = node.accept(_AnnotationReference(self.converter))
        return _OPAQUE_BINDING if binding.runtime_decorators(node) else binding

    def visit_Attribute(self, node):
        binding = node.accept(_AnnotationReference(self.converter))
        return _OPAQUE_BINDING if binding.runtime_decorators(node) else binding

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


class _StaticAttributes(ast.Visitor):
    """Names a class body assigns as ``self.NAME``: ``__static_attributes__``.

    Follows CPython's ``_PyCompile_MaybeAddStaticAttributeToClass``: any
    stored attribute of a name spelled ``self`` counts toward the nearest
    enclosing class, at any function depth, whatever the receiver parameter is
    called. Augmented assignment and ``del`` do not store through the
    attribute expression, and a nested class body records its own names.
    """

    def __init__(self, names):
        self.targets = _ReceiverFieldTarget("self", names, None)

    @classmethod
    def of(cls, definition):
        names = {}
        collector = cls(names)
        for statement in definition.body:
            statement.accept(collector)
        return sorted(names)

    def visit_Node(self, node):
        for target in node.assigned_targets():
            target.accept(self.targets)
        self.visit_children(node)

    def visit_children(self, node):
        for child in node.children():
            child.accept(self)

    def visit_AugmentedAssignment(self, node):
        self.visit_children(node)

    def visit_Delete(self, node):
        self.visit_children(node)

    def visit_AnnotatedAssignment(self, node):
        if node.value is not None:
            self.visit_Node(node)
        else:
            self.visit_children(node)

    def visit_ClassDefinition(self, node):
        for expression in (*node.arguments, *node.decorators):
            expression.accept(self)


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


class _ContainsComprehensionFrameIntrospection(ast.Visitor):
    """Whether a generator expression body inspects its own function frame."""

    def visit_Node(self, node) -> bool:
        return any(child.accept(self) for child in node.children())

    def visit_Name(self, node: ast.Name) -> bool:
        return node.identifier in ("locals", "vars", "dir", "eval", "exec", "super")

    def visit_Lambda(self, node) -> bool:
        return any(
            parameter.default is not None and parameter.default.accept(self)
            for parameter in node.parameters
        )

    def visit_FunctionDefinition(self, node) -> bool:
        return any(decorator.accept(self) for decorator in node.decorators) or any(
            parameter.default is not None and parameter.default.accept(self)
            for parameter in node.parameters
        )

    def visit_ClassDefinition(self, node) -> bool:
        return any(child.accept(self) for child in (*node.decorators, *node.arguments))


class _FusibleGeneratorExpression(ast.Visitor):
    """Return ``node`` when it is a synchronous, non-yielding generator
    expression that can be inlined into the current function scope."""

    def __init__(self, has_class_scopes: bool):
        self.has_class_scopes = has_class_scopes

    def visit_Node(self, node):
        return None

    def visit_Comprehension(self, node: ast.Comprehension):
        if node.kind != ast.ComprehensionKind.GENERATOR:
            return None
        if self.has_class_scopes and node.accept(_ComprehensionAssignment()):
            return None
        async_check = _ContainsAsync()
        yield_check = _ContainsYield()
        frame_check = _ContainsComprehensionFrameIntrospection()
        if any(
            child.accept(async_check)
            or child.accept(yield_check)
            or child.accept(frame_check)
            for child in node.children()
        ):
            return None
        return node


class _MentionsComprehensionScope(ast.Visitor):
    """Whether evaluating an outer comprehension iterable inside
    ``ComprehensionScope`` could observe the comprehension scope."""

    def __init__(self, targets: set[str]):
        self.targets = targets

    def visit_Node(self, node) -> bool:
        return any(child.accept(self) for child in node.children())

    def visit_Name(self, node: ast.Name) -> bool:
        return node.identifier in self.targets or node.identifier in (
            "locals",
            "vars",
            "dir",
            "eval",
            "exec",
            "super",
        )

    def visit_Yield(self, node: ast.Yield) -> bool:
        return True


class _ContainsBreak(ast.Visitor):
    """Whether a statement tree contains a ``break`` targeting the enclosing loop."""

    def visit_Node(self, node) -> bool:
        return any(child.accept(self) for child in node.children())

    def visit_Break(self, node) -> bool:
        return True

    def visit_For(self, node) -> bool:
        return any(stmt.accept(self) for stmt in node.else_body)

    def visit_While(self, node) -> bool:
        return any(stmt.accept(self) for stmt in node.else_body)

    def visit_FunctionDefinition(self, node) -> bool:
        return False

    def visit_ClassDefinition(self, node) -> bool:
        return False


class _PlainPairElements(ast.Visitor):
    """Return ``(key, value)`` when ``node`` is a 2-element tuple or list
    display without starred unpacking."""

    def __init__(self):
        self.in_container = False

    def visit_Node(self, node):
        return None

    def _sequence(self, node):
        if self.in_container or len(node.elements) != 2:
            return None
        self.in_container = True
        for element in node.elements:
            if element.accept(self) is False:
                return None
        return node.elements[0], node.elements[1]

    visit_Tuple = _sequence
    visit_List = _sequence

    def visit_Unpack(self, node):
        return False


class _IsTextOrBytesName(ast.Visitor):
    def visit_Node(self, node) -> bool:
        return False

    def visit_Name(self, node: ast.Name) -> bool:
        return node.identifier in ("str", "bytes", "bytearray")


class _IsRejectedSumStart(ast.Visitor):
    """Whether a ``sum`` start expression may evaluate to ``str``, ``bytes``,
    or ``bytearray``."""

    def visit_Node(self, node) -> bool:
        return False

    def visit_TextLiteral(self, node) -> bool:
        return True

    def visit_BytesLiteral(self, node) -> bool:
        return True

    def visit_FormattedString(self, node) -> bool:
        return True

    def visit_Call(self, node: ast.Call) -> bool:
        return node.function.accept(_IS_TEXT_OR_BYTES_NAME)


_CONTAINS_BREAK = _ContainsBreak()
_CONTAINS_YIELD = _ContainsYield()
_IS_TEXT_OR_BYTES_NAME = _IsTextOrBytesName()
_IS_REJECTED_SUM_START = _IsRejectedSumStart()


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

    def visit_Name(self, node):
        binding = node.accept(_StaticAnnotationReference(self.converter))
        return _OPAQUE_BINDING if binding.runtime_decorators(node) else binding

    def visit_Attribute(self, node):
        binding = node.accept(_StaticAnnotationReference(self.converter))
        return _OPAQUE_BINDING if binding.runtime_decorators(node) else binding

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


class _PlainTupleElements(ast.Visitor):
    """A tuple display's elements when none is ``*iterable``, else None."""

    def visit_Node(self, node):
        return None

    def visit_Tuple(self, node: ast.Tuple):
        if any(element.accept(_IS_UNPACK_NODE) for element in node.elements):
            return None
        return node.elements


_PLAIN_TUPLE_ELEMENTS = _PlainTupleElements()


def _simple_percent_pieces(pattern: str) -> list[tuple[str, str | None]] | None:
    """Split a ``%`` pattern into literal text and ``%s``/``%r``/``%a``
    fields, as ``(text, None)`` and ``("", conversion)``. None when the
    pattern uses keys, flags, widths, precisions, or other conversions."""
    pieces: list[tuple[str, str | None]] = []
    literal = []
    index = 0
    while index < len(pattern):
        percent = pattern.find("%", index)
        if percent < 0:
            literal.append(pattern[index:])
            break
        literal.append(pattern[index:percent])
        if percent + 1 >= len(pattern):
            return None
        conversion = pattern[percent + 1]
        if conversion == "%":
            literal.append("%")
        elif conversion in "sra":
            if any(literal):
                pieces.append(("".join(literal), None))
            literal = []
            pieces.append(("", conversion))
        else:
            return None
        index = percent + 2
    if any(literal):
        pieces.append(("".join(literal), None))
    return pieces


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
        # Only the first iterable is evaluated in this scope, but an
        # assignment expression inside binds here (PEP 572).
        node.clauses[0].iterable.accept(self)
        for name in _comprehension_assignments(node):
            self.bind(name)

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
