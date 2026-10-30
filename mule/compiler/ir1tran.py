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

from dataclasses import replace
from typing import NoReturn

from mule import ast

from . import fndb
from .annotation_types import (
    DECLARATIONS,
    DictContract,
    TupleContract,
)
from .ir1tran_expr import _CallTarget, _Expressions
from .ir1tran_scope import (
    _IS_FINAL_ANNOTATION,
    _IS_GLOBAL_VAR,
    AttributeInfo,
    BindsName,
    CallInfo,
    ClassBindingInfo,
    ClassInfo,
    ClassScope,
    ComprehensionScope,
    Converter,
    FormatInfo,
    FunctionInfo,
    ImportInfo,
    InplaceInfo,
    Namespace,
    RangeState,
    UnsupportedSyntax,
    _AnnotationShadows,
    _BoundNames,
    _ConditionVisitor,
    _ContainsYield,
    _Docstring,
    _documentation,
    _ForIteration,
    _FusedIteration,
    _FusibleGeneratorExpression,
    _GeneratorFusion,
    _GlobalAnnotationStores,
    _ImportBindings,
    _LocalBindings,
    _MentionsComprehensionScope,
    _PreparedIteration,
    _RuntimeAnnotation,
    _ScopeVisitor,
    _ValueConditionVisitor,
    find_constant_leaf,
    module_policy,
)
from .ir1tran_stmt import _RangeCandidate, _Statements
from .ir1util import (
    aver,
    change_block_successor,
    ctran_starts_block,
    link_blocks,
    link_node_to_previous_ctran,
    made_function,
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
    Ctran,
    CtranKind,
    Enclose,
    Exit,
    If,
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

    def unshadowed_global(self, name: str) -> bool:
        return (
            self.class_binding(name) is None
            and self.class_reader(name) is None
            and self.variable(name).accept(_IS_GLOBAL_VAR)
        )

    def unshadowed_builtin(self, name: str) -> bool:
        shadows = self.converter.module_annotation_shadows
        return (
            self.lambda_.policy.safety < 3
            and "*" not in shadows
            and name not in shadows
            and self.unshadowed_global(name)
        )

    def fusible_generator_expression(self, node: ast.Node) -> ast.Comprehension | None:
        return node.accept(_FusibleGeneratorExpression(bool(self.class_scopes)))

    def prepare_generator_iteration(self, start, genexpr, source):
        """Obtain the outer iterator while retaining nested generator fusion."""
        iterable = genexpr.clauses[0].iterable
        nested = self.fusible_generator_expression(iterable)
        targets: set[str] = set()
        for clause in genexpr.clauses:
            clause.target.accept(_BoundNames(targets))
        if (
            nested is None
            or self.class_scopes
            or iterable.accept(_MentionsComprehensionScope(targets))
        ):
            # Match direct_outer: the nested body's names must not resolve
            # through the consuming comprehension's hidden target bindings.
            return self.prepare_iteration(start, iterable, source)
        current, outer = self.prepare_generator_iteration(start, nested, source)
        return current, _FusedIteration(nested, outer)

    def prepare_iteration(self, start, iterable, source):
        """Evaluate an iterable and obtain its iterator once, before its loop."""
        candidate = iterable.accept(_RangeCandidate())
        callee = None
        arguments = []
        current = start
        if candidate is not None:
            callee = self.temporary()
            ctran, callable_ = self.value(current, candidate.function)
            current = Ctran()
            self.set_var(ctran, current, callee, callable_, candidate.function)
            for argument in candidate.arguments:
                current, value = self.value(current, argument.value)
                arguments.append(value)
            ctran, loaded = Ctran(), Lvar()
            self.ref(current, ctran, loaded, callee, candidate)
            current, value = Ctran(), Lvar()
            self.full_call(
                ctran,
                current,
                value,
                loaded,
                tuple(arguments),
                candidate,
                CallInfo((), self.converter.location(candidate)),
            )
        else:
            current, value = self.value(current, iterable)
        iterator = self.temporary()
        ctran, iterated = Ctran(), Lvar()
        self.protocol(current, ctran, iterated, "iterate", (value,), source)
        current = Ctran()
        self.set_var(ctran, current, iterator, iterated, source)
        return current, _ForIteration(iterator, candidate, callee, tuple(arguments))

    def drive_iterable(
        self,
        start: Ctran,
        next_: Ctran,
        genexpr: ast.Comprehension,
        consumer,
        source: ast.Node,
        *,
        iteration: _PreparedIteration | None = None,
        pre_statements=(),
        post_statements=(),
        short_circuit_stop=None,
        enclosing_statement: ast.Statement | None = None,
        enclosing_body: tuple[ast.Statement, ...] = (),
        yield_statements=(),
    ) -> None:
        """Open-code iteration over ``genexpr`` by inlining ``consumer`` at its
        yield site inside a ``ComprehensionScope``, following SBCL's
        ``dx-flet`` + ``(declare (inline wrapper))`` + ``%map`` expansion in
        ``src/code/quantifiers.lisp`` and ``src/compiler/seqtran.lisp``."""
        span = genexpr.span
        targets: set[str] = set()
        for clause in genexpr.clauses:
            clause.target.accept(_BoundNames(targets))
        outer_iterable = genexpr.clauses[0].iterable
        current = start
        held = None
        if iteration is None:
            direct_outer = (
                not self.class_scopes
                and (
                    outer_iterable.accept(_RangeCandidate()) is not None
                    or self.fusible_generator_expression(outer_iterable) is not None
                )
                and not outer_iterable.accept(_MentionsComprehensionScope(targets))
            )
            if not direct_outer:
                ctran, iterable = self.value(start, outer_iterable)
                held = self.temporary()
                current = Ctran()
                self.set_var(ctran, current, held, iterable, source)
        variables = {name: self.temporary() for name in sorted(targets)}
        fusion = None
        if genexpr.kind == ast.ComprehensionKind.GENERATOR:
            yielded = ast.Name(self.temporary().name, span=span)
            fusion = _GeneratorFusion(consumer(yielded, span), span)
            body = [
                *yield_statements,
                ast.Assignment([yielded], genexpr.element, span=span),
                fusion.consumer,
            ]
        else:
            body = consumer(genexpr.element, span)
        body: list[ast.Statement]
        for index in range(len(genexpr.clauses) - 1, -1, -1):
            clause = genexpr.clauses[index]
            for condition in reversed(clause.conditions):
                body = [ast.If(condition, body, span=span)]
            target = clause.target
            if fusion is not None and index == 0:
                # Obtaining the outer iterator and advancing it are outside the
                # generator body; binding its item and running the clauses are not.
                target = ast.Name(self.temporary().name, span=span)
                fusion.protected.body = [
                    ast.Assignment([clause.target], target, span=span),
                    *body,
                ]
                body = [fusion.protected]
            clause_source = (
                clause.iterable
                if index > 0 or held is None
                else ast.Name(held.name, span=span)
            )
            for_stmt: ast.Statement = ast.For(
                target,
                clause_source,
                body,
                is_async=clause.is_async,
                span=span,
            )
            if short_circuit_stop is not None and index > 0:
                body = [
                    for_stmt,
                    ast.If(
                        short_circuit_stop(span),
                        [ast.Break(span=span)],
                        span=span,
                    ),
                ]
            else:
                body = [for_stmt]
        statements = [*pre_statements, *body, *post_statements]
        self.comprehension_scopes.append(
            ComprehensionScope(
                variables,
                len(self.class_scopes),
                enclosing_statement=enclosing_statement,
                enclosing_body=enclosing_body,
                generator_fusion=fusion,
                outer_iteration=(for_stmt, iteration)
                if iteration is not None
                else None,
            )
        )
        try:
            frontier = self.convert_body(current, statements)
        finally:
            self.comprehension_scopes.pop()
        if held is not None:
            self.discard(frontier, next_, held.name, source)
        elif frontier is not None:
            self.flow(frontier, next_)

    def collect_comprehension(
        self, start, next_, result, node, kind, *, key=None, iteration=None
    ):
        """Collect an inlined comprehension without discarding generator origin."""
        span = node.span
        accumulator = self.temporary()
        result_name = ast.Name(accumulator.name, span=span)
        yield_statements = []
        if kind == ast.ComprehensionKind.DICT:
            if key is None:
                raise ValueError("dict comprehension requires a key expression")
            initial: ast.Expression = ast.Dict([], span=span)
            key_name = ast.Name(self.temporary().name, span=span)
            key_assignment = ast.Assignment([key_name], key, span=span)
            prefix = [key_assignment]
            if node.kind == ast.ComprehensionKind.GENERATOR:
                # A fused pair evaluates both components before yielding;
                # hashing and inserting the key belong to the dict consumer.
                yield_statements = prefix
                prefix = []

            def consumer(elem, s):
                return [
                    *prefix,
                    ast.Assignment(
                        [ast.Subscript(result_name, key_name, span=s)], elem, span=s
                    ),
                ]

        else:
            if kind == ast.ComprehensionKind.LIST:
                initial, method = ast.List([], span=span), "append"
            else:
                initial, method = ast.Set([], span=span), "add"

            def consumer(elem, s):
                return [
                    ast.ExpressionStatement(
                        ast.Call(
                            ast.Attribute(result_name, method, span=s),
                            [ast.CallArgument(elem, span=s)],
                            span=s,
                        ),
                        span=s,
                    )
                ]

        ctran = Ctran()
        self.drive_iterable(
            start,
            ctran,
            node,
            consumer,
            node,
            iteration=iteration,
            pre_statements=[ast.Assignment([result_name], initial, span=span)],
            yield_statements=yield_statements,
        )
        self.ref(ctran, next_, result, accumulator, node, final=True)

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
        if var.accept(
            _IS_GLOBAL_VAR
        ) and not self.converter.final_functions.allows_store(var.name, source):
            self.error(f"cannot reassign or delete final function '{var.name}'", source)
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
        if (
            info is None
            and len(callee.uses) == 1
            and data is not None
            and not data.keywords
        ):
            info = callee.uses[0].accept(_NativeCallee(self.converter, len(operands)))
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
        if self.comprehension_scopes:
            fusion = self.comprehension_scopes[-1].generator_fusion
            if fusion is not None and fusion.convert_statement(
                self, start, next_, statement
            ):
                return
        if (
            self.comprehension_scopes
            and self.comprehension_scopes[-1].enclosing_statement is statement
        ):
            scope = self.comprehension_scopes.pop()
            try:
                frontier = self.convert_body(start, scope.enclosing_body)
                if frontier is not None:
                    self.flow(frontier, next_)
            finally:
                self.comprehension_scopes.append(scope)
            return
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
            arg_expr = node.arguments[0].value
            genexpr = self.fusible_generator_expression(arg_expr)
            if genexpr is not None:
                following, arguments = Ctran(), Lvar()
                self.collect_comprehension(
                    ctran,
                    following,
                    arguments,
                    genexpr,
                    ast.ComprehensionKind.LIST,
                )
            else:
                following, arguments = self.value(ctran, arg_expr)
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
            if argument.kind == ast.ArgumentKind.STARRED:
                genexpr = self.fusible_generator_expression(argument.value)
                if genexpr is not None:
                    pos_name = ast.Name(positional.name, span=argument.span)
                    following = Ctran()
                    self.drive_iterable(
                        current,
                        following,
                        genexpr,
                        lambda elem, s, target_name=pos_name: [
                            ast.ExpressionStatement(
                                ast.Call(
                                    ast.Attribute(target_name, "append", span=s),
                                    [ast.CallArgument(elem, span=s)],
                                    span=s,
                                ),
                                span=s,
                            )
                        ],
                        argument,
                    )
                    current = following
                    continue
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

    def apply_decorators(self, start, held, value, decorators, *, final_steps=()):
        """Call the evaluated decorators on ``value``, innermost first."""
        if not held:
            return start, value
        result = self.temporary()
        current = Ctran()
        self.set_var(start, current, result, value, decorators[-1])
        for temporary, decorator in reversed(tuple(zip(held, decorators, strict=True))):
            original = result.sets[0].value
            if (
                any(decorator is step for step in final_steps)
                and original is not None
                and made_function(original.use()) is not None
            ):
                # Keep the marker effect without hiding a proven function
                # behind the decorator's otherwise unknown call result.
                ctran, receiver = Ctran(), Lvar()
                self.ref(current, ctran, receiver, result, decorator)
                following, marker = Ctran(), Lvar()
                self.constant(ctran, following, marker, ast.BooleanLiteral(True))
                current = Ctran()
                self.known(
                    following,
                    current,
                    None,
                    fndb.SETATTR,
                    (receiver, marker),
                    decorator,
                    AttributeInfo("__final__", self.converter.location(decorator)),
                )
                continue
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
            result = self.temporary()
            self.set_var(ctran, current, result, decorated, decorator)
        ctran, value = Ctran(), Lvar()
        self.ref(current, ctran, value, result, decorators[0], final=True)
        return ctran, value

    def final_decorator_steps(self, definition):
        if not self.is_module or self.class_scopes:
            return ()
        return self.converter.final_functions.steps(definition)

    def store_function(self, start, next_, definition, value):
        declarations = self.converter.final_functions
        if (
            self.is_module
            and not self.class_scopes
            and declarations.declares(definition)
        ):
            var = self.variable(definition.name)
            var.final_function = made_function(value.use())
            store = self.set_var(start, next_, var, value, definition)
            store.final_binding = True
            return
        self.store(start, next_, definition.name, value, definition)

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


class _NativeCallee(NodeVisitor, LeafVisitor):
    """Retarget a callee reference to a once-bound `_mule_native` import
    to the primitive the export wraps, returning the primitive's entry."""

    def __init__(self, converter, arity: int):
        self.converter = converter
        self.arity = arity

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node):
        info = node.leaf.accept(self)
        if info is not None:
            node.leaf.refs.remove(node)
            node.leaf = self.converter.known(info)
            node.leaf.refs.append(node)
        return info

    def visit_Leaf(self, leaf):
        return None

    def visit_GlobalVar(self, leaf):
        return self.converter.native_primitive(leaf.name, self.arity)


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


__all__ = [
    "_IS_FINAL_ANNOTATION",
    "AttributeInfo",
    "BindsName",
    "CallInfo",
    "ClassBindingInfo",
    "ClassInfo",
    "ClassScope",
    "ComprehensionScope",
    "Converter",
    "FormatInfo",
    "FunctionConverter",
    "FunctionInfo",
    "ImportInfo",
    "InplaceInfo",
    "Namespace",
    "RangeState",
    "UnsupportedSyntax",
    "_AnnotationShadows",
    "_ContainsYield",
    "_Docstring",
    "_GlobalAnnotationStores",
    "_ImportBindings",
    "_documentation",
    "find_constant_leaf",
    "module_policy",
]
