"""Convert function definitions and module bodies into lambdas, following
``src/compiler/ir1tran-lambda.lisp``.

A Python function becomes one ``Lambda`` whose ``Bind`` node starts the body
and whose single ``Return`` node receives every ``return`` statement's value.
The module body is the top-level lambda: it runs when the module executes, and
``def`` statements inside it construct function objects at that time.
"""

from __future__ import annotations

from mule import ast

from . import fndb
from .interface_annotations import declaration as _interface_declaration
from .ir1tran import (
    Converter,
    FunctionConverter,
    UnsupportedSyntax,
    _ContainsYield,
    _Docstring,
    _documentation,
    module_policy,
)
from .ir1util import (
    block_exception_target,
    ctran_starts_block,
    link_blocks,
    link_node_to_previous_ctran,
    use_ctran,
)
from .node import (
    Bind,
    CallKind,
    CodeInfo,
    Ctran,
    FunctionalKind,
    Lambda,
    LambdaVar,
    Lvar,
    NodeVisitor,
    Return,
    Signature,
    linked,
)

_CONTAINS_YIELD = _ContainsYield()


def convert_function(
    converter: Converter,
    definition: ast.FunctionDefinition,
    policy,
    *,
    owner_class=None,
    parent=None,
    defining_class=None,
    qualname=None,
    body_kind=None,
    annotation_scope=False,
) -> Lambda:
    policy = converter.policy(definition.decorators, policy)
    # Generator, coroutine and async generator functions return an object
    # that runs their body on its own stack; that body is not wrapped again.
    yields = any(statement.accept(_CONTAINS_YIELD) for statement in definition.body)
    if body_kind is not None:
        kind = None
    elif definition.is_async:
        kind = "async_generator" if yields else "coroutine"
    else:
        kind = "generator" if yields else None
    generator = kind is not None
    interface_declaration = _interface_declaration(converter, definition, parent)
    names = []
    annotations = []
    variadic = False
    for parameter in definition.parameters:
        converter.reserved(parameter.name, parameter)
        if parameter.name in names:
            converter.error(f"duplicate parameter '{parameter.name}'", parameter)
        # Either collecting parameter needs the general binder.
        variadic |= parameter.kind in (
            ast.ParameterKind.VAR_POSITIONAL,
            ast.ParameterKind.VAR_KEYWORD,
        )
        names.append(parameter.name)
        annotations.append(
            converter.annotation(parameter.annotation)
            if parameter.annotation is not None
            else None
        )
    qualname = qualname or (
        parent.qualified(definition.name) if parent is not None else None
    )
    qualified = qualname or definition.name
    sanitized = "".join(
        ch if " " < ch <= "~" and ch not in ('"', "\\") else f"_u{ord(ch):x}_"
        for ch in qualified
    )
    count = converter.function_names.get(sanitized, 0) + 1
    converter.function_names[sanitized] = count
    suffix = sanitized if count == 1 else f"{sanitized}.{count}"
    lambda_ = Lambda(
        f"{converter.prefix}mule_fn_{suffix}",
        definition.name,
        policy=policy,
        namespace=converter.namespace,
    )
    lambda_.coroutine_body = body_kind is not None
    lambda_.code = CodeInfo(
        definition.name,
        qualified,
        converter.filename,
        definition.source_start().line,
        3
        | (16 if parent is not None and not parent.is_module else 0)
        | {"generator": 32, "coroutine": 128, "async_generator": 512}.get(
            kind or body_kind, 0
        ),
    )
    lambda_.signature = Signature(
        kinds=tuple(parameter.kind for parameter in definition.parameters),
        default_parameters=tuple(
            index
            for index, parameter in enumerate(definition.parameters)
            if parameter.default is not None
        ),
        default_expressions=tuple(
            parameter.default
            for parameter in definition.parameters
            if parameter.default is not None
        ),
        variadic=variadic,
        annotations=tuple(
            None
            if parameter.kind
            in (ast.ParameterKind.VAR_POSITIONAL, ast.ParameterKind.VAR_KEYWORD)
            else contract
            for parameter, contract in zip(
                definition.parameters, annotations, strict=True
            )
        ),
        # A generator function returns a generator; its annotation describes
        # what the generator produces, not a value to check on return.
        # An interface annotation remains metadata, never a body assertion.
        return_annotation=(
            converter.annotation(definition.return_annotation)
            if definition.return_annotation is not None
            and (not generator or interface_declaration)
            else None
        ),
        annotation_expressions=tuple(
            parameter.annotation for parameter in definition.parameters
        ),
        return_annotation_expression=definition.return_annotation,
        interface_declaration=interface_declaration,
    )
    lambda_.owner_class = owner_class
    if defining_class is not None and definition.parameters:
        lambda_.method_class = defining_class.identity
    converter.functions.append(lambda_)
    for name in names:
        var = LambdaVar(name)
        var.home = lambda_
        lambda_.vars.append(var)
        lambda_.locals.append(var)
    function = FunctionConverter(
        converter,
        lambda_,
        [] if generator else definition.body,
        is_module=False,
        origin=definition,
        parent=parent,
        defining_class=defining_class,
        qualname=qualname,
        body_kind=body_kind,
        annotation_scope=annotation_scope,
    )

    def prologue(start: Ctran) -> Ctran:
        """Assert parameter annotations before the body runs."""
        current = start
        for parameter, contract, var in zip(
            definition.parameters, annotations, lambda_.vars, strict=True
        ):
            if contract is None:
                continue
            if parameter.kind in (
                ast.ParameterKind.VAR_POSITIONAL,
                ast.ParameterKind.VAR_KEYWORD,
            ):
                element = function.temporary()
                function.annotations[element.name] = (contract, parameter)
                values = ast.Name(parameter.name, span=parameter.span)
                if parameter.kind == ast.ParameterKind.VAR_KEYWORD:
                    values = ast.Call(
                        ast.Attribute(values, "values", span=parameter.span),
                        span=parameter.span,
                    )
                loop = ast.For(
                    ast.Name(element.name, span=parameter.span),
                    values,
                    [],
                    span=parameter.span,
                )
                ctran = Ctran()
                function.convert_statement(current, ctran, loop)
                current = Ctran()
                function.set_var(ctran, current, element, None, parameter)
                continue
            function.annotations[parameter.name] = (contract, parameter)
            lambda_.local_annotations[parameter.name] = contract
            ctran, loaded = Ctran(), Lvar()
            function.load(current, ctran, loaded, parameter.name, parameter, final=True)
            current = Ctran()
            function.store(ctran, current, parameter.name, loaded, parameter)
            if var.sets:
                var.arg_info = var.sets[-1]
        return current

    if generator:
        convert_lambda_body(
            function,
            [],
            lambda start: _start_generator(function, definition, kind, prologue(start)),
            definition,
        )
        return lambda_
    convert_lambda_body(function, definition.body, prologue, definition)
    if owner_class is not None and definition.name == "__init__":
        validate_initialization(
            converter,
            lambda_,
            converter.class_fields[owner_class],
            converter.location(definition),
        )
    return lambda_


_FACTORIES = {
    "generator": "make_generator",
    "coroutine": "make_coroutine",
    "async_generator": "make_async_generator",
}


def _start_generator(function, definition, kind, start):
    """A generator function's body: after binding and checking its arguments,
    return a generator that will run the original body on its own stack.

    The body becomes an inner function taking the bound parameters, so its
    parameters and locals behave exactly as they would in the outer frame.
    It shares the outer function's ``__class__`` cell and qualified name."""
    span = definition.span
    body = ast.FunctionDefinition(
        definition.name,
        [
            ast.Parameter(parameter.name, ast.ParameterKind.POSITIONAL_ONLY, span=span)
            for parameter in definition.parameters
        ],
        definition.body,
        span=span,
    )
    lambda_ = function.lambda_
    inner = function.converter.function(
        body,
        lambda_.policy,
        parent=function,
        defining_class=function.defining_class,
        qualname=function.qualname,
        body_kind=kind,
    )
    lambda_.generator_body = inner
    inner.generator_outer = lambda_
    ctran, created = Ctran(), Lvar()
    function.define_function(start, ctran, created, inner, body, definition.name)
    following, arguments = Ctran(), Lvar()
    function.known(
        ctran,
        following,
        arguments,
        fndb.BUILD_TUPLE,
        tuple(
            ast.Name(parameter.name, span=span) for parameter in definition.parameters
        ),
        definition,
    )
    ctran = Ctran()
    function.protocol(
        following,
        ctran,
        lambda_.return_.result,
        _FACTORIES[kind],
        (
            created,
            arguments,
            ast.TextLiteral(definition.name, span=span),
            ast.TextLiteral(function.qualname or definition.name, span=span),
        ),
        definition,
    )
    function.jump(ctran, lambda_.return_.block)
    # Nothing follows the return; the caller's implicit None is unreachable.
    return function.block().start


class _MangleNames(ast.Visitor):
    """Mangle ``__name`` private identifiers inside class scopes (CPython ``_Py_Mangle``)."""

    def __init__(self):
        self.class_name: str | None = None

    def mangle(self, name: str | None) -> str | None:
        if (
            name is None
            or self.class_name is None
            or not name.startswith("__")
            or name.endswith("__")
            or "." in name
        ):
            return name
        prefix = self.class_name.lstrip("_")
        if not prefix:
            return name
        return f"_{prefix}{name}"

    def visit_Node(self, node):
        for child in node.children():
            child.accept(self)

    def visit_Name(self, node: ast.Name):
        node.identifier = self.mangle(node.identifier)

    def visit_Attribute(self, node: ast.Attribute):
        node.value.accept(self)
        node.attribute = self.mangle(node.attribute)

    def visit_Parameter(self, node: ast.Parameter):
        node.name = self.mangle(node.name)
        self.visit_Node(node)

    def visit_Global(self, node: ast.Global):
        node.identifiers = [self.mangle(name) for name in node.identifiers]

    def visit_Nonlocal(self, node: ast.Nonlocal):
        node.identifiers = [self.mangle(name) for name in node.identifiers]

    def visit_ExceptionHandler(self, node: ast.ExceptionHandler):
        node.binding = self.mangle(node.binding)
        self.visit_Node(node)

    def visit_ImportItem(self, node: ast.ImportItem):
        if node.alias is not None:
            node.alias = self.mangle(node.alias)
        elif "." not in node.name:
            mangled = self.mangle(node.name)
            if mangled != node.name:
                node.alias = mangled

    def visit_TypeParameter(self, node: ast.TypeParameter):
        node.name = self.mangle(node.name)
        self.visit_Node(node)

    def visit_TypeAlias(self, node: ast.TypeAlias):
        node.name = self.mangle(node.name)
        self.visit_Node(node)

    def visit_CapturePattern(self, node: ast.CapturePattern):
        node.name = self.mangle(node.name)

    def visit_StarPattern(self, node: ast.StarPattern):
        node.name = self.mangle(node.name)

    def visit_MappingPattern(self, node: ast.MappingPattern):
        node.rest = self.mangle(node.rest)
        self.visit_Node(node)

    def visit_KeywordPattern(self, node: ast.KeywordPattern):
        node.name = self.mangle(node.name)
        self.visit_Node(node)

    def visit_AsPattern(self, node: ast.AsPattern):
        node.name = self.mangle(node.name)
        self.visit_Node(node)

    def visit_FunctionDefinition(self, node: ast.FunctionDefinition):
        node.name = self.mangle(node.name)
        self.visit_Node(node)

    def visit_ClassDefinition(self, node: ast.ClassDefinition):
        for decorator in node.decorators:
            decorator.accept(self)
        for type_parameter in node.type_parameters:
            type_parameter.accept(self)
        for argument in node.arguments:
            argument.accept(self)
        raw_name = node.name
        node.name = self.mangle(node.name)
        saved = self.class_name
        self.class_name = raw_name
        try:
            for statement in node.body:
                statement.accept(self)
        finally:
            self.class_name = saved


def convert_module(converter: Converter, tree: ast.Module) -> Lambda:
    """Convert the module body into the top-level lambda executed at load."""
    tree.accept(_MangleNames())
    entry = Lambda(
        f"{converter.prefix}mule_module",
        None,
        policy=module_policy(converter, tree.statements),
        namespace=converter.namespace,
    )
    entry.kind = FunctionalKind.TOPLEVEL
    entry.entry_fun = entry
    entry.code = CodeInfo("<module>", "<module>", converter.filename, 1)
    converter.functions.append(entry)
    statements = tree.statements
    docstring = ast.NoneLiteral(None, span=tree.span)
    if statements and statements[0].accept(_Docstring()):
        docstring = _documentation(statements)
        statements = statements[1:]
    statements = [
        ast.Assignment(
            [ast.Name("__doc__", span=docstring.span)], docstring, span=docstring.span
        ),
        *statements,
    ]
    function = FunctionConverter(
        converter, entry, statements, is_module=True, origin=tree
    )
    convert_lambda_body(function, statements, None, tree)
    return entry


def convert_lambda_body(function: FunctionConverter, statements, prologue, origin):
    """Build the Bind and Return nodes around a converted body."""
    lambda_ = function.lambda_
    component = function.component
    lexenv = function.lexenv
    bind = Bind(lexenv, origin)
    bind.lambda_ = lambda_
    lambda_.bind = bind
    result_ctran, result = Ctran(), Lvar()
    return_block = ctran_starts_block(result_ctran, component)
    returned = Return(lexenv, origin, result, lambda_)
    lambda_.return_ = returned
    result.dest = returned
    link_node_to_previous_ctran(returned, result_ctran)
    return_block.last = returned
    link_blocks(return_block, component.tail)
    saved = component.last_block
    # Body blocks are inserted before the return block (WITH-COMPONENT-LAST-BLOCK).
    component.last_block = return_block
    try:
        prebind = Ctran()
        bind_block = ctran_starts_block(prebind, component)
        link_blocks(component.head, bind_block)
        link_node_to_previous_ctran(bind, prebind)
        postbind = Ctran()
        use_ctran(bind, postbind)
        current = prologue(postbind) if prologue is not None else postbind
        frontier = function.convert_body(current, statements)
        if frontier is not None:
            annotation = lambda_.signature.implementation_return_annotation
            ctran = Ctran()
            delivered = Lvar() if annotation is not None else result
            function.constant(frontier, ctran, delivered, ast.NoneLiteral(None))
            if annotation is not None:
                following = Ctran()
                function.cast(
                    ctran,
                    following,
                    result,
                    delivered,
                    annotation,
                    origin,
                    declaration=("return", lambda_),
                )
                ctran = following
            function.jump(ctran, return_block)
    finally:
        component.last_block = saved


def lambda_blocks(lambda_: Lambda) -> list:
    """Blocks reachable from the lambda's bind block, following normal and
    error edges, in discovery order."""
    seen = []
    seen_set = set()
    pending = [linked(lambda_.bind, lambda_, "bind").block]
    while pending:
        block = pending.pop()
        if block in seen_set or block.start is None:
            continue
        seen_set.add(block)
        seen.append(block)
        pending.extend(block.succ)
        target = block_exception_target(block)
        if target is not None:
            pending.append(target)
    return seen


class _Receivers(NodeVisitor):
    """Lvars that hold the constructor's receiver."""

    def __init__(self, var: LambdaVar):
        self.values = {ref.lvar for ref in var.refs if ref.lvar is not None}


class _Writes(NodeVisitor):
    def __init__(self, receivers, initialized):
        self.receivers = receivers
        self.initialized = initialized

    def visit_Combination(self, node):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info.name == "%setattr"
            and node.args[0] in self.receivers
        ):
            self.initialized.add(node.info.name)


class _Uses(_Writes):
    def __init__(self, converter, receivers, initialized, fields, receiver, location):
        super().__init__(receivers, initialized)
        self.converter = converter
        self.fields = fields
        self.receiver = receiver
        self.location = location

    def require_initialized(self, location=None):
        missing = self.fields - self.initialized
        if missing:
            raise UnsupportedSyntax(
                "typed native initialization must assign every field before "
                "reading or exposing the instance or returning; missing "
                + ", ".join(sorted(missing)),
                location or self.location,
            )

    def source_location(self, node):
        if node.source is not None and node.source.span is not None:
            return self.converter.location(node.source)
        return self.location

    def visit_Node(self, node):
        if any(operand in self.receivers for operand in _operands(node)):
            self.require_initialized()

    def visit_Set(self, node):
        if node.var is self.receiver:
            raise UnsupportedSyntax(
                "typed native initialization cannot rebind its receiver",
                self.source_location(node),
            )
        if node.value in self.receivers:
            self.require_initialized(self.source_location(node))

    def visit_Combination(self, node):
        if node.kind is CallKind.FULL:
            if any(operand in self.receivers for operand in _operands(node)):
                self.require_initialized(node.info.location)
            return
        name = node.fun_info.name
        if name == "%setattr":
            if node.args[1] in self.receivers:
                self.require_initialized(node.info.location)
            super().visit_Combination(node)
            return
        if name in ("%getattr", "%resolve_method"):
            if node.args[0] in self.receivers:
                self.require_initialized(node.info.location)
            return
        if name == "%resolve_super_method":
            if node.args[2] in self.receivers:
                self.require_initialized(node.info.location)
            return
        self.visit_Node(node)

    def visit_Return(self, node):
        self.require_initialized()


def _operands(node):
    return node.accept(_Operands())


class _Operands(NodeVisitor):
    def visit_Node(self, node):
        return ()

    def visit_If(self, node):
        return (node.test,)

    def visit_Set(self, node):
        return () if node.value is None else (node.value,)

    def visit_Combination(self, node):
        return (node.fun, *node.args)

    def visit_Return(self, node):
        return (node.result,)

    def visit_Cast(self, node):
        return (node.value,)

    def visit_Exit(self, node):
        return () if node.value is None else (node.value,)


def validate_initialization(converter, lambda_: Lambda, declarations, location):
    """Establish the raw-slot invariant using forward CFG must analysis.

    Every declared native field must be written before the receiver is read,
    exposed, or the constructor returns. Error edges contribute the state at
    the raising block's entry, since any instruction in it may raise.
    """
    fields = set(declarations)
    receiver = lambda_.vars[0]
    receivers = _Receivers(receiver).values
    blocks = lambda_blocks(lambda_)
    entry = linked(lambda_.bind, lambda_, "bind").block
    predecessors = {block: [] for block in blocks}
    error_predecessors = {block: [] for block in blocks}
    for block in blocks:
        for successor in block.succ:
            if successor in predecessors:
                predecessors[successor].append(block)
        target = block_exception_target(block)
        if target is not None:
            error_predecessors[target].append(block)
    # The greatest fixed point of intersection starts non-entry blocks at top.
    incoming = {block: set(fields) for block in blocks}
    outgoing = {block: set(fields) for block in blocks}
    changed = True
    while changed:
        changed = False
        for block in blocks:
            if block is entry:
                initialized = set()
            else:
                states = [outgoing[pred] for pred in predecessors[block]]
                states.extend(incoming[pred] for pred in error_predecessors[block])
                initialized = set.intersection(*states) if states else set(fields)
            incoming[block] = set(initialized)
            writes = _Writes(receivers, initialized)
            for node in block.nodes():
                node.accept(writes)
            if outgoing[block] != initialized:
                outgoing[block] = initialized
                changed = True
    for block in blocks:
        uses = _Uses(
            converter, receivers, set(incoming[block]), fields, receiver, location
        )
        for node in block.nodes():
            node.accept(uses)
