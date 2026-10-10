"""Convert function definitions and module bodies into lambdas, following
``src/compiler/ir1tran-lambda.lisp``.

A Python function becomes one ``Lambda`` whose ``Bind`` node starts the body
and whose single ``Return`` node receives every ``return`` statement's value.
The module body is the top-level lambda: it runs when the module executes, and
``def`` statements inside it construct function objects at that time.
"""

from __future__ import annotations

from mule import ast

from .ir1tran import Converter, FunctionConverter, UnsupportedSyntax
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
    Ctran,
    FunctionalKind,
    Lambda,
    LambdaVar,
    Lvar,
    NodeVisitor,
    Return,
    Signature,
)
from .policy import OptimizationPolicy


def convert_function(
    converter: Converter,
    definition: ast.FunctionDefinition,
    policy,
    *,
    owner_class=None,
) -> Lambda:
    if definition.is_async or definition.type_parameters:
        converter.error(
            "async and generic functions are not supported yet",
            definition,
            unsupported=True,
        )
    policy = converter.policy(definition.decorators, policy)
    names = []
    annotations = []
    variadic = False
    for parameter in definition.parameters:
        converter.reserved(parameter.name, parameter)
        if parameter.name in names:
            converter.error(f"duplicate parameter '{parameter.name}'", parameter)
        if parameter.kind == ast.ParameterKind.VAR_KEYWORD:
            converter.error(
                "**kwargs parameters are not supported yet",
                parameter,
                unsupported=True,
            )
        variadic |= parameter.kind == ast.ParameterKind.VAR_POSITIONAL
        names.append(parameter.name)
        annotations.append(
            converter.annotation(parameter.annotation)
            if parameter.annotation is not None
            else None
        )
    lambda_ = Lambda(
        f"{converter.prefix}mule_fn_{len(converter.functions)}",
        definition.name,
        policy=policy,
        namespace=converter.namespace,
    )
    lambda_.signature = Signature(
        kinds=tuple(parameter.kind for parameter in definition.parameters),
        default_parameters=tuple(
            index
            for index, parameter in enumerate(definition.parameters)
            if parameter.default is not None
        ),
        variadic=variadic,
        annotations=tuple(
            None if parameter.kind == ast.ParameterKind.VAR_POSITIONAL else contract
            for parameter, contract in zip(
                definition.parameters, annotations, strict=True
            )
        ),
        return_annotation=(
            converter.annotation(definition.return_annotation)
            if definition.return_annotation is not None
            else None
        ),
        annotation_expressions=tuple(
            parameter.annotation for parameter in definition.parameters
        ),
        return_annotation_expression=definition.return_annotation,
    )
    lambda_.owner_class = owner_class
    converter.functions.append(lambda_)
    for name in names:
        var = LambdaVar(name)
        var.home = lambda_
        lambda_.vars.append(var)
        lambda_.locals.append(var)
    function = FunctionConverter(
        converter, lambda_, definition.body, is_module=False, origin=definition
    )

    def prologue(start: Ctran) -> Ctran:
        """Assert parameter annotations before the body runs."""
        current = start
        for parameter, contract in zip(definition.parameters, annotations, strict=True):
            if contract is None:
                continue
            if parameter.kind == ast.ParameterKind.VAR_POSITIONAL:
                element = function.temporary()
                function.annotations[element.name] = (contract, parameter)
                loop = ast.For(
                    ast.Name(element.name, span=parameter.span),
                    ast.Name(parameter.name, span=parameter.span),
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
        return current

    convert_lambda_body(function, definition.body, prologue, definition)
    if owner_class is not None and definition.name == "__init__":
        validate_initialization(
            converter,
            lambda_,
            converter.class_fields[owner_class],
            converter.location(definition),
        )
    return lambda_


def convert_module(converter: Converter, tree: ast.Module) -> Lambda:
    """Convert the module body into the top-level lambda executed at load."""
    entry = Lambda(
        f"{converter.prefix}mule_module",
        None,
        policy=OptimizationPolicy(),
        namespace=converter.namespace,
    )
    entry.kind = FunctionalKind.TOPLEVEL
    entry.entry_fun = entry
    converter.functions.append(entry)
    function = FunctionConverter(
        converter, entry, tree.statements, is_module=True, origin=tree
    )
    convert_lambda_body(function, tree.statements, None, tree)
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
            annotation = lambda_.signature.return_annotation
            ctran = Ctran()
            delivered = Lvar() if annotation is not None else result
            function.constant(frontier, ctran, delivered, ast.NoneLiteral(None))
            if annotation is not None:
                following = Ctran()
                function.cast(ctran, following, result, delivered, annotation, origin)
                ctran = following
            function.jump(ctran, return_block)
    finally:
        component.last_block = saved


def lambda_blocks(lambda_: Lambda) -> list:
    """Blocks reachable from the lambda's bind block, following normal and
    error edges, in discovery order."""
    seen = []
    pending = [lambda_.bind.block]
    while pending:
        block = pending.pop()
        if block in seen or block.start is None:
            continue
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
    entry = lambda_.bind.block
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
