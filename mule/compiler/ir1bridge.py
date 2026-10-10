"""Lower IR1 to the current control-flow IR consumed by analysis and LLVM.

This bridge exists only until the analysis, representation selection, and
LLVM emission phases consume IR1 directly; it then disappears with ``ir.py``.
It performs no optimization: every IR1 node becomes the instruction the
previous frontend emitted for the same construct, and every block that lies in
a handler region is split per instruction so the handler-edge analysis sees
the state before each operation.
"""

from __future__ import annotations

from mule import ast

from . import ir
from .ir1util import block_exception_target
from .node import (
    CallKind,
    Combination,
    Constant,
    Exit,
    GlobalVar,
    If,
    KnownFunction,
    Lambda,
    LambdaVar,
    NodeVisitor,
    Ref,
)

_BINARY = {f"%{operator.name.lower()}": operator for operator in ast.BinaryOperator}
_INPLACE = {f"%i{operator.name.lower()}": operator for operator in ast.BinaryOperator}
_UNARY = {f"%unary_{operator.name.lower()}": operator for operator in ast.UnaryOperator}
_COMPARE = {
    f"%compare_{operator.name.lower()}": operator for operator in ast.ComparisonOperator
}


def lower_module(compilation) -> ir.Module:
    functions = [
        lower_lambda(lambda_, compilation.facts[lambda_] if compilation.facts else None)
        for lambda_ in compilation.lambdas
    ]
    return ir.Module(functions, compilation.entry.name)


def lower_lambda(lambda_: Lambda, facts=None) -> ir.Function:
    component = lambda_.component
    # The return node is deleted with its block when no path returns normally.
    # A block holding only the return node has no instructions of its own: each
    # predecessor ends in a Return terminator instead.
    returned = lambda_.return_
    blocks = [
        block
        for block in component.blocks()
        if not (returned is not None and block.start.next is returned)
    ]
    lowering = _Lowering(lambda_, blocks)
    for block in blocks:
        lowering.lower_block(block)
    signature = lambda_.signature
    function = ir.Function(
        lambda_.name,
        lambda_.parameters,
        tuple(var.name for var in lambda_.locals),
        lowering.result,
        parameter_annotations=signature.annotations,
        return_annotation=signature.return_annotation,
        policy=lambda_.policy,
        owner_class=lambda_.owner_class,
        local_annotations=dict(lambda_.local_annotations),
        parameter_annotation_expressions=signature.annotation_expressions,
        return_annotation_expression=signature.return_annotation_expression,
        annotation_expressions=dict(lambda_.annotation_expressions),
        namespace=lambda_.namespace,
        variadic=signature.variadic,
        parameter_kinds=signature.kinds,
        default_parameters=signature.default_parameters,
        binding=lambda_.binding,
    )
    if facts is not None:
        function.facts = lowering.named_types(facts.types)
        function.targets = lowering.named_targets(facts.targets)
        function.return_type = facts.return_type
        function.variants = tuple(
            ir.Context(
                context.id,
                context.parameters,
                lowering.named_types(context.types),
                lowering.named_targets(context.targets),
                context.return_type,
            )
            for context in facts.variants
        )
    return function


class _Lowering(NodeVisitor):
    def __init__(self, lambda_, blocks):
        self.lambda_ = lambda_
        self.labels = {block: f"b{index}" for index, block in enumerate(blocks)}
        self.names: dict[int, str] = {}
        self.node_names: dict[object, str] = {}
        self.range_states: dict[int, int] = {}
        self.result: list[ir.Block] = []
        self.serial = 0
        self.current: ir.Block | None = None
        self.exception_target: str | None = None

    def fresh(self) -> str:
        name = f"v{self.serial}"
        self.serial += 1
        return name

    def name(self, node) -> str:
        name = self.names.get(id(node))
        if name is None:
            name = self.names[id(node)] = self.fresh()
            self.node_names[node] = name
        return name

    def operand(self, lvar) -> str:
        return self.name(lvar.use())

    def named_types(self, types) -> dict[str, object]:
        """Facts keyed by the result names this lowering assigned."""
        from .ctype import UNKNOWN

        named = {}
        for node, name in self.node_names.items():
            named[name] = types.get(node, UNKNOWN)
        return named

    def named_targets(self, targets) -> dict[str, str]:
        return {
            self.node_names[node]: lambda_.name
            for node, lambda_ in targets.items()
            if node in self.node_names
        }

    def state(self, range_state) -> int:
        index = self.range_states.get(id(range_state))
        if index is None:
            index = self.range_states[id(range_state)] = len(self.range_states)
        return index

    def new_block(self) -> ir.Block:
        block = ir.Block(
            f"b{len(self.labels) + len(self.result)}"
            if self.current is not None
            else self.labels[self.block],
            exception_target=self.exception_target,
        )
        self.result.append(block)
        return block

    def emit(self, instruction) -> None:
        self.current.instructions.append(instruction)
        if self.exception_target is not None:
            # Each operation in a handler region gets its own block so the
            # handler edge observes the state before that operation.
            following = self.new_block()
            self.current.terminator = ir.Jump(following.label)
            self.current = following

    def produce(self, node, instruction) -> None:
        """Emit a result-producing instruction and discard an unused result."""
        self.emit(instruction)
        if node.lvar is None:
            self.emit(ir.Discard(instruction.result))

    def lower_block(self, block) -> None:
        self.block = block
        target = block_exception_target(block)
        self.exception_target = self.labels[target] if target is not None else None
        self.current = None
        self.current = self.new_block()
        for node in block.nodes():
            node.accept(self)
        self.current.terminator = self.terminator(block)

    def terminator(self, block):
        last = block.last
        if isinstance(last, If):
            return ir.Branch(
                self.operand(last.test),
                self.labels[last.consequent],
                self.labels[last.alternative],
            )
        if isinstance(last, Combination) and last.fun_info is not None:
            name = last.fun_info.name
            if name == "%raise":
                return ir.Raise(self.operand(last.args[0]))
            if name == "%reraise":
                return ir.Raise(None)
            if name == "%unwind":
                return ir.Propagate()
        returned = self.lambda_.return_
        if returned is not None and (
            last is returned
            or (
                block.succ == [returned.block] and returned.block.start.next is returned
            )
        ):
            use = next(use for use in returned.result.uses if use.block is block)
            return ir.Return(self.name(use))
        if len(block.succ) != 1:
            raise ValueError(f"{block!r} has no lowering for its control transfer")
        return ir.Jump(self.labels[block.succ[0]])

    # Nodes

    def visit_Ref(self, node: Ref):
        leaf = node.leaf
        if isinstance(leaf, Constant):
            self.produce(node, ir.Constant(self.name(node), leaf.value))
        elif isinstance(leaf, LambdaVar):
            self.produce(node, ir.Load(self.name(node), leaf.name, True, node.final))
        elif isinstance(leaf, GlobalVar):
            self.produce(node, ir.Load(self.name(node), leaf.name, False))
        elif isinstance(leaf, (KnownFunction, Lambda)):
            return
        else:
            raise TypeError(f"no lowering for a reference to {leaf!r}")

    def visit_Set(self, node):
        local = isinstance(node.var, LambdaVar)
        if node.value is None:
            self.emit(ir.ClearBinding(node.var.name, local))
            return
        self.emit(ir.Store(node.var.name, self.operand(node.value), local))

    def visit_Cast(self, node):
        self.produce(
            node,
            ir.AssertType(
                self.name(node),
                self.operand(node.value),
                node.contract,
                node.location,
                node.safety if node.type_check else 0,
            ),
        )

    def visit_Combination(self, node: Combination):
        if node.kind in (CallKind.FULL, CallKind.GUARDED):
            info = node.info
            self.produce(
                node,
                ir.Call(
                    self.name(node),
                    self.operand(node.fun),
                    tuple(self.operand(arg) for arg in node.args),
                    location=info.location,
                    keywords=info.keywords,
                ),
            )
            return
        name = node.fun_info.name
        args = node.args
        info = node.info
        result = self.name(node) if node.fun_info.result else None
        if name in _BINARY:
            self.produce(
                node,
                ir.Binary(
                    result, _BINARY[name], self.operand(args[0]), self.operand(args[1])
                ),
            )
        elif name in _INPLACE:
            self.produce(
                node,
                ir.Binary(
                    result,
                    _INPLACE[name],
                    self.operand(args[0]),
                    self.operand(args[1]),
                    inplace=True,
                    reuse_local=info.reuse_local,
                ),
            )
        elif name in _UNARY:
            self.produce(node, ir.Unary(result, _UNARY[name], self.operand(args[0])))
        elif name in _COMPARE:
            self.produce(
                node,
                ir.Compare(
                    result, _COMPARE[name], self.operand(args[0]), self.operand(args[1])
                ),
            )
        elif name == "%getattr":
            self.produce(
                node,
                ir.LoadAttribute(
                    result, self.operand(args[0]), info.name, info.location
                ),
            )
        elif name == "%setattr":
            self.emit(
                ir.StoreAttribute(
                    self.operand(args[0]),
                    info.name,
                    self.operand(args[1]),
                    info.location,
                )
            )
        elif name == "%resolve_method":
            self.produce(
                node,
                ir.ResolveMethod(
                    result, self.operand(args[0]), info.name, info.location
                ),
            )
        elif name == "%call_resolved":
            self.produce(
                node,
                ir.CallMethod(
                    result,
                    self.operand(args[0]),
                    tuple(self.operand(arg) for arg in args[1:]),
                    keywords=info.keywords,
                ),
            )
        elif name == "%make_function":
            lambda_ = args[0].use().leaf
            self.produce(
                node,
                ir.DefineFunction(
                    result,
                    lambda_.name,
                    info.public_name,
                    tuple(self.operand(arg) for arg in args[1:]),
                    info.default_contracts,
                    info.location,
                ),
            )
        elif name == "%make_class":
            operands = [self.operand(arg) for arg in args]
            base = operands.pop(0) if info.has_base else None
            self.produce(
                node,
                ir.DefineClass(
                    result,
                    info.public_name,
                    base,
                    info.fields,
                    info.method_names,
                    tuple(operands),
                    field_annotations=info.field_annotations,
                    policy=info.policy,
                    location=info.location,
                    class_id=info.class_id,
                    annotation_expressions=info.annotation_expressions,
                ),
            )
        elif name == "%import":
            self.produce(
                node,
                ir.ImportModule(
                    result, info.module_name, info.initializer, info.filename
                ),
            )
        elif name == "%has_binding":
            self.produce(node, ir.HasModuleBinding(result, self.operand(args[0]), info))
        elif name == "%build_list":
            self.produce(
                node, ir.BuildList(result, tuple(self.operand(arg) for arg in args))
            )
        elif name == "%build_tuple":
            self.produce(
                node, ir.BuildTuple(result, tuple(self.operand(arg) for arg in args))
            )
        elif name == "%format":
            self.produce(
                node,
                ir.FormatValue(
                    result,
                    self.operand(args[0]),
                    self.operand(args[1]),
                    info.conversion,
                ),
            )
        elif name == "%range_prepare":
            self.produce(
                node,
                ir.PrepareRange(
                    result,
                    self.operand(args[0]),
                    self.operand(args[1]),
                    self.state(info),
                ),
            )
        elif name == "%range_available":
            self.produce(node, ir.RangeAvailable(result, self.state(info)))
        elif name == "%range_advance":
            self.produce(
                node,
                ir.RangeAdvance(result, self.state(info), info.minimum, info.maximum),
            )
        elif node.fun_info.protocol is not None:
            self.produce(
                node,
                ir.ProtocolCall(
                    result,
                    node.fun_info.protocol,
                    tuple(self.operand(arg) for arg in args),
                ),
            )
        elif name == "%catch_exception":
            self.produce(node, ir.CatchException(result))
        elif name == "%enter_handler":
            self.emit(ir.EnterHandler(self.operand(args[0])))
        elif name == "%leave_handler":
            self.emit(ir.LeaveHandler())
        elif name == "%assertion_failure":
            self.emit(ir.AssertionFailure(self.operand(args[0])))
        elif name in ("%raise", "%reraise", "%unwind"):
            return
        else:
            raise ValueError(f"no lowering for known function {name}")

    def visit_Exit(self, node: Exit):
        if node.entry is not None:
            raise ValueError("non-local exits are not lowered yet")

    def visit_Node(self, node):
        return None
