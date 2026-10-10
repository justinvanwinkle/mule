"""Convert the supported Python subset into language-level control flow."""

from mule import ast
from mule.parser import parse

from . import ir


class UnsupportedSyntax(SyntaxError):
    """Valid Python syntax that the native compiler cannot yet lower."""


class Lowerer:
    def __init__(self, source: str, filename: str):
        self.source = source.replace("\r\n", "\n").replace("\r", "\n")
        self.filename = filename
        self.functions: list[ir.Function] = []

    def error(self, message: str, node: ast.Node, *, unsupported=False):
        span = node.span
        start = span.start if span else ast.Position(1, 0)
        end = span.end if span else ast.Position(1, 1)
        lines = self.source.splitlines(keepends=True)
        line = lines[start.line - 1] if start.line <= len(lines) else None
        error_type = UnsupportedSyntax if unsupported else SyntaxError
        raise error_type(
            message,
            (
                self.filename,
                start.line,
                start.column + 1,
                line,
                end.line,
                end.column + 1,
            ),
        )

    def function(self, definition: ast.FunctionDefinition) -> ir.Function:
        if definition.is_async or definition.decorators or definition.type_parameters:
            self.error(
                "async, decorated, and generic functions are not supported yet",
                definition,
                unsupported=True,
            )
        if definition.return_annotation is not None:
            self.error(
                "function annotations are not supported yet",
                definition,
                unsupported=True,
            )
        parameters = []
        for parameter in definition.parameters:
            if parameter.name in parameters:
                self.error(f"duplicate parameter '{parameter.name}'", parameter)
            if (
                parameter.kind
                not in (
                    ast.ParameterKind.POSITIONAL_ONLY,
                    ast.ParameterKind.POSITIONAL_OR_KEYWORD,
                )
                or parameter.default is not None
                or parameter.annotation is not None
            ):
                self.error(
                    "only fixed positional parameters are supported yet",
                    parameter,
                    unsupported=True,
                )
            parameters.append(parameter.name)
        function = ir.Function(f"mule_fn_{len(self.functions)}", tuple(parameters), ())
        self.functions.append(function)
        builder = FunctionBuilder(self, function, definition.body, is_module=False)
        builder.statements(definition.body)
        builder.finish()
        return function


class FunctionBuilder:
    def __init__(
        self,
        lowerer: Lowerer,
        function: ir.Function,
        statements: list[ast.Statement],
        *,
        is_module: bool,
    ):
        self.lowerer = lowerer
        self.function = function
        self.is_module = is_module
        self.values = 0
        self.hidden = 0
        self.loops: list[tuple[ir.Block, ir.Block]] = []
        self.globals: set[str] = set()
        self.locals: dict[str, None] = dict.fromkeys(function.parameters)
        self._bindings(statements)
        self.current = self.block()

    def error(self, message: str, node: ast.Node, *, unsupported=False):
        self.lowerer.error(message, node, unsupported=unsupported)

    def _bindings(self, statements):
        """Classify the whole function, including assignments after an early read."""
        seen = set(self.function.parameters)

        def visit(node):
            if isinstance(node, ast.FunctionDefinition):
                if not self.is_module:
                    self.error(
                        "nested functions are not supported yet", node, unsupported=True
                    )
                seen.add(node.name)
                return
            if isinstance(node, ast.Global):
                for name in node.identifiers:
                    if name not in self.globals and name in seen:
                        self.error(
                            f"name '{name}' is used or assigned before global declaration",
                            node,
                        )
                    self.globals.add(name)
                return
            if isinstance(node, ast.Nonlocal):
                self.error(
                    "nonlocal bindings are not supported yet", node, unsupported=True
                )
            if isinstance(node, ast.Name):
                seen.add(node.identifier)
            for child in ast.children(node):
                visit(child)

        for statement in statements:
            visit(statement)
        if not self.is_module:
            for statement in statements:
                for node in ast.walk(statement):
                    targets = []
                    if isinstance(node, ast.Assignment):
                        targets = node.targets
                    elif isinstance(
                        node,
                        (
                            ast.AugmentedAssignment,
                            ast.AnnotatedAssignment,
                            ast.NamedExpression,
                        ),
                    ):
                        targets = [node.target]
                    for target in targets:
                        if (
                            isinstance(target, ast.Name)
                            and target.identifier not in self.globals
                        ):
                            self.locals[target.identifier] = None

    def block(self) -> ir.Block:
        block = ir.Block(f"b{len(self.function.blocks)}")
        self.function.blocks.append(block)
        return block

    def emit(self, instruction: ir.Instruction):
        self.current.instructions.append(instruction)

    def result(self) -> str:
        value = f"v{self.values}"
        self.values += 1
        return value

    def constant(self, value) -> str:
        result = self.result()
        self.emit(ir.Constant(result, value))
        return result

    def local(self, name: str) -> bool:
        return name in self.locals

    def load(self, name: str, *, clear=False) -> str:
        result = self.result()
        self.emit(ir.Load(result, name, self.local(name), clear))
        return result

    def store(self, name: str, value: str):
        self.emit(ir.Store(name, value, self.local(name)))

    def temporary(self) -> str:
        name = f"$merge{self.hidden}"
        self.hidden += 1
        self.locals[name] = None
        return name

    def jump(self, block: ir.Block):
        self.current.terminator = ir.Jump(block.label)

    def statements(self, statements: list[ast.Statement]):
        for statement in statements:
            if self.current.terminator is not None:
                # Compile unreachable source too, so unsupported/invalid syntax
                # cannot be hidden behind a return, break, or continue.
                self.current = self.block()
            self.statement(statement)

    def statement(self, node: ast.Statement):
        match node:
            case ast.Pass() | ast.Global():
                return
            case ast.ExpressionStatement(expression=expression):
                self.emit(ir.Discard(self.expression(expression)))
            case ast.Assignment(targets=targets, value=value):
                for target in targets:
                    if not isinstance(target, ast.Name):
                        self.error(
                            "only identifier assignment targets are supported yet",
                            target,
                            unsupported=True,
                        )
                result = self.expression(value)
                if len(targets) == 1:
                    self.store(targets[0].identifier, result)
                else:
                    temporary = self.temporary()
                    self.store(temporary, result)
                    for index, target in enumerate(targets):
                        self.store(
                            target.identifier,
                            self.load(temporary, clear=index == len(targets) - 1),
                        )
            case ast.AugmentedAssignment(
                target=ast.Name(identifier=name), operator=op, value=value
            ):
                left = self.load(name)
                right = self.expression(value)
                result = self.result()
                self.emit(ir.Binary(result, op, left, right, inplace=True))
                self.store(name, result)
            case ast.If(condition=condition, body=body, else_body=otherwise):
                then, other, join = self.block(), self.block(), self.block()
                self.condition(condition, then, other)
                self.current = then
                self.statements(body)
                if self.current.terminator is None:
                    self.jump(join)
                self.current = other
                self.statements(otherwise)
                if self.current.terminator is None:
                    self.jump(join)
                self.current = join
            case ast.While(condition=condition, body=body, else_body=otherwise):
                test, loop, other, done = (
                    self.block(),
                    self.block(),
                    self.block(),
                    self.block(),
                )
                self.jump(test)
                self.current = test
                self.condition(condition, loop, other)
                self.current = loop
                self.loops.append((test, done))
                self.statements(body)
                self.loops.pop()
                if self.current.terminator is None:
                    self.jump(test)
                self.current = other
                self.statements(otherwise)
                if self.current.terminator is None:
                    self.jump(done)
                self.current = done
            case ast.Break() | ast.Continue():
                if not self.loops:
                    self.error(f"'{type(node).__name__.lower()}' outside loop", node)
                self.jump(self.loops[-1][1 if isinstance(node, ast.Break) else 0])
            case ast.Return(value=value):
                if self.is_module:
                    self.error("'return' outside function", node)
                self.current.terminator = ir.Return(
                    self.constant(None) if value is None else self.expression(value)
                )
            case ast.FunctionDefinition():
                function = self.lowerer.function(node)
                result = self.result()
                self.emit(ir.DefineFunction(result, function.name, node.name))
                self.store(node.name, result)
            case _:
                self.error(
                    f"{type(node).__name__} is not supported by the native compiler yet",
                    node,
                    unsupported=True,
                )

    def expression(self, node: ast.Expression) -> str:
        match node:
            case ast.Literal(value=value) if (
                isinstance(value, (str, bytes, int, float)) or value is None
            ):
                return self.constant(value)
            case ast.Name(identifier=name):
                return self.load(name)
            case ast.Binary(operator=op, left=left, right=right):
                a, b = self.expression(left), self.expression(right)
                result = self.result()
                self.emit(ir.Binary(result, op, a, b))
                return result
            case ast.Unary(operator=op, operand=operand):
                value = self.expression(operand)
                result = self.result()
                self.emit(ir.Unary(result, op, value))
                return result
            case ast.Call(function=callee, arguments=arguments):
                for argument in arguments:
                    if argument.kind != ast.ArgumentKind.POSITIONAL:
                        self.error(
                            "only positional call arguments are supported yet",
                            argument,
                            unsupported=True,
                        )
                function = self.expression(callee)
                args = tuple(self.expression(argument.value) for argument in arguments)
                result = self.result()
                self.emit(ir.Call(result, function, args))
                return result
            case ast.NamedExpression(target=target, value=value):
                temporary = self.temporary()
                self.store(temporary, self.expression(value))
                self.store(target.identifier, self.load(temporary))
                return self.load(temporary, clear=True)
            case ast.Boolean(operator=op, operands=values):
                return self.boolean(op, values)
            case ast.Conditional(
                condition=condition, then=then_value, otherwise=other_value
            ):
                then, other, join = self.block(), self.block(), self.block()
                temporary = self.temporary()
                self.condition(condition, then, other)
                self.current = then
                self.store(temporary, self.expression(then_value))
                self.jump(join)
                self.current = other
                self.store(temporary, self.expression(other_value))
                self.jump(join)
                self.current = join
                return self.load(temporary, clear=True)
            case ast.Comparison():
                return self.comparison(node)
            case _:
                self.error(
                    f"{type(node).__name__} is not supported by the native compiler yet",
                    node,
                    unsupported=True,
                )

    def boolean(self, operator, expressions):
        temporary = self.temporary()
        join = self.block()
        for expression in expressions[:-1]:
            next_block = self.block()
            true, false = (
                (next_block, join)
                if operator == ast.BooleanOperator.AND
                else (join, next_block)
            )
            self.value_condition(expression, temporary, true, false)
            self.current = next_block
            self.emit(ir.Discard(self.load(temporary, clear=True)))
        self.store(temporary, self.expression(expressions[-1]))
        self.jump(join)
        self.current = join
        return self.load(temporary, clear=True)

    def value_condition(self, node, destination, true, false):
        """Branch on a boolean operand while retaining its selected value.

        Nested and/or expressions share the surrounding short-circuit edges.
        Other expression forms produce their ordinary values before testing;
        those boundaries can intentionally test a selected object again.
        """
        if isinstance(node, ast.Boolean):
            for operand in node.operands[:-1]:
                next_block = self.block()
                yes, no = (
                    (next_block, false)
                    if node.operator == ast.BooleanOperator.AND
                    else (true, next_block)
                )
                self.value_condition(operand, destination, yes, no)
                self.current = next_block
                self.emit(ir.Discard(self.load(destination, clear=True)))
            self.value_condition(node.operands[-1], destination, true, false)
            return
        self.store(destination, self.expression(node))
        self.current.terminator = ir.Branch(
            self.load(destination), true.label, false.label
        )

    def condition(self, node, true, false):
        """Lower truth-testing contexts without re-testing short-circuit values."""
        match node:
            case ast.Boolean(operator=operator, operands=operands):
                for operand in operands[:-1]:
                    next_block = self.block()
                    yes, no = (
                        (next_block, false)
                        if operator == ast.BooleanOperator.AND
                        else (true, next_block)
                    )
                    self.condition(operand, yes, no)
                    self.current = next_block
                self.condition(operands[-1], true, false)
            case ast.Unary(operator=ast.UnaryOperator.NOT, operand=operand):
                self.condition(operand, false, true)
            case ast.Conditional(condition=test, then=yes, otherwise=no):
                yes_block, no_block = self.block(), self.block()
                self.condition(test, yes_block, no_block)
                self.current = yes_block
                self.condition(yes, true, false)
                self.current = no_block
                self.condition(no, true, false)
            case ast.Comparison() if len(node.operators) > 1:
                self.comparison_condition(node, true, false)
            case _:
                self.current.terminator = ir.Branch(
                    self.expression(node), true.label, false.label
                )

    def comparison_condition(self, node, true, false):
        previous = self.temporary()
        self.store(previous, self.expression(node.left))
        for index, (operator, comparator) in enumerate(
            zip(node.operators, node.comparators)
        ):
            left = self.load(previous, clear=True)
            right = self.expression(comparator)
            last = index == len(node.operators) - 1
            if not last:
                self.store(previous, right)
                right = self.load(previous)
            result = self.result()
            self.emit(ir.Compare(result, operator, left, right))
            if last:
                self.current.terminator = ir.Branch(result, true.label, false.label)
            else:
                next_block, failed = self.block(), self.block()
                self.current.terminator = ir.Branch(
                    result, next_block.label, failed.label
                )
                self.current = failed
                self.emit(ir.Discard(self.load(previous, clear=True)))
                self.jump(false)
                self.current = next_block

    def comparison(self, node: ast.Comparison):
        if len(node.operators) == 1:
            left, right = (
                self.expression(node.left),
                self.expression(node.comparators[0]),
            )
            result = self.result()
            self.emit(ir.Compare(result, node.operators[0], left, right))
            return result
        previous, outcome = self.temporary(), self.temporary()
        join = self.block()
        self.store(previous, self.expression(node.left))
        for index, (operator, comparator) in enumerate(
            zip(node.operators, node.comparators)
        ):
            left = self.load(previous, clear=True)
            right = self.expression(comparator)
            if index < len(node.operators) - 1:
                self.store(previous, right)
                right = self.load(previous)
            result = self.result()
            self.emit(ir.Compare(result, operator, left, right))
            self.store(outcome, result)
            if index < len(node.operators) - 1:
                next_block = self.block()
                self.current.terminator = ir.Branch(
                    self.load(outcome), next_block.label, join.label
                )
                self.current = next_block
                self.emit(ir.Discard(self.load(outcome, clear=True)))
        self.jump(join)
        self.current = join
        # A short-circuit can leave the middle operand in the temporary. Its
        # release is conditional because the successful final comparison moved it.
        # Assigning None releases either state without reading an unbound slot.
        self.store(previous, self.constant(None))
        self.emit(ir.Discard(self.load(previous, clear=True)))
        return self.load(outcome, clear=True)

    def finish(self):
        if self.current.terminator is None:
            self.current.terminator = ir.Return(self.constant(None))
        self.function.locals = tuple(self.locals)


def lower(source: str, filename: str = "<unknown>") -> ir.Module:
    tree = parse(source, filename)
    lowerer = Lowerer(source, filename)
    entry = ir.Function("mule_module", (), ())
    lowerer.functions.append(entry)
    builder = FunctionBuilder(lowerer, entry, tree.statements, is_module=True)
    builder.statements(tree.statements)
    builder.finish()
    module = ir.Module(lowerer.functions, entry.name)
    ir.verify(module)
    return module
