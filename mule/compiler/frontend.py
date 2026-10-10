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


class FunctionBuilder(ast.Visitor):
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
        scope = _ScopeVisitor(self)
        for statement in statements:
            statement.accept(scope)
        if not self.is_module:
            collector = _LocalBindings(self)
            for statement in statements:
                statement.accept(collector)

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

    def constant(self, value: ast.Literal) -> str:
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
        node.accept(self)

    def expression(self, node: ast.Expression) -> str:
        return node.accept(self)

    def visit_Node(self, node):
        self.error(
            "syntax is not supported by the native compiler yet",
            node,
            unsupported=True,
        )

    def visit_Pass(self, node):
        return None

    def visit_Global(self, node):
        return None

    def visit_ExpressionStatement(self, node):
        self.emit(ir.Discard(self.expression(node.expression)))

    def visit_Assignment(self, node):
        result = self.expression(node.value)
        if len(node.targets) == 1:
            node.targets[0].accept(_AssignmentTarget(self, result))
        else:
            temporary = self.temporary()
            self.store(temporary, result)
            for index, target in enumerate(node.targets):
                value = self.load(temporary, clear=index == len(node.targets) - 1)
                target.accept(_AssignmentTarget(self, value))

    def visit_AugmentedAssignment(self, node):
        node.target.accept(_AugmentedTarget(self, node))

    def visit_If(self, node):
        then, other, join = self.block(), self.block(), self.block()
        self.condition(node.condition, then, other)
        self.current = then
        self.statements(node.body)
        if self.current.terminator is None:
            self.jump(join)
        self.current = other
        self.statements(node.else_body)
        if self.current.terminator is None:
            self.jump(join)
        self.current = join

    def visit_While(self, node):
        test, loop, other, done = self.block(), self.block(), self.block(), self.block()
        self.jump(test)
        self.current = test
        self.condition(node.condition, loop, other)
        self.current = loop
        self.loops.append((test, done))
        self.statements(node.body)
        self.loops.pop()
        if self.current.terminator is None:
            self.jump(test)
        self.current = other
        self.statements(node.else_body)
        if self.current.terminator is None:
            self.jump(done)
        self.current = done

    def visit_Break(self, node):
        if not self.loops:
            self.error("'break' outside loop", node)
        self.jump(self.loops[-1][1])

    def visit_Continue(self, node):
        if not self.loops:
            self.error("'continue' outside loop", node)
        self.jump(self.loops[-1][0])

    def visit_Return(self, node):
        if self.is_module:
            self.error("'return' outside function", node)
        self.current.terminator = ir.Return(
            self.constant(ast.NoneLiteral())
            if node.value is None
            else self.expression(node.value)
        )

    def visit_FunctionDefinition(self, node):
        function = self.lowerer.function(node)
        result = self.result()
        self.emit(ir.DefineFunction(result, function.name, node.name))
        self.store(node.name, result)

    def visit_ClassDefinition(self, node):
        if node.decorators or node.type_parameters:
            self.error(
                "decorated and generic classes are not supported yet",
                node,
                unsupported=True,
            )
        if len(node.arguments) > 1 or any(
            argument.kind != ast.ArgumentKind.POSITIONAL for argument in node.arguments
        ):
            self.error(
                "classes support at most one positional base",
                node,
                unsupported=True,
            )
        body = _ClassBody(self)
        for statement in node.body:
            statement.accept(body)
        # Base expressions are evaluated before any method definitions, as in Python.
        base = self.expression(node.arguments[0].value) if node.arguments else None
        fields = {}
        methods = {}
        for method in body.methods:
            function = self.lowerer.function(method)
            if not method.parameters:
                self.error("instance methods require a receiver parameter", method)
            receiver = method.parameters[0].name
            collector = _ReceiverFields(receiver, fields)
            for statement in method.body:
                statement.accept(collector)
            value = self.result()
            self.emit(ir.DefineFunction(value, function.name, method.name))
            # Repeated definitions preserve Python's last-definition-wins behavior.
            previous = methods.get(method.name)
            if previous is not None:
                self.emit(ir.Discard(previous))
            methods[method.name] = value
        result = self.result()
        self.emit(
            ir.DefineClass(
                result,
                node.name,
                base,
                tuple(fields),
                tuple(methods),
                tuple(methods.values()),
            )
        )
        self.store(node.name, result)

    def visit_IntegerLiteral(self, node):
        return self.constant(node)

    def visit_FloatLiteral(self, node):
        return self.constant(node)

    def visit_BooleanLiteral(self, node):
        return self.constant(node)

    def visit_NoneLiteral(self, node):
        return self.constant(node)

    def visit_TextLiteral(self, node):
        return self.constant(node)

    def visit_BytesLiteral(self, node):
        return self.constant(node)

    def visit_Name(self, node):
        return self.load(node.identifier)

    def visit_Attribute(self, node):
        receiver = self.expression(node.value)
        result = self.result()
        self.emit(ir.LoadAttribute(result, receiver, node.attribute))
        return result

    def visit_Binary(self, node):
        a, b = self.expression(node.left), self.expression(node.right)
        result = self.result()
        self.emit(ir.Binary(result, node.operator, a, b))
        return result

    def visit_Unary(self, node):
        value = self.expression(node.operand)
        result = self.result()
        self.emit(ir.Unary(result, node.operator, value))
        return result

    def visit_Call(self, node):
        for argument in node.arguments:
            if argument.kind != ast.ArgumentKind.POSITIONAL:
                self.error(
                    "only positional call arguments are supported yet",
                    argument,
                    unsupported=True,
                )
        return node.function.accept(_CallTarget(self, node.arguments))

    def visit_NamedExpression(self, node):
        temporary = self.temporary()
        self.store(temporary, self.expression(node.value))
        self.store(node.target.identifier, self.load(temporary))
        return self.load(temporary, clear=True)

    def visit_Boolean(self, node):
        return self.boolean(node.operator, node.operands)

    def visit_Conditional(self, node):
        then, other, join = self.block(), self.block(), self.block()
        temporary = self.temporary()
        self.condition(node.condition, then, other)
        self.current = then
        self.store(temporary, self.expression(node.then))
        self.jump(join)
        self.current = other
        self.store(temporary, self.expression(node.otherwise))
        self.jump(join)
        self.current = join
        return self.load(temporary, clear=True)

    def visit_Comparison(self, node):
        return self.comparison(node)

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
        node.accept(_ValueConditionVisitor(self, destination, true, false))

    def condition(self, node, true, false):
        """Lower truth-testing contexts without re-testing short-circuit values."""
        node.accept(_ConditionVisitor(self, true, false))

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
        self.store(previous, self.constant(ast.NoneLiteral()))
        self.emit(ir.Discard(self.load(previous, clear=True)))
        return self.load(outcome, clear=True)

    def finish(self):
        if self.current.terminator is None:
            self.current.terminator = ir.Return(self.constant(ast.NoneLiteral()))
        self.function.locals = tuple(self.locals)


class _AssignmentTarget(ast.Visitor):
    def __init__(self, builder, value):
        self.builder = builder
        self.value = value

    def visit_Node(self, node):
        self.builder.error(
            "only identifier and attribute assignment targets are supported yet",
            node,
            unsupported=True,
        )

    def visit_Name(self, node):
        self.builder.store(node.identifier, self.value)

    def visit_Attribute(self, node):
        receiver = self.builder.expression(node.value)
        self.builder.emit(ir.StoreAttribute(receiver, node.attribute, self.value))


class _AugmentedTarget(ast.Visitor):
    def __init__(self, builder, assignment):
        self.builder = builder
        self.assignment = assignment

    def visit_Node(self, node):
        self.builder.error(
            "only identifier and attribute assignment targets are supported yet",
            node,
            unsupported=True,
        )

    def binary(self, left, reuse_local=None):
        right = self.builder.expression(self.assignment.value)
        result = self.builder.result()
        self.builder.emit(
            ir.Binary(
                result,
                self.assignment.operator,
                left,
                right,
                inplace=True,
                reuse_local=reuse_local,
            )
        )
        return result

    def visit_Name(self, node):
        left = self.builder.load(node.identifier)
        result = self.binary(
            left, node.identifier if self.builder.local(node.identifier) else None
        )
        self.builder.store(node.identifier, result)

    def visit_Attribute(self, node):
        temporary = self.builder.temporary()
        self.builder.store(temporary, self.builder.expression(node.value))
        left = self.builder.result()
        self.builder.emit(
            ir.LoadAttribute(left, self.builder.load(temporary), node.attribute)
        )
        result = self.binary(left)
        self.builder.emit(
            ir.StoreAttribute(
                self.builder.load(temporary, clear=True), node.attribute, result
            )
        )


class _CallTarget(ast.Visitor):
    def __init__(self, builder, arguments):
        self.builder = builder
        self.arguments = arguments

    def arguments_values(self):
        return tuple(
            self.builder.expression(argument.value) for argument in self.arguments
        )

    def visit_Node(self, node):
        callee = self.builder.expression(node)
        arguments = self.arguments_values()
        result = self.builder.result()
        self.builder.emit(ir.Call(result, callee, arguments))
        return result

    def visit_Attribute(self, node):
        receiver = self.builder.expression(node.value)
        callee = self.builder.result()
        self.builder.emit(ir.ResolveMethod(callee, receiver, node.attribute))
        arguments = self.arguments_values()
        result = self.builder.result()
        self.builder.emit(ir.CallMethod(result, callee, arguments))
        return result


class _ClassBody(ast.Visitor):
    def __init__(self, builder):
        self.builder = builder
        self.methods = []

    def visit_Node(self, node):
        self.builder.error(
            "class bodies support only instance methods and pass",
            node,
            unsupported=True,
        )

    def visit_FunctionDefinition(self, node):
        self.methods.append(node)

    def visit_Pass(self, node):
        pass


class _ReceiverFields(ast.Visitor):
    def __init__(self, receiver, fields):
        self.targets = _ReceiverFieldTarget(receiver, fields)

    def visit_Node(self, node):
        for target in node.assigned_targets():
            target.accept(self.targets)
        for child in node.children():
            child.accept(self)

    def visit_FunctionDefinition(self, node):
        pass

    def visit_ClassDefinition(self, node):
        pass


class _ReceiverFieldTarget(ast.Visitor):
    def __init__(self, receiver, fields):
        self.receiver = _ReceiverName(receiver)
        self.fields = fields

    def visit_Attribute(self, node):
        if node.value.accept(self.receiver):
            self.fields[node.attribute] = None


class _ReceiverName(ast.Visitor):
    def __init__(self, name):
        self.name = name

    def visit_Name(self, node):
        return node.identifier == self.name


class _ScopeVisitor(ast.Visitor):
    def __init__(self, builder):
        self.builder = builder
        self.seen = set(builder.function.parameters)

    def visit_Node(self, node):
        for child in node.children():
            child.accept(self)

    def visit_Name(self, node):
        self.seen.add(node.identifier)

    def visit_FunctionDefinition(self, node):
        if not self.builder.is_module:
            self.builder.error(
                "nested functions are not supported yet", node, unsupported=True
            )
        self.seen.add(node.name)

    def visit_ClassDefinition(self, node):
        if not self.builder.is_module:
            self.builder.error(
                "nested classes are not supported yet", node, unsupported=True
            )
        for argument in node.arguments:
            argument.accept(self)
        self.seen.add(node.name)

    def visit_Global(self, node):
        for name in node.identifiers:
            if name not in self.builder.globals and name in self.seen:
                self.builder.error(
                    f"name '{name}' is used or assigned before global declaration", node
                )
            self.builder.globals.add(name)

    def visit_Nonlocal(self, node):
        self.builder.error(
            "nonlocal bindings are not supported yet", node, unsupported=True
        )


class _LocalBindings(ast.Visitor):
    def __init__(self, builder):
        self.builder = builder
        self.target_visitor = _LocalTarget(builder)

    def visit_Node(self, node):
        for target in node.assigned_targets():
            target.accept(self.target_visitor)
        for child in node.children():
            child.accept(self)

    def visit_FunctionDefinition(self, node):
        pass

    def visit_ClassDefinition(self, node):
        pass


class _LocalTarget(ast.Visitor):
    def __init__(self, builder):
        self.builder = builder

    def visit_Node(self, node):
        pass

    def visit_Name(self, node):
        if node.identifier not in self.builder.globals:
            self.builder.locals[node.identifier] = None


class _ConditionVisitor(ast.Visitor):
    def __init__(self, builder, true, false):
        self.builder = builder
        self.true, self.false = true, false

    def visit_Node(self, node):
        self.builder.current.terminator = ir.Branch(
            self.builder.expression(node), self.true.label, self.false.label
        )

    def visit_Boolean(self, node):
        for operand in node.operands[:-1]:
            next_block = self.builder.block()
            yes, no = (
                (next_block, self.false)
                if node.operator == ast.BooleanOperator.AND
                else (self.true, next_block)
            )
            self.builder.condition(operand, yes, no)
            self.builder.current = next_block
        self.builder.condition(node.operands[-1], self.true, self.false)

    def visit_Unary(self, node):
        if node.operator == ast.UnaryOperator.NOT:
            self.builder.condition(node.operand, self.false, self.true)
        else:
            self.visit_Node(node)

    def visit_Conditional(self, node):
        yes_block, no_block = self.builder.block(), self.builder.block()
        self.builder.condition(node.condition, yes_block, no_block)
        self.builder.current = yes_block
        self.builder.condition(node.then, self.true, self.false)
        self.builder.current = no_block
        self.builder.condition(node.otherwise, self.true, self.false)

    def visit_Comparison(self, node):
        if len(node.operators) > 1:
            self.builder.comparison_condition(node, self.true, self.false)
        else:
            self.visit_Node(node)


class _ValueConditionVisitor(ast.Visitor):
    def __init__(self, builder, destination, true, false):
        self.builder = builder
        self.destination = destination
        self.true, self.false = true, false

    def visit_Node(self, node):
        self.builder.store(self.destination, self.builder.expression(node))
        self.builder.current.terminator = ir.Branch(
            self.builder.load(self.destination), self.true.label, self.false.label
        )

    def visit_Boolean(self, node):
        for operand in node.operands[:-1]:
            next_block = self.builder.block()
            yes, no = (
                (next_block, self.false)
                if node.operator == ast.BooleanOperator.AND
                else (self.true, next_block)
            )
            self.builder.value_condition(operand, self.destination, yes, no)
            self.builder.current = next_block
            self.builder.emit(
                ir.Discard(self.builder.load(self.destination, clear=True))
            )
        self.builder.value_condition(
            node.operands[-1], self.destination, self.true, self.false
        )


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
