"""Language-level control flow for the initial native compiler.

Control edges and single-consumer expression results are separate. Variables
remain explicit; converting this representation to LLVM is a backend task.
"""

from dataclasses import dataclass, field

from mule import ast


class Operation:
    """Behavior shared by instructions and control-flow terminators."""

    __slots__ = ()

    def accept(self, visitor):
        return visitor.visit_Operation(self)

    def operands(self) -> tuple[str, ...]:
        return ()

    def successors(self) -> tuple[str, ...]:
        return ()

    def defined_result(self) -> str | None:
        return None

    def global_names(self) -> tuple[str, ...]:
        return ()

    def function_definition(self) -> tuple[str, str] | None:
        return None

    def stored_local(self) -> tuple[str, str] | None:
        return None

    def argument_count(self) -> int:
        return 0

    def confirms_assignment(self, name: str, value: str) -> bool:
        return False

    def validate(self, function, functions, following):
        pass


class ResultOperation(Operation):
    __slots__ = ()

    def defined_result(self) -> str:
        return self.result


class Visitor:
    """An operation selects its visitor method directly."""

    def visit_Operation(self, operation):
        return None

    def visit_Constant(self, operation):
        return self.visit_Operation(operation)

    def visit_Load(self, operation):
        return self.visit_Operation(operation)

    def visit_Store(self, operation):
        return self.visit_Operation(operation)

    def visit_Binary(self, operation):
        return self.visit_Operation(operation)

    def visit_Unary(self, operation):
        return self.visit_Operation(operation)

    def visit_Compare(self, operation):
        return self.visit_Operation(operation)

    def visit_Call(self, operation):
        return self.visit_Operation(operation)

    def visit_DefineFunction(self, operation):
        return self.visit_Operation(operation)

    def visit_Discard(self, operation):
        return self.visit_Operation(operation)

    def visit_Jump(self, operation):
        return self.visit_Operation(operation)

    def visit_Branch(self, operation):
        return self.visit_Operation(operation)

    def visit_Return(self, operation):
        return self.visit_Operation(operation)


@dataclass(frozen=True, slots=True)
class Constant(ResultOperation):
    result: str
    value: ast.Literal

    def accept(self, visitor):
        return visitor.visit_Constant(self)


@dataclass(frozen=True, slots=True)
class Load(ResultOperation):
    result: str
    name: str
    local: bool
    clear: bool = False

    def accept(self, visitor):
        return visitor.visit_Load(self)

    def global_names(self):
        return () if self.local else (self.name,)

    def validate(self, function, functions, following):
        if self.local and self.name not in function.locals:
            raise ValueError(f"undeclared local {self.name}")


@dataclass(frozen=True, slots=True)
class Store(Operation):
    name: str
    value: str
    local: bool

    def accept(self, visitor):
        return visitor.visit_Store(self)

    def operands(self):
        return (self.value,)

    def global_names(self):
        return () if self.local else (self.name,)

    def stored_local(self):
        return (self.name, self.value) if self.local else None

    def confirms_assignment(self, name, value):
        return self.local and self.name == name and self.value == value

    def validate(self, function, functions, following):
        if self.local and self.name not in function.locals:
            raise ValueError(f"undeclared local {self.name}")


@dataclass(frozen=True, slots=True)
class Binary(ResultOperation):
    result: str
    operator: ast.BinaryOperator
    left: str
    right: str
    inplace: bool = False
    reuse_local: str | None = None

    def accept(self, visitor):
        return visitor.visit_Binary(self)

    def operands(self):
        return (self.left, self.right)

    def validate(self, function, functions, following):
        if self.reuse_local is not None and (
            not self.inplace
            or self.reuse_local not in function.locals
            or following is None
            or not following.confirms_assignment(self.reuse_local, self.result)
        ):
            raise ValueError("bigint reuse requires immediate assignment to its local")


@dataclass(frozen=True, slots=True)
class Unary(ResultOperation):
    result: str
    operator: ast.UnaryOperator
    operand: str

    def accept(self, visitor):
        return visitor.visit_Unary(self)

    def operands(self):
        return (self.operand,)


@dataclass(frozen=True, slots=True)
class Compare(ResultOperation):
    result: str
    operator: ast.ComparisonOperator
    left: str
    right: str

    def accept(self, visitor):
        return visitor.visit_Compare(self)

    def operands(self):
        return (self.left, self.right)


@dataclass(frozen=True, slots=True)
class Call(ResultOperation):
    result: str
    callee: str
    args: tuple[str, ...]

    def accept(self, visitor):
        return visitor.visit_Call(self)

    def operands(self):
        return (self.callee, *self.args)

    def argument_count(self):
        return len(self.args)


@dataclass(frozen=True, slots=True)
class DefineFunction(ResultOperation):
    result: str
    function: str
    public_name: str

    def accept(self, visitor):
        return visitor.visit_DefineFunction(self)

    def function_definition(self):
        return (self.result, self.function)

    def validate(self, function, functions, following):
        if self.function not in functions:
            raise ValueError("reference to missing function")


@dataclass(frozen=True, slots=True)
class Discard(Operation):
    value: str

    def accept(self, visitor):
        return visitor.visit_Discard(self)

    def operands(self):
        return (self.value,)


type Instruction = (
    Constant | Load | Store | Binary | Unary | Compare | Call | DefineFunction | Discard
)


@dataclass(frozen=True, slots=True)
class Jump(Operation):
    target: str

    def accept(self, visitor):
        return visitor.visit_Jump(self)

    def successors(self):
        return (self.target,)


@dataclass(frozen=True, slots=True)
class Branch(Operation):
    condition: str
    true: str
    false: str

    def accept(self, visitor):
        return visitor.visit_Branch(self)

    def operands(self):
        return (self.condition,)

    def successors(self):
        return (self.true, self.false)


@dataclass(frozen=True, slots=True)
class Return(Operation):
    value: str

    def accept(self, visitor):
        return visitor.visit_Return(self)

    def operands(self):
        return (self.value,)


type Terminator = Jump | Branch | Return


@dataclass(slots=True)
class Block:
    label: str
    instructions: list[Instruction] = field(default_factory=list)
    terminator: Terminator | None = None


@dataclass(slots=True)
class Function:
    name: str
    parameters: tuple[str, ...]
    locals: tuple[str, ...]
    blocks: list[Block] = field(default_factory=list)


@dataclass(slots=True)
class Module:
    functions: list[Function]
    entry: str


def operands(operation: Instruction | Terminator) -> tuple[str, ...]:
    return operation.operands()


def verify(module: Module) -> None:
    """Reject malformed graphs before emission, including value ownership errors."""
    functions = {function.name for function in module.functions}
    if len(functions) != len(module.functions) or module.entry not in functions:
        raise ValueError("duplicate function or missing module entry")
    for function in module.functions:
        _verify_function(function, functions)


def _verify_function(function: Function, functions: set[str]) -> None:
    if not function.blocks:
        raise ValueError(f"{function.name}: missing entry block")
    blocks = {block.label: block for block in function.blocks}
    if len(blocks) != len(function.blocks):
        raise ValueError(f"{function.name}: duplicate block")
    if len(set(function.locals)) != len(function.locals) or not set(
        function.parameters
    ) <= set(function.locals):
        raise ValueError("invalid parameter/local declarations")
    definitions = {}
    successors = {}
    predecessors = {label: set() for label in blocks}
    for block in function.blocks:
        if block.terminator is None:
            raise ValueError(f"{function.name}/{block.label}: unterminated block")
        targets = block.terminator.successors()
        if not set(targets) <= blocks.keys():
            raise ValueError("control edge to missing block")
        successors[block.label] = targets
        for target in targets:
            predecessors[target].add(block.label)
        for index, operation in enumerate(block.instructions):
            following = (
                block.instructions[index + 1]
                if index + 1 < len(block.instructions)
                else None
            )
            operation.validate(function, functions, following)
            result = operation.defined_result()
            if result is not None:
                if result in definitions:
                    raise ValueError(f"multiply defined value {result}")
                definitions[result] = (block.label, index)

    entry = function.blocks[0].label
    reachable = set()
    pending = [entry]
    while pending:
        label = pending.pop()
        if label not in reachable:
            reachable.add(label)
            pending.extend(successors[label])
    dominators = {
        label: ({entry} if label == entry else set(reachable)) for label in reachable
    }
    changed = True
    while changed:
        changed = False
        for label in reachable - {entry}:
            parents = predecessors[label] & reachable
            incoming = set.intersection(*(dominators[parent] for parent in parents))
            new = {label} | incoming
            if new != dominators[label]:
                dominators[label] = new
                changed = True

    consumed = set()
    for block in function.blocks:
        for index, operation in enumerate([*block.instructions, block.terminator]):
            for value in operands(operation):
                if value not in definitions or value in consumed:
                    raise ValueError(f"undefined or multiply consumed value {value}")
                producer, position = definitions[value]
                if producer == block.label:
                    if position >= index:
                        raise ValueError(f"value {value} used before definition")
                elif (
                    block.label in reachable and producer not in dominators[block.label]
                ):
                    raise ValueError(f"value {value} does not dominate its use")
                consumed.add(value)
    if consumed != definitions.keys():
        raise ValueError(
            f"unconsumed results in {function.name}: {definitions.keys() - consumed}"
        )
