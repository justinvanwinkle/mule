"""Handwritten statement grammar contracts for Mule's own syntax tree."""

import pytest

from mule import ast as A
from test.mule.parser import ast_query as q
from test.mule.parser.ast_query import assert_ast_equal, at, nodes, one

STATEMENTS = [
    # Assignment and deletion (20).
    ("assign_name", "answer = 42"),
    ("assign_chain", "left = right = value"),
    ("assign_tuple", "left, right = pair"),
    ("assign_list", "[left, right] = pair"),
    ("assign_star", "head, *middle, tail = values"),
    ("assign_nested", "(first, [second, *rest]) = values"),
    ("assign_attribute", "obj.value = result"),
    ("assign_subscript", "values[index] = result"),
    ("assign_slice", "values[start:stop:step] = result"),
    ("assign_parenthesized", "(target) = value"),
    ("annotate_name", "value: int"),
    ("annotate_value", "value: list[int] = []"),
    ("annotate_attribute", "obj.value: int = 1"),
    ("annotate_subscript", "values[0]: int = 1"),
    ("annotate_parenthesized", "(value): int = 1"),
    ("augment_name", "count += 1"),
    ("augment_attribute", "obj.count **= exponent"),
    ("augment_subscript", "items[0] >>= bits"),
    ("delete_multiple", "del first, obj.second, values[0]"),
    ("delete_nested", "del (first, [second, third])"),
    # Import statements (10).
    ("import_simple", "import package"),
    ("import_dotted", "import package.submodule"),
    ("import_alias", "import package.submodule as local"),
    ("import_multiple", "import first, second as local, third.child"),
    ("from_import", "from package import member"),
    ("from_import_aliases", "from package.child import first as local, second"),
    ("from_import_parentheses", "from package import (first, second as local, )"),
    ("from_import_star", "from package import *"),
    ("from_import_relative", "from ..package import member"),
    ("from_import_relative_only", "from ... import member"),
    # Conditional statements and suites (15).
    ("if_block", "if ready:\n    act()"),
    ("if_inline", "if ready: act()"),
    ("if_else", "if ready:\n    act()\nelse:\n    wait()"),
    ("if_elif", "if first:\n    a()\nelif second:\n    b()"),
    ("if_elif_else", "if first: a()\nelif second: b()\nelse: c()"),
    ("if_multiple_elif", "if first: a()\nelif second: b()\nelif third: c()"),
    ("if_nested", "if outer:\n    if inner:\n        act()"),
    ("if_nested_else", "if outer:\n    if inner: a()\n    else: b()\nelse: c()"),
    ("if_walrus", "if (item := next_item()):\n    consume(item)"),
    ("if_semicolon_suite", "if ready: first(); second(); third()"),
    ("if_parenthesized_condition", "if (\n    ready\n    and available\n):\n    act()"),
    ("if_comment_before_else", "if ready:\n    act()\n# branch\nelse:\n    wait()"),
    ("if_blank_suite_lines", "if ready:\n\n    # work\n    act()\n\n    finish()"),
    ("if_tabs", "if ready:\n\tact()\n\tfinish()"),
    ("if_formfeed_indent", "if ready:\n\f    act()"),
    # For and while loops (15).
    ("for_name", "for item in items:\n    consume(item)"),
    ("for_tuple", "for key, value in pairs:\n    consume(key, value)"),
    ("for_list", "for [first, second] in pairs: consume(first, second)"),
    ("for_star", "for first, *rest in rows: consume(first, rest)"),
    ("for_attribute", "for obj.item in items: pass"),
    ("for_subscript", "for values[0] in items: pass"),
    ("for_else", "for item in items:\n    consume(item)\nelse:\n    finish()"),
    ("for_break", "for item in items:\n    if item: break"),
    (
        "for_continue",
        "for item in items:\n    if not item: continue\n    consume(item)",
    ),
    ("for_nested", "for row in rows:\n    for cell in row:\n        consume(cell)"),
    ("for_expression_list", "for item in first, second: consume(item)"),
    ("while_simple", "while ready:\n    act()"),
    ("while_else", "while ready:\n    act()\nelse:\n    finish()"),
    ("while_walrus", "while (item := next_item()): consume(item)"),
    (
        "while_break_continue",
        "while ready:\n    if skip: continue\n    if done: break\n    act()",
    ),
    # Function declarations and their suites (20).
    ("function_empty", "def function(): pass"),
    ("function_positional", "def function(first, second): return first"),
    ("function_defaults", "def function(first=1, second=None): pass"),
    ("function_annotations", "def function(item: list[int]) -> int: return len(item)"),
    ("function_positional_only", "def function(first, second, /): pass"),
    ("function_positional_only_default", "def function(first, /, second=2): pass"),
    ("function_keyword_only", "def function(*, first, second=2): pass"),
    ("function_vararg", "def function(*items): pass"),
    ("function_kwarg", "def function(**options): pass"),
    (
        "function_full_signature",
        "def function(a: int, b=2, /, c=3, *items: str, d, e=5, **options: bool) -> tuple: pass",
    ),
    ("function_trailing_comma", "def function(first, second,): pass"),
    (
        "function_multiline_signature",
        "def function(\n    first: int,\n    *,\n    second: str = 'x',\n) -> None:\n    pass",
    ),
    ("function_decorator", "@decorate\ndef function(): pass"),
    ("function_decorators", "@outer(1)\n@package.inner\ndef function(): pass"),
    ("function_expression_decorator", "@(registry['name'])\ndef function(): pass"),
    (
        "function_nested",
        "def outer():\n    def inner():\n        return 1\n    return inner",
    ),
    ("function_yield", "def values():\n    yield 1\n    yield from source"),
    (
        "function_type_comment",
        "def function(item): # type: (int) -> str\n    return str(item)",
    ),
    (
        "function_parameter_comments",
        "def function(\n    first, # type: int\n    second, # type: str\n):\n    pass",
    ),
    ("function_docstring", "def function():\n    'documentation'\n    return None"),
    # Classes (10).
    ("class_empty", "class Thing: pass"),
    ("class_empty_bases", "class Thing(): pass"),
    ("class_base", "class Thing(Base): pass"),
    ("class_multiple_bases", "class Thing(First, Second): pass"),
    ("class_metaclass", "class Thing(Base, metaclass=Meta): pass"),
    ("class_unpack_bases", "class Thing(*bases, **options): pass"),
    ("class_decorators", "@decorate\n@register('thing')\nclass Thing: pass"),
    (
        "class_attributes_method",
        "class Thing:\n    value: int = 1\n    def method(self):\n        return self.value",
    ),
    ("class_nested", "class Outer:\n    class Inner:\n        pass"),
    (
        "class_multiline_bases",
        "class Thing(\n    Base,\n    metaclass=Meta,\n):\n    pass",
    ),
    # Context managers and exception handlers (20).
    ("with_simple", "with manager: act()"),
    ("with_name", "with manager as value: consume(value)"),
    ("with_tuple_target", "with manager as (first, second): pass"),
    ("with_star_target", "with manager as (first, *rest): pass"),
    ("with_attribute_target", "with manager as obj.value: pass"),
    ("with_subscript_target", "with manager as values[0]: pass"),
    ("with_multiple", "with first() as a, second() as b: consume(a, b)"),
    ("with_parentheses", "with (first() as a, second() as b,): consume(a, b)"),
    ("with_multiline", "with (\n    first() as a,\n    second() as b,\n):\n    act()"),
    ("with_type_comment", "with manager as value: # type: str\n    consume(value)"),
    ("try_bare_except", "try: act()\nexcept: recover()"),
    ("try_typed_except", "try: act()\nexcept Error: recover()"),
    ("try_except_binding", "try: act()\nexcept Error as error: recover(error)"),
    (
        "try_except_tuple",
        "try: act()\nexcept (FirstError, SecondError) as error: recover(error)",
    ),
    (
        "try_multiple_except",
        "try: act()\nexcept FirstError: first()\nexcept SecondError: second()",
    ),
    (
        "try_else_finally",
        "try: act()\nexcept Error: recover()\nelse: success()\nfinally: cleanup()",
    ),
    ("try_finally", "try: act()\nfinally: cleanup()"),
    ("try_except_star", "try: act()\nexcept* Error as errors: recover(errors)"),
    (
        "try_multiple_except_star",
        "try: act()\nexcept* FirstError: first()\nexcept* SecondError: second()",
    ),
    (
        "try_except_star_else_finally",
        "try: act()\nexcept* Error: recover()\nelse: success()\nfinally: cleanup()",
    ),
    # Async statements (10).
    ("async_function", "async def function(): pass"),
    (
        "async_function_decorated",
        "@decorate\nasync def function(item: int) -> int: return item",
    ),
    ("async_function_await", "async def function(): return await result()"),
    ("async_function_yield", "async def values(): yield 1"),
    (
        "async_for",
        "async def function():\n    async for item in items:\n        consume(item)",
    ),
    (
        "async_for_else",
        "async def function():\n    async for item in items: consume(item)\n    else: finish()",
    ),
    (
        "async_for_unpack",
        "async def function():\n    async for first, *rest in items: pass",
    ),
    (
        "async_with",
        "async def function():\n    async with manager as value: consume(value)",
    ),
    (
        "async_with_multiple",
        "async def function():\n    async with first() as a, second() as b: pass",
    ),
    (
        "async_with_parentheses",
        "async def function():\n    async with (first() as a, second() as b,): pass",
    ),
    # Simple statements, scope declarations, and physical lines (15).
    ("pass", "pass"),
    ("return_empty", "def function(): return"),
    ("return_tuple", "def function(): return first, second"),
    ("raise_empty", "raise"),
    ("raise_exception", "raise Error('message')"),
    ("raise_cause", "raise Error('message') from cause"),
    ("raise_suppressed_cause", "raise Error('message') from None"),
    ("assert_simple", "assert ready"),
    ("assert_message", "assert ready, 'not ready'"),
    (
        "global_multiple",
        "def function():\n    global first, second\n    first = second",
    ),
    (
        "nonlocal_multiple",
        "def outer():\n    first = second = 0\n    def inner():\n        nonlocal first, second\n        first += second",
    ),
    ("semicolon_statements", "first = 1; second = 2; pass;"),
    ("explicit_continuation", "result = first + \\\n    second"),
    ("crlf_lines", "if ready:\r\n    act()\r\nelse:\r\n    wait()\r\n"),
    ("assignment_type_comment", "first, second = pair # type: tuple[int, str]"),
]

# Expectations are authored from the grammar, with no host-parser oracle.
# The constructors below only abbreviate tree construction; they do not read source.
PO = A.ParameterKind.POSITIONAL_ONLY
KO = A.ParameterKind.KEYWORD_ONLY
VP = A.ParameterKind.VAR_POSITIONAL
VK = A.ParameterKind.VAR_KEYWORD


def n(identifier):
    return A.Name(identifier=identifier)


def call(identifier, *arguments):
    return A.Call(
        function=n(identifier),
        arguments=[A.CallArgument(value=value) for value in arguments],
    )


def expr(identifier, *arguments):
    return A.ExpressionStatement(expression=call(identifier, *arguments))


def tup(*items):
    return A.Tuple(elements=list(items))


def lst(*items):
    return A.List(elements=list(items))


def attr(identifier, field):
    return A.Attribute(value=n(identifier), attribute=field)


def sub(identifier, index):
    return A.Subscript(value=n(identifier), index=index)


def star(value):
    return A.Unpack(value=value)


def assign(target, value, **fields):
    return A.Assignment(targets=[target], value=value, **fields)


def ann(target, annotation, value=None, parenthesized=False):
    return A.AnnotatedAssignment(
        target=target, annotation=annotation, value=value, parenthesized=parenthesized
    )


def parameter(
    name,
    kind=A.ParameterKind.POSITIONAL_OR_KEYWORD,
    *,
    annotation=None,
    default=None,
    type_comment=None,
):
    return A.Parameter(
        name=name,
        kind=kind,
        annotation=annotation,
        default=default,
        type_comment=type_comment,
    )


def function(
    name="function",
    *,
    parameters=None,
    body=None,
    decorators=None,
    returns=None,
    type_comment=None,
    async_=False,
):
    return A.FunctionDefinition(
        name=name,
        parameters=parameters or [],
        body=body or [A.Pass()],
        decorators=decorators or [],
        return_annotation=returns,
        type_comment=type_comment,
        is_async=async_,
    )


def keyword(value, arg=None):
    kind = A.ArgumentKind.KEYWORD if arg is not None else A.ArgumentKind.DOUBLE_STARRED
    return A.CallArgument(value=value, kind=kind, keyword=arg)


def klass(name="Thing", *, bases=None, keywords=None, decorators=None, body=None):
    arguments = bases or []
    return A.ClassDefinition(
        name=name,
        arguments=arguments + (keywords or []),
        decorators=decorators or [],
        body=body or [A.Pass()],
    )


def branch(condition=None, body=None, orelse=None):
    return A.If(
        condition=n("ready") if condition is None else condition,
        body=body or [expr("act")],
        else_body=orelse or [],
    )


def loop(target=None, iterable=None, body=None, orelse=None, async_=False):
    return A.For(
        target=target or n("item"),
        iterable=iterable or n("items"),
        body=body or [expr("consume", n("item"))],
        else_body=orelse or [],
        is_async=async_,
    )


def while_(condition=None, body=None, orelse=None):
    return A.While(
        condition=condition or n("ready"),
        body=body or [expr("act")],
        else_body=orelse or [],
    )


def item(expression, target=None):
    return A.WithItem(context=expression, target=target)


def with_(items, body=None, type_comment=None, async_=False):
    return A.With(
        items=items, body=body or [A.Pass()], type_comment=type_comment, is_async=async_
    )


def handler(type_=None, name=None, body=None):
    return A.ExceptionHandler(
        exception=type_, binding=name, body=body or [expr("recover")]
    )


def try_(handlers=None, orelse=None, finalbody=None, star_=False):
    return A.Try(
        body=[expr("act")],
        handlers=handlers or [],
        else_body=orelse or [],
        finally_body=finalbody or [],
        exception_groups=star_,
    )


EXPECTED_STATEMENTS = {
    "assign_name": [assign(n("answer"), A.IntegerLiteral(42))],
    "assign_chain": [A.Assignment(targets=[n("left"), n("right")], value=n("value"))],
    "assign_tuple": [assign(tup(n("left"), n("right")), n("pair"))],
    "assign_list": [assign(lst(n("left"), n("right")), n("pair"))],
    "assign_star": [assign(tup(n("head"), star(n("middle")), n("tail")), n("values"))],
    "assign_nested": [
        assign(tup(n("first"), lst(n("second"), star(n("rest")))), n("values"))
    ],
    "assign_attribute": [assign(attr("obj", "value"), n("result"))],
    "assign_subscript": [assign(sub("values", n("index")), n("result"))],
    "assign_slice": [
        assign(
            sub("values", A.Slice(lower=n("start"), upper=n("stop"), step=n("step"))),
            n("result"),
        )
    ],
    "assign_parenthesized": [assign(n("target"), n("value"))],
    "annotate_name": [ann(n("value"), n("int"))],
    "annotate_value": [ann(n("value"), sub("list", n("int")), lst())],
    "annotate_attribute": [ann(attr("obj", "value"), n("int"), A.IntegerLiteral(1))],
    "annotate_subscript": [
        ann(sub("values", A.IntegerLiteral(0)), n("int"), A.IntegerLiteral(1))
    ],
    "annotate_parenthesized": [
        ann(n("value"), n("int"), A.IntegerLiteral(1), parenthesized=True)
    ],
    "augment_name": [
        A.AugmentedAssignment(
            target=n("count"), operator=A.BinaryOperator.ADD, value=A.IntegerLiteral(1)
        )
    ],
    "augment_attribute": [
        A.AugmentedAssignment(
            target=attr("obj", "count"),
            operator=A.BinaryOperator.POWER,
            value=n("exponent"),
        )
    ],
    "augment_subscript": [
        A.AugmentedAssignment(
            target=sub("items", A.IntegerLiteral(0)),
            operator=A.BinaryOperator.RIGHT_SHIFT,
            value=n("bits"),
        )
    ],
    "delete_multiple": [
        A.Delete(
            targets=[
                n("first"),
                attr("obj", "second"),
                sub("values", A.IntegerLiteral(0)),
            ]
        )
    ],
    "delete_nested": [
        A.Delete(targets=[tup(n("first"), lst(n("second"), n("third")))])
    ],
    "import_simple": [A.Import(items=[A.ImportItem(name="package")])],
    "import_dotted": [A.Import(items=[A.ImportItem(name="package.submodule")])],
    "import_alias": [
        A.Import(items=[A.ImportItem(name="package.submodule", alias="local")])
    ],
    "import_multiple": [
        A.Import(
            items=[
                A.ImportItem(name="first"),
                A.ImportItem(name="second", alias="local"),
                A.ImportItem(name="third.child"),
            ]
        )
    ],
    "from_import": [
        A.ImportFrom(module="package", items=[A.ImportItem(name="member")])
    ],
    "from_import_aliases": [
        A.ImportFrom(
            module="package.child",
            items=[
                A.ImportItem(name="first", alias="local"),
                A.ImportItem(name="second"),
            ],
        )
    ],
    "from_import_parentheses": [
        A.ImportFrom(
            module="package",
            items=[
                A.ImportItem(name="first"),
                A.ImportItem(name="second", alias="local"),
            ],
        )
    ],
    "from_import_star": [
        A.ImportFrom(module="package", items=[A.ImportItem(name="*")])
    ],
    "from_import_relative": [
        A.ImportFrom(module="package", level=2, items=[A.ImportItem(name="member")])
    ],
    "from_import_relative_only": [
        A.ImportFrom(module=None, level=3, items=[A.ImportItem(name="member")])
    ],
    "if_block": [branch()],
    "if_inline": [branch()],
    "if_else": [branch(orelse=[expr("wait")])],
    "if_elif": [branch(n("first"), [expr("a")], [branch(n("second"), [expr("b")])])],
    "if_elif_else": [
        branch(n("first"), [expr("a")], [branch(n("second"), [expr("b")], [expr("c")])])
    ],
    "if_multiple_elif": [
        branch(
            n("first"),
            [expr("a")],
            [branch(n("second"), [expr("b")], [branch(n("third"), [expr("c")])])],
        )
    ],
    "if_nested": [branch(n("outer"), [branch(n("inner"))])],
    "if_nested_else": [
        branch(n("outer"), [branch(n("inner"), [expr("a")], [expr("b")])], [expr("c")])
    ],
    "if_walrus": [
        branch(
            A.NamedExpression(target=n("item"), value=call("next_item")),
            [expr("consume", n("item"))],
        )
    ],
    "if_semicolon_suite": [branch(body=[expr("first"), expr("second"), expr("third")])],
    "if_parenthesized_condition": [
        branch(
            A.Boolean(
                operator=A.BooleanOperator.AND, operands=[n("ready"), n("available")]
            )
        )
    ],
    "if_comment_before_else": [branch(orelse=[expr("wait")])],
    "if_blank_suite_lines": [branch(body=[expr("act"), expr("finish")])],
    "if_tabs": [branch(body=[expr("act"), expr("finish")])],
    "if_formfeed_indent": [branch()],
    "for_name": [loop()],
    "for_tuple": [
        loop(
            tup(n("key"), n("value")),
            n("pairs"),
            [expr("consume", n("key"), n("value"))],
        )
    ],
    "for_list": [
        loop(
            lst(n("first"), n("second")),
            n("pairs"),
            [expr("consume", n("first"), n("second"))],
        )
    ],
    "for_star": [
        loop(
            tup(n("first"), star(n("rest"))),
            n("rows"),
            [expr("consume", n("first"), n("rest"))],
        )
    ],
    "for_attribute": [loop(attr("obj", "item"), body=[A.Pass()])],
    "for_subscript": [loop(sub("values", A.IntegerLiteral(0)), body=[A.Pass()])],
    "for_else": [loop(orelse=[expr("finish")])],
    "for_break": [loop(body=[branch(n("item"), [A.Break()])])],
    "for_continue": [
        loop(
            body=[
                branch(
                    A.Unary(operator=A.UnaryOperator.NOT, operand=n("item")),
                    [A.Continue()],
                ),
                expr("consume", n("item")),
            ]
        )
    ],
    "for_nested": [
        loop(
            n("row"),
            n("rows"),
            [loop(n("cell"), n("row"), [expr("consume", n("cell"))])],
        )
    ],
    "for_expression_list": [loop(iterable=tup(n("first"), n("second")))],
    "while_simple": [while_()],
    "while_else": [while_(orelse=[expr("finish")])],
    "while_walrus": [
        while_(
            A.NamedExpression(target=n("item"), value=call("next_item")),
            [expr("consume", n("item"))],
        )
    ],
    "while_break_continue": [
        while_(
            body=[
                branch(n("skip"), [A.Continue()]),
                branch(n("done"), [A.Break()]),
                expr("act"),
            ]
        )
    ],
    "function_empty": [function()],
    "function_positional": [
        function(
            parameters=[parameter("first"), parameter("second")],
            body=[A.Return(value=n("first"))],
        )
    ],
    "function_defaults": [
        function(
            parameters=[
                parameter("first", default=A.IntegerLiteral(1)),
                parameter("second", default=A.NoneLiteral()),
            ]
        )
    ],
    "function_annotations": [
        function(
            parameters=[parameter("item", annotation=sub("list", n("int")))],
            returns=n("int"),
            body=[A.Return(value=call("len", n("item")))],
        )
    ],
    "function_positional_only": [
        function(parameters=[parameter("first", PO), parameter("second", PO)])
    ],
    "function_positional_only_default": [
        function(
            parameters=[
                parameter("first", PO),
                parameter("second", default=A.IntegerLiteral(2)),
            ]
        )
    ],
    "function_keyword_only": [
        function(
            parameters=[
                parameter("first", KO),
                parameter("second", KO, default=A.IntegerLiteral(2)),
            ]
        )
    ],
    "function_vararg": [function(parameters=[parameter("items", VP)])],
    "function_kwarg": [function(parameters=[parameter("options", VK)])],
    "function_full_signature": [
        function(
            parameters=[
                parameter("a", PO, annotation=n("int")),
                parameter("b", PO, default=A.IntegerLiteral(2)),
                parameter("c", default=A.IntegerLiteral(3)),
                parameter("items", VP, annotation=n("str")),
                parameter("d", KO),
                parameter("e", KO, default=A.IntegerLiteral(5)),
                parameter("options", VK, annotation=n("bool")),
            ],
            returns=n("tuple"),
        )
    ],
    "function_trailing_comma": [
        function(parameters=[parameter("first"), parameter("second")])
    ],
    "function_multiline_signature": [
        function(
            parameters=[
                parameter("first", annotation=n("int")),
                parameter(
                    "second", KO, annotation=n("str"), default=A.TextLiteral("x")
                ),
            ],
            returns=A.NoneLiteral(),
        )
    ],
    "function_decorator": [function(decorators=[n("decorate")])],
    "function_decorators": [
        function(
            decorators=[call("outer", A.IntegerLiteral(1)), attr("package", "inner")]
        )
    ],
    "function_expression_decorator": [
        function(decorators=[sub("registry", A.TextLiteral("name"))])
    ],
    "function_nested": [
        function(
            "outer",
            body=[
                function("inner", body=[A.Return(value=A.IntegerLiteral(1))]),
                A.Return(value=n("inner")),
            ],
        )
    ],
    "function_yield": [
        function(
            "values",
            body=[
                A.ExpressionStatement(expression=A.Yield(value=A.IntegerLiteral(1))),
                A.ExpressionStatement(
                    expression=A.Yield(value=n("source"), from_iterable=True)
                ),
            ],
        )
    ],
    "function_type_comment": [
        function(
            parameters=[parameter("item")],
            type_comment="(int) -> str",
            body=[A.Return(value=call("str", n("item")))],
        )
    ],
    "function_parameter_comments": [
        function(
            parameters=[
                parameter("first", type_comment="int"),
                parameter("second", type_comment="str"),
            ]
        )
    ],
    "function_docstring": [
        function(
            body=[
                A.ExpressionStatement(expression=A.TextLiteral("documentation")),
                A.Return(value=A.NoneLiteral()),
            ]
        )
    ],
    "class_empty": [klass()],
    "class_empty_bases": [klass()],
    "class_base": [klass(bases=[A.CallArgument(n("Base"))])],
    "class_multiple_bases": [
        klass(bases=[A.CallArgument(n("First")), A.CallArgument(n("Second"))])
    ],
    "class_metaclass": [
        klass(
            bases=[A.CallArgument(n("Base"))],
            keywords=[keyword(arg="metaclass", value=n("Meta"))],
        )
    ],
    "class_unpack_bases": [
        klass(
            bases=[A.CallArgument(n("bases"), A.ArgumentKind.STARRED)],
            keywords=[keyword(value=n("options"))],
        )
    ],
    "class_decorators": [
        klass(decorators=[n("decorate"), call("register", A.TextLiteral("thing"))])
    ],
    "class_attributes_method": [
        klass(
            body=[
                ann(n("value"), n("int"), A.IntegerLiteral(1)),
                function(
                    "method",
                    parameters=[parameter("self")],
                    body=[A.Return(value=attr("self", "value"))],
                ),
            ]
        )
    ],
    "class_nested": [klass("Outer", body=[klass("Inner")])],
    "class_multiline_bases": [
        klass(
            bases=[A.CallArgument(n("Base"))],
            keywords=[keyword(arg="metaclass", value=n("Meta"))],
        )
    ],
    "with_simple": [with_([item(n("manager"))], [expr("act")])],
    "with_name": [
        with_([item(n("manager"), n("value"))], [expr("consume", n("value"))])
    ],
    "with_tuple_target": [with_([item(n("manager"), tup(n("first"), n("second")))])],
    "with_star_target": [with_([item(n("manager"), tup(n("first"), star(n("rest"))))])],
    "with_attribute_target": [with_([item(n("manager"), attr("obj", "value"))])],
    "with_subscript_target": [
        with_([item(n("manager"), sub("values", A.IntegerLiteral(0)))])
    ],
    "with_multiple": [
        with_(
            [item(call("first"), n("a")), item(call("second"), n("b"))],
            [expr("consume", n("a"), n("b"))],
        )
    ],
    "with_parentheses": [
        with_(
            [item(call("first"), n("a")), item(call("second"), n("b"))],
            [expr("consume", n("a"), n("b"))],
        )
    ],
    "with_multiline": [
        with_(
            [item(call("first"), n("a")), item(call("second"), n("b"))], [expr("act")]
        )
    ],
    "with_type_comment": [
        with_(
            [item(n("manager"), n("value"))],
            [expr("consume", n("value"))],
            type_comment="str",
        )
    ],
    "try_bare_except": [try_([handler()])],
    "try_typed_except": [try_([handler(n("Error"))])],
    "try_except_binding": [
        try_([handler(n("Error"), "error", [expr("recover", n("error"))])])
    ],
    "try_except_tuple": [
        try_(
            [
                handler(
                    tup(n("FirstError"), n("SecondError")),
                    "error",
                    [expr("recover", n("error"))],
                )
            ]
        )
    ],
    "try_multiple_except": [
        try_(
            [
                handler(n("FirstError"), body=[expr("first")]),
                handler(n("SecondError"), body=[expr("second")]),
            ]
        )
    ],
    "try_else_finally": [
        try_([handler(n("Error"))], [expr("success")], [expr("cleanup")])
    ],
    "try_finally": [try_(finalbody=[expr("cleanup")])],
    "try_except_star": [
        try_(
            [handler(n("Error"), "errors", [expr("recover", n("errors"))])], star_=True
        )
    ],
    "try_multiple_except_star": [
        try_(
            [
                handler(n("FirstError"), body=[expr("first")]),
                handler(n("SecondError"), body=[expr("second")]),
            ],
            star_=True,
        )
    ],
    "try_except_star_else_finally": [
        try_([handler(n("Error"))], [expr("success")], [expr("cleanup")], star_=True)
    ],
    "async_function": [function(async_=True)],
    "async_function_decorated": [
        function(
            parameters=[parameter("item", annotation=n("int"))],
            returns=n("int"),
            decorators=[n("decorate")],
            body=[A.Return(value=n("item"))],
            async_=True,
        )
    ],
    "async_function_await": [
        function(body=[A.Return(value=A.Await(value=call("result")))], async_=True)
    ],
    "async_function_yield": [
        function(
            "values",
            body=[A.ExpressionStatement(expression=A.Yield(value=A.IntegerLiteral(1)))],
            async_=True,
        )
    ],
    "async_for": [function(body=[loop(async_=True)], async_=True)],
    "async_for_else": [
        function(body=[loop(orelse=[expr("finish")], async_=True)], async_=True)
    ],
    "async_for_unpack": [
        function(
            body=[loop(tup(n("first"), star(n("rest"))), body=[A.Pass()], async_=True)],
            async_=True,
        )
    ],
    "async_with": [
        function(
            body=[
                with_(
                    [item(n("manager"), n("value"))],
                    [expr("consume", n("value"))],
                    async_=True,
                )
            ],
            async_=True,
        )
    ],
    "async_with_multiple": [
        function(
            body=[
                with_(
                    [item(call("first"), n("a")), item(call("second"), n("b"))],
                    async_=True,
                )
            ],
            async_=True,
        )
    ],
    "async_with_parentheses": [
        function(
            body=[
                with_(
                    [item(call("first"), n("a")), item(call("second"), n("b"))],
                    async_=True,
                )
            ],
            async_=True,
        )
    ],
    "pass": [A.Pass()],
    "return_empty": [function(body=[A.Return()])],
    "return_tuple": [function(body=[A.Return(value=tup(n("first"), n("second")))])],
    "raise_empty": [A.Raise()],
    "raise_exception": [A.Raise(exception=call("Error", A.TextLiteral("message")))],
    "raise_cause": [
        A.Raise(exception=call("Error", A.TextLiteral("message")), cause=n("cause"))
    ],
    "raise_suppressed_cause": [
        A.Raise(
            exception=call("Error", A.TextLiteral("message")), cause=A.NoneLiteral()
        )
    ],
    "assert_simple": [A.Assert(condition=n("ready"))],
    "assert_message": [
        A.Assert(condition=n("ready"), message=A.TextLiteral("not ready"))
    ],
    "global_multiple": [
        function(
            body=[
                A.Global(identifiers=["first", "second"]),
                assign(n("first"), n("second")),
            ]
        )
    ],
    "nonlocal_multiple": [
        function(
            "outer",
            body=[
                A.Assignment(
                    targets=[n("first"), n("second")], value=A.IntegerLiteral(0)
                ),
                function(
                    "inner",
                    body=[
                        A.Nonlocal(identifiers=["first", "second"]),
                        A.AugmentedAssignment(
                            target=n("first"),
                            operator=A.BinaryOperator.ADD,
                            value=n("second"),
                        ),
                    ],
                ),
            ],
        )
    ],
    "semicolon_statements": [
        assign(n("first"), A.IntegerLiteral(1)),
        assign(n("second"), A.IntegerLiteral(2)),
        A.Pass(),
    ],
    "explicit_continuation": [
        assign(
            n("result"),
            A.Binary(left=n("first"), operator=A.BinaryOperator.ADD, right=n("second")),
        )
    ],
    "crlf_lines": [branch(orelse=[expr("wait")])],
    "assignment_type_comment": [
        assign(tup(n("first"), n("second")), n("pair"), type_comment="tuple[int, str]")
    ],
}


@pytest.mark.parametrize(
    "case, source", STATEMENTS, ids=[case for case, _ in STATEMENTS]
)
def test_statement_has_expected_mule_ast(parse, case, source):
    actual = parse(source, filename=f"{case}.py", type_comments=True)
    expected = A.Module(statements=EXPECTED_STATEMENTS[case])
    assert_ast_equal(actual, expected)


def test_chained_assignment_preserves_target_order(parse):
    tree = parse("left = right = source")
    assignment = one(tree, q.Assignment)
    assert [target.identifier for target in assignment.targets] == ["left", "right"]
    assert assignment.value.identifier == "source"


def test_nested_assignment_keeps_unpack_in_target_tree(parse):
    tree = parse("first, [second, *rest] = values")
    target = at(tree, "statements.0.targets.0")
    assert_ast_equal(target, tup(n("first"), lst(n("second"), star(n("rest")))))
    assert target.elements[0].identifier == "first"
    assert_ast_equal(target.elements[1], lst(n("second"), star(n("rest"))))
    assert target.elements[1].elements[0].identifier == "second"
    assert one(tree, q.Unpack).value.identifier == "rest"
    assert at(tree, "statements.0.value").identifier == "values"


def test_attribute_assignment_retains_receiver_expression(parse):
    tree = parse("obj.value = result")
    target = one(tree, q.Attribute, attribute="value")
    assert target.value.identifier == "obj"
    assert one(tree, q.Assignment).targets == [target]
    assert one(tree, q.Assignment).value.identifier == "result"


def test_annotation_records_parenthesized_target(parse):
    tree = parse("plain: int\n(parenthesized): int")
    annotations = nodes(tree, q.AnnotatedAssignment)
    assert [node.parenthesized for node in annotations] == [False, True]
    assert [node.target.identifier for node in annotations] == [
        "plain",
        "parenthesized",
    ]
    assert all(node.value is None for node in annotations)


def test_augmented_assignment_keeps_target_and_power_operator(parse):
    tree = parse("values[index] **= amount")
    statement = one(tree, q.AugmentedAssignment)
    assert statement.operator is A.BinaryOperator.POWER
    assert statement.target.value.identifier == "values"
    assert statement.target.index.identifier == "index"
    assert statement.value.identifier == "amount"


def test_delete_preserves_multiple_target_shapes(parse):
    tree = parse("del first, obj.value, values[index]")
    statement = one(tree, q.Delete)
    assert_ast_equal(
        statement,
        A.Delete([n("first"), attr("obj", "value"), sub("values", n("index"))]),
    )
    assert statement.targets[0].identifier == "first"
    assert statement.targets[1].value.identifier == "obj"
    assert statement.targets[2].index.identifier == "index"


def test_relative_import_level_and_aliases(parse):
    tree = parse("from ...package.child import first as local, second")
    statement = one(tree, q.ImportFrom)
    assert statement.level == 3
    assert statement.module == "package.child"
    assert [(item.name, item.alias) for item in statement.items] == [
        ("first", "local"),
        ("second", None),
    ]


def test_elif_is_nested_conditional_in_else_body(parse):
    tree = parse("if first: a()\nelif second: b()\nelse: c()")
    outer = at(tree, "statements.0")
    inner = at(tree, "statements.0.else_body.0")
    assert_ast_equal(
        outer,
        branch(
            n("first"), [expr("a")], [branch(n("second"), [expr("b")], [expr("c")])]
        ),
    )
    assert_ast_equal(inner, branch(n("second"), [expr("b")], [expr("c")]))
    assert inner.condition.identifier == "second"
    assert inner.else_body[0].expression.function.identifier == "c"


def test_for_else_and_unpack_target(parse):
    tree = parse("for head, *tail in rows:\n    consume(head)\nelse:\n    finish()")
    statement = one(tree, q.For)
    assert_ast_equal(statement.target, tup(n("head"), star(n("tail"))))
    assert statement.target.elements[0].identifier == "head"
    assert_ast_equal(statement.target.elements[1], star(n("tail")))
    assert statement.target.elements[1].value.identifier == "tail"
    assert statement.iterable.identifier == "rows"
    assert statement.else_body[0].expression.function.identifier == "finish"


def test_with_items_preserve_order_and_optional_targets(parse):
    tree = parse("with first() as value, second(), third() as (a, b): pass")
    statement = one(tree, q.With)
    assert [item.context.function.identifier for item in statement.items] == [
        "first",
        "second",
        "third",
    ]
    assert statement.items[0].target.identifier == "value"
    assert statement.items[1].target is None
    assert_ast_equal(statement.items[2].target, tup(n("a"), n("b")))
    assert [element.identifier for element in statement.items[2].target.elements] == [
        "a",
        "b",
    ]


def test_except_star_records_exception_group_handling(parse):
    tree = parse(
        "try: work()\nexcept* Error as errors: recover(errors)\nelse: success()\nfinally: cleanup()"
    )
    statement = one(tree, q.Try)
    assert statement.exception_groups is True
    assert statement.handlers[0].exception.identifier == "Error"
    assert statement.handlers[0].binding == "errors"
    assert statement.else_body[0].expression.function.identifier == "success"
    assert statement.finally_body[0].expression.function.identifier == "cleanup"


def test_function_parameters_keep_source_order_and_individual_defaults(parse):
    tree = parse("def function(a, b=2, /, c=3, *items, d, e=5, **options): pass")
    parameters = one(tree, q.FunctionDefinition).parameters
    assert [parameter.name for parameter in parameters] == [
        "a",
        "b",
        "c",
        "items",
        "d",
        "e",
        "options",
    ]
    assert [parameter.kind for parameter in parameters] == [
        PO,
        PO,
        A.ParameterKind.POSITIONAL_OR_KEYWORD,
        VP,
        KO,
        KO,
        VK,
    ]
    assert [
        parameter.default.value if parameter.default is not None else None
        for parameter in parameters
    ] == [None, 2, 3, None, None, 5, None]


def test_class_decorators_and_arguments_preserve_source_order(parse):
    tree = parse(
        "@outer\n@inner(1)\nclass Thing(Base, metaclass=Meta, **options): pass"
    )
    statement = one(tree, q.ClassDefinition)
    assert statement.decorators[0].identifier == "outer"
    assert statement.decorators[1].function.identifier == "inner"
    assert [argument.kind for argument in statement.arguments] == [
        A.ArgumentKind.POSITIONAL,
        A.ArgumentKind.KEYWORD,
        A.ArgumentKind.DOUBLE_STARRED,
    ]
    assert [argument.keyword for argument in statement.arguments] == [
        None,
        "metaclass",
        None,
    ]
    assert [argument.value.identifier for argument in statement.arguments] == [
        "Base",
        "Meta",
        "options",
    ]


def test_async_compound_statements_keep_async_flags(parse):
    tree = parse(
        "async def function():\n    async with manager as value:\n        async for item in values:\n            await consume(item)"
    )
    function = one(tree, q.FunctionDefinition)
    assert function.name == "function"
    assert function.is_async is True
    assert one(tree, q.With).is_async is True
    assert one(tree, q.For).is_async is True
    assert one(tree, q.Await).value.function.identifier == "consume"


def test_type_comments_attach_to_assignment_loop_and_function(parse):
    source = (
        "value = [] # type: list[int]\n"
        "for item in values: # type: int\n"
        "    pass\n"
        "def function(item): # type: (int) -> str\n"
        "    return str(item)\n"
    )
    tree = parse(source, type_comments=True)
    assert one(tree, q.Assignment).type_comment == "list[int]"
    assert one(tree, q.For).type_comment == "int"
    assert one(tree, q.FunctionDefinition).type_comment == "(int) -> str"
