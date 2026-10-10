from mule import ast


def test_walk_visits_root_and_nested_children_in_source_order():
    first = ast.Name("first")
    second = ast.Name("second")
    root = ast.Tuple([first, ast.List([second])])
    assert list(ast.walk(root)) == [root, first, root.elements[1], second]


def test_children_visits_direct_nodes_without_descending():
    left = ast.Name("left")
    right = ast.Unary(ast.UnaryOperator.NEGATIVE, ast.IntegerLiteral(1))
    root = ast.Binary(ast.BinaryOperator.ADD, left, right)
    assert list(ast.children(root)) == [left, right]


def test_walk_visits_nodes_in_call_argument_records():
    call = ast.Call(
        ast.Name("f"), [ast.CallArgument(ast.Name("items"), ast.ArgumentKind.STARRED)]
    )
    assert list(ast.walk(call)) == [
        call,
        call.function,
        call.arguments[0],
        call.arguments[0].value,
    ]


def test_walk_omits_coordinates_and_operator_metadata():
    expression = ast.Name("x", span=ast.Span(ast.Position(1, 0), ast.Position(1, 1)))
    assert list(ast.walk(expression)) == [expression]


def test_node_list_defaults_are_independently_owned():
    first = ast.Module()
    second = ast.Module()
    first.statements.append(ast.Pass())
    assert second.statements == []


def test_missing_parameter_default_differs_from_none_literal():
    required = ast.Parameter("required")
    optional = ast.Parameter("optional", default=ast.NoneLiteral())
    assert required.default is None
    assert optional.default == ast.NoneLiteral()


def test_calls_preserve_interleaved_argument_order():
    call = ast.Call(
        ast.Name("f"),
        [
            ast.CallArgument(ast.IntegerLiteral(1), ast.ArgumentKind.KEYWORD, "named"),
            ast.CallArgument(ast.Name("items"), ast.ArgumentKind.STARRED),
        ],
    )
    assert [argument.kind for argument in call.arguments] == [
        ast.ArgumentKind.KEYWORD,
        ast.ArgumentKind.STARRED,
    ]


def test_type_parameter_constraints_are_separate_from_bound():
    constrained = ast.TypeParameter("T", constraints=[ast.Name("int"), ast.Name("str")])
    bounded = ast.TypeParameter("U", bound=ast.Name("int"))
    assert constrained.bound is None
    assert bounded.constraints == []


def test_unpacked_dictionary_entry_has_no_key():
    entry = ast.DictEntry(None, ast.Name("mapping"))
    assert list(ast.children(entry)) == [entry.value]


def test_pattern_walk_reaches_capture_and_mapping_key():
    pattern = ast.MappingPattern(
        [ast.PatternEntry(ast.TextLiteral("key"), ast.CapturePattern("item"))],
        rest="rest",
    )
    assert list(ast.walk(pattern)) == [
        pattern,
        pattern.entries[0],
        pattern.entries[0].key,
        pattern.entries[0].pattern,
    ]
