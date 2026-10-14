"""Public AST parsing uses Mule's frontend with Python-compatible node fields."""

import pytest

from test.mule.compiler.test_exception_groups import compare


@pytest.mark.integration
def test_runtime_ast_fields_modes_targets_patterns_and_type_parameters(tmp_path):
    result = compare(
        tmp_path,
        """
import ast
sources = (
    'value = café + 2\\nassert value == 4\\n',
    'def function(x, /, y=2, *args, key=None, **kwargs):\\n return x if x else y\\n',
    '@decorator\\nclass Example(Base, metaclass=Meta):\\n pass\\n',
    'values = [x * 2 for x in items if x > 1]\\n',
    'value = {key: item for key, item in entries}\\n',
    'async def function():\\n async for item in items:\\n  await consume(item)\\n',
    'try:\\n work()\\nexcept* ValueError as error:\\n raise RuntimeError() from error\\nfinally:\\n finish()\\n',
    'match subject:\\n case None | True:\\n  pass\\n case [first, *rest]:\\n  pass\\n case {\"key\": item, **remaining}:\\n  pass\\n',
    'type Alias[T: int = str, *Ts, **P] = tuple[T, *Ts]\\n',
    'def generic[T](value: T) -> T:\\n return value\\n',
    'value = f\"result={number!r:04}\"\\n',
    'value = t\"result={number!r:04}\"\\n',
    '(target): int = 3\\ndel target, values[0]\\n',
)
for source in sources:
    print(ast.dump(ast.parse(source), include_attributes=False))
print(ast.dump(ast.parse('40 + 2', mode='eval')))
print(ast.dump(ast.parse('value = 3\\n', mode='single')))
tree = ast.parse('café = 2\\n')
print(ast.dump(tree, include_attributes=True))
tree = ast.parse('@decorator\\ndef function(argument: int = 3):\\n pass\\n')
print(ast.dump(tree, include_attributes=True))
only_ast = compile('40 + 2', '<expr>', 'eval', ast.PyCF_ONLY_AST)
print(ast.dump(only_ast))
code_obj = compile('x * y + 2', '<calc>', 'eval')
print(code_obj.co_name, code_obj.co_filename, code_obj.co_firstlineno)
print(eval(code_obj, {'x': 6}, {'y': 7}))
custom_expr = ast.Expression(
    ast.BoolOp(
        ast.And(),
        [
            ast.UnaryOp(ast.Not(), ast.Name('$integration', ast.Load())),
            ast.Call(
                ast.Name('$mark', ast.Load()),
                [],
                [ast.keyword('level', ast.Constant(2))],
            ),
        ],
    )
)
ast.fix_missing_locations(custom_expr)
compiled_custom = compile(custom_expr, '<pytest match expression>', 'eval')
class Lookup:
    def __getitem__(self, key):
        if key == '$integration':
            return False
        if key == '$mark':
            return lambda **kw: kw == {'level': 2}
        raise KeyError(key)
print(bool(eval(compiled_custom, {'__builtins__': {}}, Lookup())))
try:
    compile('1 +', '<bad>', 'eval')
except SyntaxError as exc:
    print(type(exc).__name__, exc.filename, exc.lineno)
""",
    )
    assert result.stderr == ""
