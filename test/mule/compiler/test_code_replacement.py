"""Native code metadata copies preserve executable entry and function isolation."""

from test.mule.compiler.test_exception_groups import compare


def test_replace_flags_validation_and_generator_function_metadata(tmp_path):
    result = compare(
        tmp_path,
        """
import types
def generate():
    yield 42
original = generate.__code__
changed = original.replace(co_flags=original.co_flags | 0x100)
print(changed is original, original.co_flags, changed.co_flags)
print(changed.co_name == original.co_name, changed.co_freevars == original.co_freevars)
generate.__code__ = changed
print(generate.__code__ is changed, next(generate()))
print(types.coroutine(generate) is generate)
for changes in ({'co_flags': -1}, {'co_flags': 1 << 40}, {'co_flags': 'x'},
                {'bad_keyword': 0}):
    try:
        original.replace(**changes)
    except (TypeError, ValueError, OverflowError) as error:
        print(type(error).__name__, str(error))
try:
    generate.__code__ = None
except TypeError as error:
    print(str(error))
""",
    )
    assert result.stderr == ""
