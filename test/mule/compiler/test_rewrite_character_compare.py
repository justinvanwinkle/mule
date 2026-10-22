"""Character comparisons in a runtime-input tokenizer and escaping near-misses."""

import re
from textwrap import dedent

import pytest

from test.mule.compiler.rewrite_support import build_case

pytestmark = pytest.mark.integration


@pytest.fixture(scope="module")
def character_case(tmp_path_factory):
    return build_case(
        tmp_path_factory.mktemp("character-compare"),
        dedent(
            """\
            def tokenize(text):
                # Each token records its category and source span. Characters
                # remain comparison-only locals even across quoted strings.
                tokens = []
                i = 0
                n = len(text)
                while i < n:
                    ch = text[i]
                    if ch == ' ' or ch == '\\t':
                        i += 1
                    elif ch == '"':
                        start = i
                        i += 1
                        while i < n:
                            inner = text[i]
                            if inner == '\\\\':
                                i += 2
                            elif inner == '"':
                                i += 1
                                break
                            else:
                                i += 1
                        tokens.append((1, start, i))
                    elif '0' <= ch <= '9':
                        start = i
                        i += 1
                        while i < n and '0' <= text[i] <= '9':
                            i += 1
                        tokens.append((2, start, i))
                    elif ch == '[' or ch == ']' or ch == ',':
                        tokens.append((3, i, i + 1))
                        i += 1
                    elif 'α' <= ch <= 'ω' or ch == '😀':
                        tokens.append((4, i, i + 1))
                        i += 1
                    else:
                        tokens.append((5, i, i + 1))
                        i += 1
                return tokens

            def indexed(text, index):
                ch = text[index]
                return ch == 'α', ch != '😀', ch < 'β', 'ω' >= ch

            def escaped(text, index):
                ch = text[index]
                return ch, ch == 'α'

            def multi_character(text, index):
                ch = text[index]
                return ch == '==', ch != '', ch < 'αβ'

            def mixed_assignment(text, index, replace):
                ch = text[index]
                if replace:
                    ch = 'α'
                return ch == 'α'

            text = input()
            index = int(input())
            print(tokenize(text))
            try:
                print(indexed(text, index))
                print(escaped(text, index))
                print(multi_character(text, index))
                print(mixed_assignment(text, index, index < 0))
                print(mixed_assignment(text, index, index >= 0))
            except IndexError:
                print('index-error')
            print(text)
            """
        ),
        optimize=3,
        lto=False,
    )


@pytest.mark.parametrize(
    ("text", "index"),
    [
        ('[12, "a\\"b", α😀]', 0),
        ("éαβω😀", -1),
        ("éαβω😀", -5),
        ("éαβω😀", 1),
        ("éαβω😀", 5),
        ("éαβω😀", -6),
        ("", 0),
        ("α", 2**100),
        ("α", -(2**100)),
    ],
)
def test_character_tokenizer_and_index_boundaries_match_python(
    character_case, text, index
):
    character_case.compare(stdin=f"{text}\n{index}\n")


def test_character_tokenizer_uses_codepoints_without_character_objects(character_case):
    # input() reaches the general body. The emitted list specialization is
    # guarded by mule_exact_list and is not the path this program executes.
    body = character_case.function_ir(r"^mule_fn_tokenize\.body$")
    assert "@mule_fast_getitem(" not in body, (
        "Runtime text scanning still constructs character objects through generic indexing"
    )
    assert "@mule.protocol.getitem(" not in body
    assert re.search(r"\bicmp (?:eq|ne) i64\b", body)
    assert re.search(r"\bicmp (?:slt|sle|sgt|sge|ult|ule|ugt|uge) i64\b", body)
    assert "@mule.fast.getitem(" not in body
    assert "@mule_compare(" not in body
    assert "@mule_compare_bool(" not in body

    # Inspect the linked physical helper, not merely its selection in the loop.
    # Bounds/error routines remain legal, but successful indexing must load a
    # storage unit and tag its codepoint rather than allocate a character.
    if "@mule_text_codepoint_getitem(" in body:
        physical = character_case.function_assembly(r"^mule_text_codepoint_getitem$")
        assert re.search(r"\bmovz(?:b|w)l\b", physical)
        assert re.search(r"\b(?:shl|sal|lea)\w*\b", physical)
    else:
        # An inlined storage access must preserve the same native operation.
        assert re.search(r"getelementptr(?: inbounds)? (?:nuw )?i(?:8|16|32),", body)
        assert re.search(r"\bload i(?:8|16|32),", body)
        assert re.search(r"\bzext i(?:8|16|32) .* to i64", body)
        physical = body
    for forbidden in (
        "latin1_character",
        "from_codepoints",
        "new_value",
        "__rust_alloc",
        "mule_getitem>",
        "mule_text_getitem>",
        "mule_compare>",
    ):
        assert forbidden not in physical


@pytest.mark.parametrize("function", ["escaped", "multi_character", "mixed_assignment"])
def test_character_noncomparison_uses_retain_text_representation(
    character_case, function
):
    # These functions need no specialized entry. Check the pre-inline body so
    # O3 may inline or remove its call without making matcher coverage vacuous.
    body = character_case.function_ir(rf"^mule_fn_{function}\.body$", optimized=False)
    assert "@mule.fast.getitem(" in body
    assert "@mule_text_codepoint_getitem(" not in body
