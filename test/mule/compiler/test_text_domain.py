"""Bounded content facts, independent of compiler activation and identity."""

import itertools
import json

import pytest

from mule.compiler.text_domain import AFFIX_LIMIT, TextContent


def test_literal_content_counts_unicode_characters_and_bounds_storage():
    fact = TextContent.from_literal("é🙂abcXYZ")
    assert fact.min_length == fact.max_length == 8
    assert fact.prefix == "é🙂abc"
    assert fact.suffix == "bcXYZ"
    assert fact.matches("é🙂abcXYZ")
    assert not fact.matches("é🙂abcXY")
    assert TextContent.from_literal("e\u0301").min_length == 2
    long = TextContent.from_literal("a" * 10000)
    assert len(long.prefix) == len(long.suffix) == AFFIX_LIMIT
    assert long.min_length == long.max_length == 10000
    # The middle is deliberately forgotten, not an exact literal/identity fact.
    assert TextContent.from_literal("abcdeXfghij").matches("abcdeYfghij")


def test_join_retains_common_guarantees_not_exact_literal_identity():
    left = TextContent.from_literal("ops-add")
    right = TextContent.from_literal("ops-remove")
    joined = left.join(right)
    assert joined == TextContent(7, 10, "ops-")
    assert joined.matches("ops-add") and joined.matches("ops-remove")
    assert not joined.matches("query-list")
    assert left.fits(joined) and right.fits(joined)
    assert not joined.fits(left)
    assert joined.join(TextContent()) == TextContent()


@pytest.mark.parametrize(
    ("fact", "expected"),
    [
        (TextContent(0, 2, "a", "b"), TextContent.from_literal("ab")),
        (TextContent(2, 3, "ab", "ba"), TextContent.from_literal("aba")),
        (TextContent(2, 3, "ab", "ab"), TextContent.from_literal("ab")),
        (TextContent(2, 4, "ab", "ba"), TextContent(3, 4, "ab", "ba")),
        (TextContent(4, 5, "aaa", "aaaa"), TextContent(4, 5, "aaaa", "aaaa")),
    ],
)
def test_overlapping_ends_canonicalize_implied_constraints(fact, expected):
    assert fact == expected
    assert fact.fits(expected) and expected.fits(fact)


@pytest.mark.parametrize(
    ("left", "right"),
    [
        (TextContent(prefix="a"), TextContent(prefix="b")),
        (TextContent(suffix="a"), TextContent(suffix="b")),
        (TextContent(max_length=2), TextContent(min_length=3)),
        (TextContent(2, 2, "a"), TextContent(2, 2, suffix="bb")),
    ],
)
def test_disjoint_intersection_has_no_possible_text(left, right):
    assert left.meet(right) is None
    assert right.meet(left) is None


@pytest.mark.parametrize(
    "fields",
    [
        {"min_length": -1},
        {"min_length": 3, "max_length": 2},
        {"max_length": 1, "prefix": "ab"},
        {"min_length": 2, "max_length": 2, "prefix": "ab", "suffix": "ba"},
        {"prefix": "a" * (AFFIX_LIMIT + 1)},
        {"suffix": "a" * (AFFIX_LIMIT + 1)},
    ],
)
def test_inconsistent_or_unbounded_affix_storage_is_rejected(fields):
    with pytest.raises(ValueError):
        TextContent(**fields)


def test_widening_stabilizes_growing_lengths_and_weakening_ends():
    old = TextContent(4, 8, "ops-", "!")
    grown = old.widen(TextContent(4, 9, "ops-", "!"))
    assert grown == TextContent(4, None, "ops-", "!")
    assert grown.widen(TextContent(4, 10000, "ops-", "!")) == grown
    lowered = TextContent(8, 20, "ops-").widen(TextContent(7, 20, "ops-"))
    assert lowered == TextContent(4, 20, "ops-")
    weakened = grown.widen(TextContent(3, 9, "op", "?"))
    assert weakened == TextContent(2, None, "op")
    assert grown.fits(weakened)


def test_arbitrary_length_bounds_do_not_require_enumerating_lengths():
    enormous = 10**100
    left = TextContent(enormous, None, "start", "end")
    right = TextContent(enormous + 1, enormous + 2, "start", "end")
    assert right.fits(left)
    assert left.meet(right) == right
    assert left.join(right) == left


def test_content_serialization_round_trips_without_other_type_facts():
    fact = TextContent(4, 100, "ops-", "🙂")
    data = json.loads(json.dumps(fact.to_dict()))
    assert TextContent.from_dict(data) == fact
    assert TextContent.from_dict(TextContent().to_dict()) == TextContent()
    del data["prefix"]
    with pytest.raises(KeyError):
        TextContent.from_dict(data)


def test_lattice_operations_preserve_independent_concrete_string_sets():
    samples = [
        "".join(chars)
        for length in range(5)
        for chars in itertools.product("ab🙂", repeat=length)
    ]
    specifications = [
        (0, None, "", ""),
        (0, 0, "", ""),
        (0, 2, "a", ""),
        (1, 3, "", "b"),
        (2, 4, "ab", "ba"),
        (2, 2, "a", "b"),
        (1, 4, "🙂", ""),
        (0, 4, "", "🙂"),
    ]
    facts = [TextContent(*fields) for fields in specifications]
    # Interpret the supplied constraints, not the canonicalized fields.
    concrete = [
        {
            text
            for text in samples
            if len(text) >= lo
            and (hi is None or len(text) <= hi)
            and text.startswith(prefix)
            and text.endswith(suffix)
        }
        for lo, hi, prefix, suffix in specifications
    ]
    for index, fact in enumerate(facts):
        assert {text for text in samples if fact.matches(text)} == concrete[index]
    for i, left in enumerate(facts):
        assert left.join(left) == left.meet(left) == left
        for j, right in enumerate(facts):
            joined = left.join(right)
            assert joined == right.join(left)
            assert concrete[i] | concrete[j] <= {
                t for t in samples if joined.matches(t)
            }
            assert left.fits(joined) and right.fits(joined)
            met = left.meet(right)
            assert met == right.meet(left)
            actual = set() if met is None else {t for t in samples if met.matches(t)}
            assert actual == concrete[i] & concrete[j]
            if met is not None:
                assert met.fits(left) and met.fits(right)
            if left.fits(right):
                assert concrete[i] <= concrete[j]
            for third in facts:
                assert left.join(right).join(third) == left.join(right.join(third))


def test_bounded_domains_match_exhaustive_concrete_containment_and_emptiness():
    samples = [
        "".join(chars)
        for length in range(5)
        for chars in itertools.product("ab", repeat=length)
    ]
    ends = ["", "a", "b", "aa", "ab", "ba", "bb"]
    interpretations = {}
    for hi in range(5):
        for lo in range(hi + 1):
            for prefix, suffix in itertools.product(ends, repeat=2):
                concrete = {
                    text
                    for text in samples
                    if lo <= len(text) <= hi
                    and text.startswith(prefix)
                    and text.endswith(suffix)
                }
                if not concrete:
                    with pytest.raises(ValueError):
                        TextContent(lo, hi, prefix, suffix)
                    continue
                fact = TextContent(lo, hi, prefix, suffix)
                assert {text for text in samples if fact.matches(text)} == concrete
                assert TextContent.from_dict(fact.to_dict()) == fact
                interpretations[fact] = concrete
    for left, left_values in interpretations.items():
        for right, right_values in interpretations.items():
            assert left.fits(right) == (left_values <= right_values)
