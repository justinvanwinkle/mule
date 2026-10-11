"""The generated Unicode character database matches CPython's str methods."""

import importlib.util
import subprocess
import unicodedata
from pathlib import Path

import pytest

from mule.compiler import build

MODULE = Path(__file__).resolve().parents[3] / "stdlib" / "_unicode_data.py"
CODEPOINTS = range(0x110000)


def case_mapping(mapped):
    return ord(mapped) if len(mapped) == 1 else tuple(map(ord, mapped))


def final_sigma_skips(character):
    """Whether str.lower's Final_Sigma scan skips the character (Case_Ignorable).

    A cased character directly before sigma stops the scan and makes the sigma
    final unless it is skipped; an uncased one between sigma and a following
    cased letter makes the sigma final unless it is skipped.
    """
    if is_cased(character):
        return (character + "Σ").lower()[-1] == "σ"
    return ("AΣ" + character + "A").lower()[1] == "σ"


def is_cased(character):
    return (
        character.islower()
        or character.isupper()
        or unicodedata.category(character) == "Lt"
    )


REFERENCES = {
    "is_alpha": str.isalpha,
    "is_decimal": str.isdecimal,
    "is_digit": str.isdigit,
    "is_numeric": str.isnumeric,
    "is_space": str.isspace,
    "is_printable": str.isprintable,
    "is_lower": str.islower,
    "is_upper": str.isupper,
    "is_cased": is_cased,
    "is_case_ignorable": final_sigma_skips,
    "is_titlecase": lambda character: unicodedata.category(character) == "Lt",
    "is_xid_start": lambda character: character != "_" and character.isidentifier(),
    "is_xid_continue": lambda character: ("a" + character).isidentifier(),
    "is_linebreak": lambda character: len(("a" + character + "b").splitlines()) == 2,
    "decimal_value": lambda character: unicodedata.decimal(character, -1),
    "digit_value": lambda character: unicodedata.digit(character, -1),
    "lower_full": lambda character: case_mapping(character.lower()),
    "upper_full": lambda character: case_mapping(character.upper()),
    "title_full": lambda character: case_mapping(character.title()),
    "fold_full": lambda character: case_mapping(character.casefold()),
}


@pytest.fixture(scope="module")
def unicode_data():
    specification = importlib.util.spec_from_file_location("_unicode_data", MODULE)
    module = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(module)
    return module


def test_unicode_data_records_running_unicode_version():
    assert f"(Unicode {unicodedata.unidata_version})" in MODULE.read_text()


@pytest.mark.parametrize("name", REFERENCES)
def test_unicode_data_function_matches_cpython_for_every_codepoint(unicode_data, name):
    function = getattr(unicode_data, name)
    reference = REFERENCES[name]
    mismatches = [
        (hex(codepoint), actual, expected)
        for codepoint, actual, expected in zip(
            CODEPOINTS,
            map(function, CODEPOINTS),
            map(reference, map(chr, CODEPOINTS)),
            strict=True,
        )
        if actual != expected or type(actual) is not type(expected)
    ]
    assert mismatches[:10] == []


NATIVE_PROGRAM = """\
from _unicode_data import (
    decimal_value,
    digit_value,
    fold_full,
    is_alpha,
    is_case_ignorable,
    is_cased,
    is_decimal,
    is_digit,
    is_linebreak,
    is_lower,
    is_numeric,
    is_printable,
    is_space,
    is_titlecase,
    is_upper,
    is_xid_continue,
    is_xid_start,
    lower_full,
    title_full,
    upper_full,
)

print(upper_full(0xDF), lower_full(0xDF), title_full(0xDF), fold_full(0xDF))
print(lower_full(0x130), upper_full(0x130), fold_full(0x130))
print(title_full(0x1C5), upper_full(0x1C5), lower_full(0x1C5), is_titlecase(0x1C5))
print(upper_full(0x390), lower_full(0x3A3), fold_full(0x41), upper_full(0x61))
print(decimal_value(0x663), digit_value(0x663), is_decimal(0x663), is_alpha(0x663))
print(decimal_value(0xBD), digit_value(0xBD), is_numeric(0xBD), is_digit(0xBD))
print(decimal_value(0xB2), digit_value(0xB2), is_decimal(0xB2), is_digit(0xB2))
print(is_linebreak(0x2028), is_linebreak(0x85), is_linebreak(0xA0), is_space(0xA0))
print(is_printable(0x378), is_printable(0xA0), is_printable(0x20), is_alpha(0x378))
print(is_lower(0x2B0), is_cased(0x2B0), is_case_ignorable(0x2B0), is_upper(0x2160))
print(is_xid_start(0x5F), is_xid_continue(0x5F), is_xid_start(0x2118))
print(is_alpha(0x10FFFF), lower_full(0x10FFFF), upper_full(0x1E943))
"""


def test_unicode_data_compiles_and_runs_natively(tmp_path):
    source = tmp_path / "main.py"
    source.write_text(NATIVE_PROGRAM)
    executable = build(source, tmp_path / "program")
    result = subprocess.run(
        [executable], check=False, capture_output=True, text=True, timeout=10
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "(83, 83) 223 (83, 115) (115, 115)\n"
        "(105, 775) 304 (105, 775)\n"
        "453 452 454 True\n"
        "(921, 776, 769) 963 97 65\n"
        "3 3 True False\n"
        "-1 -1 True False\n"
        "-1 2 False True\n"
        "True True False True\n"
        "False False True False\n"
        "True True True True\n"
        "False True True\n"
        "False 1114111 125217\n"
    )
