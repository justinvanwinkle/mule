"""Object contracts must decline raw entry selection without crashing codegen."""

import pytest

from mule.compiler import emit_llvm


@pytest.mark.parametrize(
    ("annotation", "value"),
    [
        ("bool", "True"),
        ("None", "None"),
        ("str", "'text'"),
        ("bytes", "b'text'"),
        ("list[int]", "[1]"),
        ("tuple[int, str]", "(1, 'text')"),
        ("dict[str, int]", "{'key': 1}"),
        ("Payload", "Payload()"),
        ("int | str", "'text'"),
    ],
)
@pytest.mark.parametrize("location", ["parameter", "local", "return"])
def test_object_contracts_preserve_descriptor_codegen(annotation, value, location):
    source = "from mule_types import int64, optimize\nclass Payload:\n    pass\n"
    source += "@optimize(speed=3, safety=0)\n"
    if location == "parameter":
        source += f"def probe(value: {annotation}):\n    return value\n"
        source += f"print(probe({value}))\n"
    elif location == "local":
        source += f"def probe(value: int64):\n    local: {annotation} = {value}\n    return local\n"
        source += "print(probe(1))\n"
    else:
        source += f"def probe(value: int64) -> {annotation}:\n    return {value}\n"
        source += "print(probe(1))\n"
    llvm = emit_llvm(source)
    assert "mule_fn_probe" in llvm
