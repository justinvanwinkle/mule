"""Interface declarations must not become false implementation proofs."""

import logging
import subprocess

import pytest

from mule.compiler import build
from mule.compiler.annotation_types import BUILTINS, EXPORTS
from mule.compiler.ctype import NoneType
from mule.compiler.main import compile_source
from mule.compiler.modules import ModuleCompiler
from test.mule.compiler.test_native import run_native


def compile_declaration(caplog, source):
    logger = logging.getLogger("mule.compiler.main")
    logger.addHandler(caplog.handler)
    try:
        with caplog.at_level(logging.WARNING, logger=logger.name):
            compilation = compile_source(
                source, "interfaces.py", imports=ModuleCompiler("interfaces.py")
            )
    finally:
        logger.removeHandler(caplog.handler)
    function = next(
        function
        for function in compilation.lambdas
        if function.source_name == "declaration"
    )
    return compilation, function


@pytest.mark.parametrize("body", ("...", '"Documentation"\n ...'))
@pytest.mark.parametrize("annotation", ("int", "float", "int64"))
def test_ellipsis_retains_interface_without_constraining_body(caplog, body, annotation):
    compilation, function = compile_declaration(
        caplog, f"from mule import int64\ndef declaration() -> {annotation}:\n {body}\n"
    )
    expected = EXPORTS["int64"] if annotation == "int64" else BUILTINS[annotation]
    assert function.signature.return_annotation is expected
    assert function.signature.implementation_return_annotation is None
    assert compilation.facts[function].return_type == NoneType()
    assert ("return", function) not in compilation.annotations.ignored
    assert not any(
        "the annotation is ignored" in record.message for record in caplog.records
    )


@pytest.mark.parametrize(
    "imports,decorator",
    (
        ("from abc import abstractmethod", "abstractmethod"),
        ("from abc import abstractmethod as abstract", "abstract"),
        ("import abc", "abc.abstractmethod"),
        ("import abc as interfaces", "interfaces.abstractmethod"),
        ("from abc import abstractclassmethod", "abstractclassmethod"),
        ("from abc import abstractstaticmethod", "abstractstaticmethod"),
        ("from abc import abstractproperty", "abstractproperty"),
    ),
)
def test_abstract_decorators_keep_declared_type(caplog, imports, decorator):
    compilation, function = compile_declaration(
        caplog, f"{imports}\n@{decorator}\ndef declaration() -> int:\n pass\n"
    )
    assert function.signature.return_annotation is BUILTINS["int"]
    assert function.signature.implementation_return_annotation is None
    assert compilation.facts[function].return_type == NoneType()
    assert not any(
        "the annotation is ignored" in record.message for record in caplog.records
    )


@pytest.mark.parametrize(
    "prefix,definition",
    (
        ("", "def declaration() -> int:\n pass\n"),
        ("", "def declaration() -> int:\n ...\n return None\n"),
        (
            "def abstractmethod(function):\n return function\n",
            "@abstractmethod\ndef declaration() -> int:\n pass\n",
        ),
        (
            "from abc import abstractmethod\nabstractmethod = lambda function: function\n",
            "@abstractmethod\ndef declaration() -> int:\n pass\n",
        ),
        (
            "from abc import abstractmethod\n",
            "def outer(abstractmethod):\n @abstractmethod\n def declaration() -> int:\n  pass\n return declaration\n",
        ),
        (
            "from abc import abstractmethod\n",
            "class Example:\n def abstractmethod(function):\n  return function\n @abstractmethod\n def declaration() -> int:\n  pass\n",
        ),
    ),
)
def test_concrete_and_shadowed_declarations_still_report_contradictions(
    caplog, prefix, definition
):
    _, function = compile_declaration(caplog, prefix + definition)
    assert not function.signature.interface_declaration
    assert function.signature.return_annotation is None
    assert any(
        "the annotation is ignored" in record.message for record in caplog.records
    )


def test_abstract_body_still_supplies_actual_return_facts(caplog):
    compilation, function = compile_declaration(
        caplog,
        "from abc import abstractmethod\n@abstractmethod\n"
        "def declaration() -> int:\n return 1.25\n",
    )
    assert function.signature.return_annotation is BUILTINS["int"]
    assert not compilation.facts[function].return_type.fits(BUILTINS["int"].fact)
    assert function.signature.implementation_return_annotation is None
    assert not any(
        "the annotation is ignored" in record.message for record in caplog.records
    )


def test_async_placeholder_retains_awaited_interface_without_result_assertion(caplog):
    _, function = compile_declaration(caplog, "async def declaration() -> int:\n ...\n")
    assert function.signature.return_annotation is BUILTINS["int"]
    assert function.signature.implementation_return_annotation is None
    assert not any(
        "the annotation is ignored" in record.message for record in caplog.records
    )


@pytest.mark.parametrize("safety", (0, 3))
def test_native_ellipsis_calls_preserve_none_and_numeric_errors(tmp_path, safety):
    result = run_native(
        tmp_path,
        "from mule import int64, optimize\n"
        f"@optimize(speed=3, safety={safety})\n"
        "def integer() -> int64:\n ...\n"
        f"@optimize(speed=3, safety={safety})\n"
        "def floating() -> float:\n ...\n"
        "print(integer(), floating())\n"
        "print(floating.__annotations__['return'] is float)\n"
        "try:\n print(floating() + 1.0)\nexcept TypeError:\n print('not a float')\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "None None\nTrue\nnot a float\n"
    assert "contradicts annotation" not in result.stderr


@pytest.mark.integration
def test_native_abstract_body_and_override_remain_callable(tmp_path):
    result = run_native(
        tmp_path,
        "from abc import abstractmethod\n"
        "class Base:\n"
        " @abstractmethod\n def declaration(self) -> int:\n  return 'fallback'\n"
        "class Concrete(Base):\n"
        " def declaration(self) -> int:\n  return 42\n"
        "print(Base.declaration(None), Concrete().declaration())\n"
        "print(Base.declaration.__isabstractmethod__)\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "fallback 42\nTrue\n"
    assert "contradicts annotation" not in result.stderr


@pytest.mark.integration
def test_optimized_native_placeholder_does_not_assume_declared_result(tmp_path):
    source = tmp_path / "declaration.py"
    source.write_text(
        "from mule import int64, optimize\n"
        "@optimize(speed=3, safety=0)\n"
        "def declaration() -> int64:\n ...\n"
        "print(declaration())\n"
        "try:\n print(declaration() + 1)\nexcept TypeError:\n print('not an integer')\n"
    )
    executable = build(source, tmp_path / "declaration", optimize=True, workers=1)
    result = subprocess.run(
        [executable], capture_output=True, text=True, check=False, timeout=10
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "None\nnot an integer\n"
