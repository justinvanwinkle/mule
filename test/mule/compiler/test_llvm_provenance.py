"""Generated LLVM identifies its source without changing executable code."""

import subprocess

from mule.compiler import emit_llvm
from mule.compiler.driver import Toolchain


def test_llvm_comments_identify_source_entries_blocks_and_operations():
    filename = "origin\nfile.py"
    llvm = emit_llvm(
        "def increment(value):\n return value + 1\nprint(increment(41))\n",
        filename,
    )
    comments = [
        line.strip() for line in llvm.splitlines() if line.lstrip().startswith(";")
    ]
    assert any("origin\\nfile.py" in line and "increment" in line for line in comments)
    assert any("entry: external argument wrapper" in line for line in comments)
    assert any("entry: general body" in line for line in comments)
    assert any("IR2 block:" in line for line in comments)
    assert any("VOP:" in line and "source:" in line for line in comments)
    assert any("operation:" in line for line in comments)
    assert any("helper:" in line and "emitter:" in line for line in comments)

    # Comments must remain single-line even when a source filename contains LF.
    # LLVM must parse both forms to the same module, not merely accept the text.
    stripped = (
        "\n".join(
            line for line in llvm.splitlines() if not line.lstrip().startswith(";")
        )
        + "\n"
    )
    toolchain = Toolchain(optimize=0)
    outputs = []
    for module in (llvm, stripped):
        result = subprocess.run(
            [
                toolchain.clang,
                "-Wno-override-module",
                "-S",
                "-emit-llvm",
                "-x",
                "ir",
                "-",
                "-o",
                "-",
            ],
            input=module,
            capture_output=True,
            text=True,
            check=True,
            timeout=30,
        )
        outputs.append(result.stdout)
    assert outputs[0] == outputs[1]


def test_source_filename_comments_do_not_add_helper_dependencies():
    definitions = []
    for filename in (
        "source.py",
        "source @mule.fast.getitem( @llvm.sadd.with.overflow.i64.py",
    ):
        llvm = emit_llvm("pass\n", filename)
        definitions.append(
            {
                line
                for line in llvm.splitlines()
                if line.startswith(("define ", "declare "))
            }
        )
    assert definitions[0] == definitions[1]
