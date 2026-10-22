"""Constant slice rewrites through normal source compilation and native use."""

import pytest

from mule import ast
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1opt import constant_literal
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case


class _LiteralValue(ast.Visitor):
    def visit_Node(self, node):
        raise AssertionError(f"unexpected folded literal {node!r}")

    def visit_TextLiteral(self, node):
        return node.value

    def visit_BytesLiteral(self, node):
        return node.value

    def visit_IntegerLiteral(self, node):
        return node.value

    def visit_Tuple(self, node):
        return tuple(element.accept(self) for element in node.elements)


_LITERAL_VALUE = _LiteralValue()
_SEQUENCES = ["héllo🚀world", b"\x00header\xfftail", tuple(range(12))]
_SLICES = [
    ("[:]", slice(None)),
    ("[-8:-1:2]", slice(-8, -1, 2)),
    ("[::-1]", slice(None, None, -1)),
    ("[False:True:True]", slice(False, True, True)),
    ("[True:False:-1]", slice(True, False, -1)),
    ("[100:-100:-2]", slice(100, -100, -2)),
    ("[2:2]", slice(2, 2)),
    ("[-100:100]", slice(-100, 100)),
]


def _compile_record_header(sequence, slice_source):
    # The runtime field prevents folding the entire record instead of its header.
    source = (
        f"def record_header(payload):\n return {sequence!r}{slice_source}, payload\n"
    )
    compilation = compile_source(source)
    check_ir1_consistency(compilation.components)
    function = next(
        function
        for function in compilation.lambdas
        if function.source_name == "record_header"
    )
    return function


@pytest.mark.parametrize("sequence", _SEQUENCES, ids=["text", "bytes", "tuple"])
@pytest.mark.parametrize(
    ("slice_source", "selection"),
    _SLICES,
    ids=[
        "default",
        "negative-bounds",
        "reverse",
        "bool-bounds-and-step",
        "bool-bounds-reverse",
        "clamped-reverse",
        "empty",
        "clamped",
    ],
)
def test_constant_slice_normal_compilation_folds_header(
    sequence, slice_source, selection
):
    function = _compile_record_header(sequence, slice_source)
    assert not list(known_calls(function, "%getslice"))
    assert not list(known_calls(function, "%getitem"))
    record = next(known_calls(function, "%build_tuple"))
    header = constant_literal(record.args[0])
    assert header is not None
    assert header.accept(_LITERAL_VALUE) == sequence[selection]


@pytest.mark.parametrize("kind", ["text", "bytes", "tuple"])
@pytest.mark.parametrize("length", [256, 257])
def test_constant_slice_fold_limit_uses_source_length(kind, length):
    # Both lengths select the same small result. The input size controls eligibility.
    if kind == "text":
        sequence = "a" * length
    elif kind == "bytes":
        sequence = b"a" * length
    else:
        sequence = (7,) * length
    function = _compile_record_header(sequence, "[-5::2]")
    slices = list(known_calls(function, "%getslice"))
    if length == 256:
        assert not slices
        record = next(known_calls(function, "%build_tuple"))
        header = constant_literal(record.args[0])
        assert header is not None
        assert header.accept(_LITERAL_VALUE) == sequence[-5::2]
    else:
        assert len(slices) == 1


@pytest.mark.parametrize("sequence", _SEQUENCES, ids=["text", "bytes", "tuple"])
@pytest.mark.parametrize("slice_source", ["[::0]", "[::False]", "[:'bad']"])
def test_invalid_constant_slice_preserves_runtime_error(sequence, slice_source):
    function = _compile_record_header(sequence, slice_source)
    assert len(list(known_calls(function, "%getslice"))) == 1


@pytest.mark.parametrize("sequence", _SEQUENCES, ids=["text", "bytes", "tuple"])
def test_runtime_slice_bound_does_not_match_constant_slice(sequence):
    function = _compile_record_header(sequence, "[payload:]")
    assert len(list(known_calls(function, "%getslice"))) == 1


_RECORD_PROGRAM = r"""
def encode_record(payload):
    label = 'wire:validated:unused'[5:-7]
    return label + ':' + payload

def packet_descriptor(payload):
    magic = b'\x00MULE\xff'[True:-True]
    return magic, payload

def label_columns(payload):
    columns = ('ignored', 'id', 'owner', 'status', 'checksum')[1:-1]
    return columns, payload
"""


def _native_source():
    lines = [_RECORD_PROGRAM, "def report_edges(payload):"]
    for sequence in _SEQUENCES:
        for slice_source, _ in _SLICES:
            lines.append(f"    print({sequence!r}{slice_source}, payload)")
    for length in (256, 257):
        for sequence in ("a" * length, b"a" * length, (7,) * length):
            lines.append(f"    print({sequence!r}[-5::2], payload)")
    lines.append("def report_invalid(payload):")
    for sequence in _SEQUENCES:
        for slice_source in ("[::0]", "[::False]", "[:'bad']"):
            lines.extend(
                [
                    "    try:",
                    f"        print({sequence!r}{slice_source}, payload)",
                    "    except (ValueError, TypeError) as error:",
                    "        print(type(error).__name__, payload)",
                ]
            )
    lines.extend(
        [
            "payload = input()",
            "print(encode_record(payload))",
            "print(packet_descriptor(payload))",
            "print(label_columns(payload))",
            "report_edges(payload)",
            "report_invalid(payload)",
        ]
    )
    return "\n".join(lines) + "\n"


@pytest.mark.integration
@pytest.mark.parametrize("optimize", [1, 3], ids=["O1-no-lto", "O3-no-lto"])
def test_constant_slice_record_paths_remove_slicing_and_match_python(
    tmp_path, optimize
):
    case = build_case(tmp_path, _native_source(), optimize=optimize, lto=False)
    for function in ("encode_record", "packet_descriptor", "label_columns"):
        body = case.function_ir(rf"mule_fn_{function}\.")
        # GETSLICE lowers through mule.fast.getslice to mule_fast_getslice or
        # compiled make_slice/getitem. None is necessary in these record paths.
        assert "getslice" not in body, body
        assert "make_slice" not in body, body
        assert "getitem" not in body, body
    # Both executions consume runtime data. Only the literal headers may fold.
    case.compare("sample-record\n")
    case.compare("héllo🚀\n")
