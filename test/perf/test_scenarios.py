"""Independent correctness checks for the initial standalone workloads."""

import copy
import importlib
import subprocess
from pathlib import Path

import pytest

from test.python_host import python_host

SUITE = Path(__file__).resolve().parents[2] / "perf" / "suite"
SCENARIOS = (
    "account_ledger",
    "account_ledger_typed",
    "binary_trees",
    "bytes_codec",
    "closure_pipeline",
    "dataclass_ast",
    "exception_validation",
    "expression_tree",
    "float_nbody",
    "float_nbody_typed",
    "generator_pipeline",
    "graph_paths",
    "http_routing",
    "integer_accumulation",
    "integer_accumulation_typed",
    "interval_merging",
    "json_tokenizer",
    "kv_record_parse",
    "log_aggregation",
    "lru_simulation",
    "matrix_multiply",
    "matrix_multiply_typed",
    "set_operations",
    "word_frequency",
)

TYPED_PAIRS = (
    ("account_ledger", "account_ledger_typed"),
    ("float_nbody", "float_nbody_typed"),
    ("integer_accumulation", "integer_accumulation_typed"),
    ("matrix_multiply", "matrix_multiply_typed"),
)


@pytest.fixture
def load_case(monkeypatch):
    import mule  # noqa: F401

    monkeypatch.syspath_prepend(str(SUITE))
    return importlib.import_module


@pytest.mark.parametrize("name", SCENARIOS)
def test_seeded_inputs_and_repeatable_results(load_case, name):
    case = load_case(name)
    data = case.prepare(31, 42)
    assert data == case.prepare(31, 42)
    assert data != case.prepare(31, 43)
    snapshot = copy.deepcopy(data)
    result = case.run(data)
    assert data == snapshot
    assert case.run(data) == result
    assert data == snapshot


def test_integer_accumulation(load_case):
    kernel = load_case("integer_accumulation").run
    values = [-9, 0, 7, -3, 11]
    assert kernel(values) == (6, 2)
    assert values == [-9, 0, 7, -3, 11]
    assert kernel([]) == (0, 0)


def test_log_aggregation(load_case):
    kernel = load_case("log_aggregation").run
    records = [
        "10.0.0.4 GET /api/items/0 200 10 5",
        "10.0.0.7 POST /api/items/1 500 20 8",
        "10.0.0.4 GET /api/items/0 200 3 2",
    ]
    snapshot = list(records)
    assert kernel(records) == (3, 33, 15, 8, 48, 900, 22)
    assert records == snapshot
    assert kernel([]) == (0, 0, 0, 0, 0, 0, 0)


def test_http_routes_and_aggregation(load_case):
    case = load_case("http_routing")
    requests = [
        "GET /health HTTP/1.1\r\nHost: localhost\r\n\r\n",
        "GET /items?view=compact&id=17 HTTP/1.1\r\n\r\n",
        "POST /events HTTP/1.1\r\ncontent-length: 3\r\n\r\nabc",
        "POST /events HTTP/1.1\r\nContent-Length: 4\r\n\r\nabc",
        "GET /missing/3 HTTP/1.1\r\n\r\n",
        "DELETE /items?id=17 HTTP/1.1\r\n\r\n",
    ]
    expected = [
        (1, 200, "ok\n"),
        (2, 200, '{"id":17}\n'),
        (3, 202, '{"accepted":3}\n'),
        (3, 400, "invalid content length\n"),
        (4, 404, "not found\n"),
        (2, 405, "method not allowed\n"),
    ]
    handle = case.handle
    assert [handle(request) for request in requests] == expected
    kernel = case.run
    snapshot = list(requests)
    assert kernel(requests) == (15, 1811, 80)
    assert requests == snapshot
    assert kernel([]) == (0, 0, 0)


def test_graph_known_paths_and_unreachable_vertices(load_case):
    kernel = load_case("graph_paths").run
    graph = [
        [(1, 5), (2, 20)],
        [(0, 5), (2, 2)],
        [(0, 20), (1, 2), (3, 1)],
        [(2, 1)],
    ]
    snapshot = copy.deepcopy(graph)
    assert kernel(graph) == (63, 4)
    assert graph == snapshot
    assert kernel([[(1, 5)], [(0, 5)], [], []]) == (10, 2)
    assert kernel([[]]) == (0, 1)


def bellman_ford_result(graph):
    """An independent relaxation algorithm; no heap or Dijkstra traversal."""
    distances = [float("inf")] * len(graph)
    distances[0] = 0
    for _ in range(len(graph) - 1):
        previous = list(distances)
        for source, edges in enumerate(graph):
            for target, weight in edges:
                distances[target] = min(distances[target], previous[source] + weight)
        if distances == previous:
            break
    reachable = [
        (index, distance)
        for index, distance in enumerate(distances)
        if distance != float("inf")
    ]
    return sum((index + 1) * distance for index, distance in reachable), len(reachable)


@pytest.mark.parametrize("size,seed", ((1, 0), (2, 42), (7, -7), (25, 999)))
def test_generated_graph_against_independent_algorithm(load_case, size, seed):
    case = load_case("graph_paths")
    graph = case.prepare(size, seed)
    expected = bellman_ford_result(graph)
    assert expected[1] == size
    assert case.run(graph) == expected


def test_standalone_cli_protocol(load_case, tmp_path):
    completed = subprocess.run(
        [
            python_host(),
            str(SUITE / "integer_accumulation.py"),
            "--size",
            "23",
            "--seed",
            "17",
            "--loops",
            "2",
            "--warmups",
            "0",
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=True,
    )
    lines = [line.split("\t", 1) for line in completed.stdout.splitlines()]
    fields = dict(lines)
    assert len(lines) == len(fields) == 7
    assert fields["CASE"] == "integer_accumulation"
    assert fields["SIZE"] == "23"
    assert fields["SEED"] == "17"
    assert fields["LOOPS"] == "2"
    case = load_case("integer_accumulation")
    assert fields["RESULT"] == repr(case.run(case.prepare(23, 17)))
    assert float(fields["PREPARE"]) >= 0
    assert float(fields["KERNEL"]) > 0
    assert completed.stderr == ""


def test_invalid_cli_rejects_unknown_option(tmp_path):
    completed = subprocess.run(
        [
            python_host(),
            str(SUITE / "integer_accumulation.py"),
            "--variant",
            "unrecognized",
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )
    assert completed.returncode != 0
    assert "unknown option: --variant" in completed.stderr
    assert completed.stdout == ""


@pytest.mark.parametrize("name", SCENARIOS)
def test_sbcl_equivalent_matches_python(load_case, name, tmp_path):
    lisp_source = SUITE / f"{name}.lisp"
    assert lisp_source.is_file()
    completed = subprocess.run(
        [
            "sbcl",
            "--script",
            str(lisp_source),
            "--size",
            "31",
            "--seed",
            "42",
            "--loops",
            "2",
            "--warmups",
            "1",
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=True,
    )
    lines = [line.split("\t", 1) for line in completed.stdout.splitlines()]
    fields = dict(lines)
    assert len(lines) == len(fields) == 7
    assert fields["CASE"] == name
    assert fields["SIZE"] == "31"
    assert fields["SEED"] == "42"
    assert fields["LOOPS"] == "2"
    case = load_case(name)
    assert fields["RESULT"] == repr(case.run(case.prepare(31, 42)))
    assert float(fields["PREPARE"]) >= 0
    assert float(fields["KERNEL"]) >= 0
    assert completed.stderr == ""


@pytest.mark.parametrize(("baseline_name", "typed_name"), TYPED_PAIRS)
def test_typed_variation_matches_unannotated_scenario(
    load_case, baseline_name, typed_name, tmp_path
):
    baseline = load_case(baseline_name)
    typed = load_case(typed_name)
    for size, seed in ((7, 11), (31, 42), (64, 99)):
        base_data = baseline.prepare(size, seed)
        typed_data = typed.prepare(size, seed)
        assert typed_data == base_data
        assert typed.run(typed_data) == baseline.run(base_data)

    completed = subprocess.run(
        [
            python_host(),
            str(SUITE / f"{typed_name}.py"),
            "--size",
            "19",
            "--seed",
            "23",
            "--loops",
            "1",
            "--warmups",
            "0",
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=True,
    )
    fields = dict(line.split("\t", 1) for line in completed.stdout.splitlines())
    assert fields["CASE"] == typed_name
    assert fields["RESULT"] == repr(baseline.run(baseline.prepare(19, 23)))


@pytest.mark.parametrize(
    "name",
    (
        "account_ledger_typed",
        "binary_trees",
        "bytes_codec",
        "dataclass_ast",
        "float_nbody",
        "float_nbody_typed",
        "generator_pipeline",
        "integer_accumulation_typed",
        "matrix_multiply_typed",
        "set_operations",
    ),
)
def test_new_and_annotated_scenarios_compile_through_ir1(name):
    from mule.compiler.main import compile_toplevel, ir1_phases
    from mule.compiler.modules import ModuleCompiler

    source_path = SUITE / f"{name}.py"
    compiler = ModuleCompiler(str(source_path))
    compilation = compile_toplevel(
        source_path.read_text(encoding="utf-8"),
        str(source_path),
        imports=compiler,
    )
    ir1_phases(compilation)
    assert compilation.annotations.contradictions == []
    assert compilation.annotations.ignored == set()


def _strip_python_types_and_policy(source: str):
    """Erase type annotations, mule imports, and policy directives from an AST."""
    import ast as host_ast

    tree = host_ast.parse(source)

    class StripTypesAndPolicy(host_ast.NodeTransformer):
        def visit_Module(self, node):
            self.generic_visit(node)
            if (
                node.body
                and isinstance(node.body[0], host_ast.Expr)
                and isinstance(node.body[0].value, host_ast.Constant)
                and isinstance(node.body[0].value.value, str)
            ):
                node.body = node.body[1:]
            return node

        def visit_ImportFrom(self, node):
            if node.module == "mule":
                return None
            return node

        def visit_Assign(self, node):
            self.generic_visit(node)
            if len(node.targets) == 1 and isinstance(node.targets[0], host_ast.Name):
                ident = node.targets[0].id
                if ident in ("__speed__", "__safety__", "__debug_level__"):
                    return None
                if ident in ("NAME", "DESCRIPTION"):
                    node.value = host_ast.Constant(value=ident)
            return node

        def visit_AnnAssign(self, node):
            self.generic_visit(node)
            if node.value is None:
                return None
            return host_ast.Assign(targets=[node.target], value=node.value)

        def visit_FunctionDef(self, node):
            self.generic_visit(node)
            node.returns = None
            node.decorator_list = [
                dec
                for dec in node.decorator_list
                if not (
                    isinstance(dec, host_ast.Call)
                    and isinstance(dec.func, host_ast.Name)
                    and dec.func.id == "optimize"
                )
            ]
            return node

        def visit_ClassDef(self, node):
            self.generic_visit(node)
            node.decorator_list = [
                dec
                for dec in node.decorator_list
                if not (
                    isinstance(dec, host_ast.Call)
                    and isinstance(dec.func, host_ast.Name)
                    and dec.func.id == "optimize"
                )
            ]
            return node

        def visit_arg(self, node):
            node.annotation = None
            return node

    stripped = StripTypesAndPolicy().visit(tree)
    return host_ast.dump(stripped, include_attributes=False)


def _parse_sexprs(source: str):
    """Parse Common Lisp forms into nested lists/atoms, ignoring ;; comments."""
    lines = []
    for line in source.splitlines():
        in_str = False
        escaped = False
        out = []
        for ch in line:
            if escaped:
                out.append(ch)
                escaped = False
                continue
            if ch == "\\":
                out.append(ch)
                escaped = True
                continue
            if ch == '"':
                in_str = not in_str
                out.append(ch)
                continue
            if ch == ";" and not in_str:
                break
            out.append(ch)
        lines.append("".join(out))
    text = "\n".join(lines)
    tokens = []
    i = 0
    n = len(text)
    while i < n:
        ch = text[i]
        if ch.isspace():
            i += 1
        elif ch in ("(", ")", "'"):
            tokens.append(ch)
            i += 1
        elif ch == '"':
            j = i + 1
            while j < n:
                if text[j] == "\\":
                    j += 2
                elif text[j] == '"':
                    j += 1
                    break
                else:
                    j += 1
            tokens.append(text[i:j])
            i = j
        else:
            j = i
            while j < n and not text[j].isspace() and text[j] not in ("(", ")"):
                j += 1
            tokens.append(text[i:j])
            i = j

    pos = 0

    def read_form():
        nonlocal pos
        tok = tokens[pos]
        pos += 1
        if tok == "'":
            return ["quote", read_form()]
        if tok == "(":
            items = []
            while tokens[pos] != ")":
                items.append(read_form())
            pos += 1
            return items
        return tok

    forms = []
    while pos < len(tokens):
        forms.append(read_form())
    return forms


def _strip_lisp_types_and_policy(source: str):
    """Erase Common Lisp declaim/declare/the/:type/of-type from S-expressions."""

    def clean(form):
        if not isinstance(form, list):
            return form
        if not form:
            return form
        head = form[0]
        if head in ("declaim", "declare"):
            return None
        if head == "the" and len(form) == 3:
            return clean(form[2])
        if (
            head == "defparameter"
            and len(form) >= 3
            and form[1] in ("*name*", "*case*", "*description*")
        ):
            return ["defparameter", form[1], form[1]]
        out = []
        i = 0
        while i < len(form):
            item = form[i]
            if item in (":type", "of-type") and i + 1 < len(form):
                i += 2
                continue
            cleaned = clean(item)
            if cleaned is not None:
                out.append(cleaned)
            i += 1
        return out

    forms = []
    for form in _parse_sexprs(source):
        cleaned = clean(form)
        if cleaned is not None:
            forms.append(cleaned)
    return forms


@pytest.mark.parametrize(("baseline_name", "typed_name"), TYPED_PAIRS)
def test_typed_variation_code_is_identical_apart_from_types_and_policy_hints(
    baseline_name, typed_name
):
    base_py = (SUITE / f"{baseline_name}.py").read_text(encoding="utf-8")
    typed_py = (SUITE / f"{typed_name}.py").read_text(encoding="utf-8")
    assert _strip_python_types_and_policy(typed_py) == _strip_python_types_and_policy(
        base_py
    )
    base_lisp = (SUITE / f"{baseline_name}.lisp").read_text(encoding="utf-8")
    typed_lisp = (SUITE / f"{typed_name}.lisp").read_text(encoding="utf-8")
    assert _strip_lisp_types_and_policy(typed_lisp) == _strip_lisp_types_and_policy(
        base_lisp
    )
