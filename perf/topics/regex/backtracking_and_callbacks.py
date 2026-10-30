"""Pathological regex backtracking and regex-to-Python callback boundary workloads.

Exercises backtracking-heavy regex engines (nested quantifiers, ambiguous
alternations, backreferences on near-miss inputs) and re.sub / re.finditer
invoking stateful Python callbacks across native-to-Python call boundaries.
"""

import re
import sys
import time

DEFAULT_SIZE = 300
DEFAULT_LOOPS = 1

# Patterns that force controlled backtracking on near-miss inputs (~12-16 char ambiguous prefixes)
_BACKTRACK_PATTERNS = (
    re.compile(r"^(?:[a-z0-9]{1,3}_?)+=([a-z0-9]{2,6});$"),
    re.compile(r"^([a-z]{2,4})[a-z0-9_]*?\1[a-z0-9_]*?:(?:[0-9]{1,3}|[a-z]{1,3}){2,5}!$"),
    re.compile(r"^(?:[a-zA-Z]{1,4}\.?)+@([a-z0-9-]+\.)+[a-z]{2,6}$"),
)

_TEMPLATE_TOKEN_RE = re.compile(
    r"\{\{\s*(?P<var>[a-zA-Z_][a-zA-Z0-9_.]*)"
    r"(?:\|(?P<filter>[a-z_]+)(?::(?P<arg>[a-zA-Z0-9_-]+))?)?\s*\}\}"
    r"|(?P<link>\[(?P<label>[^\]]+)\]\((?P<url>https?://[^\s)]+)\))"
)


class RenderContext:
    def __init__(self, values: dict[str, object]) -> None:
        self.values = values
        self.substitutions = 0
        self.filter_calls = 0
        self.links_rewritten = 0

    def resolve(self, match: re.Match[str]) -> str:
        self.substitutions += 1
        link = match.group("link")
        if link is not None:
            self.links_rewritten += 1
            label = match.group("label") or ""
            url = match.group("url") or ""
            return f'<a href="{url}" rel="noopener">{label}</a>'

        var_name = match.group("var") or ""
        raw_val = self.values.get(var_name, "UNDEFINED")
        text = str(raw_val)

        flt = match.group("filter")
        if flt is not None:
            self.filter_calls += 1
            arg = match.group("arg")
            if flt == "upper":
                text = text.upper()
            elif flt == "lower":
                text = text.lower()
            elif flt == " default" or flt == "default":
                if raw_val is None or text == "":
                    text = arg or "N/A"
            elif flt == "truncate" and arg is not None and arg.isdigit():
                limit = int(arg)
                if len(text) > limit:
                    text = text[:limit] + "..."
        return text


def prepare(size: int) -> tuple[list[str], list[str], dict[str, object]]:
    backtrack_inputs: list[str] = []
    documents: list[str] = []
    state = 554433221

    for i in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        # Near-miss strings (14-16 char ambiguous prefix) mixed with valid matches
        if (i % 3) == 0:
            s1 = "ab1_cd2_ef3_gh4=xyz99!"
        elif (i % 3) == 1:
            s1 = "tok1_tok2_tok3=val12;"
        else:
            s1 = "pre_a1b2_pre_c3d4:123abc45?"
        backtrack_inputs.append(s1)
        backtrack_inputs.append("svc.mod.pkg.hdl@sub1.example.invalid_9")

        doc = (
            f"Header #{i}: User {{{{ user.name|upper }}}} (id={{{{ user.id|default:0 }}}})\n"
            f"Summary: {{{{ report.summary|truncate:24 }}}} — Status: {{{{ report.status|lower }}}}\n"
            f"Reference: [Documentation Portal](https://docs.example.com/v2/guide/{i}) and "
            f"[Metrics Dashboard](https://metrics.example.com/d/{state & 0xFFFF})\n"
            f"Missing: {{{{ missing.field|default:fallback_val }}}}\n"
        )
        documents.append(doc)

    env: dict[str, object] = {
        "user.name": "Alexandra Vance-Sterling",
        "user.id": (1 << 68) + 4219,
        "report.summary": "Quarterly Compiler Throughput and Latency Analysis Report",
        "report.status": "VERIFIED_OK",
        "missing.field": "",
    }
    return backtrack_inputs, documents, env


def scenario_pathological_backtracking(backtrack_inputs: list[str]) -> tuple[int, int]:
    """Execute backtracking-heavy patterns on near-miss and matching inputs."""
    matched = 0
    failed = 0

    for _ in range(425):
        for s in backtrack_inputs:
            for rx in _BACKTRACK_PATTERNS:
                m = rx.match(s)
                if m is not None:
                    matched += 1 + len(m.group(1) or "")
                else:
                    failed += 1

    return matched, failed


def scenario_sub_with_python_callbacks(
    documents: list[str], env: dict[str, object]
) -> tuple[int, int, int, int]:
    """Run re.sub with a bound Python method callback and re.finditer scanning."""
    ctx = RenderContext(env)
    rendered_chars = 0
    span_sum = 0

    for _ in range(4090):
        for doc in documents:
            rendered = _TEMPLATE_TOKEN_RE.sub(ctx.resolve, doc)
            rendered_chars += len(rendered)
            for m in _TEMPLATE_TOKEN_RE.finditer(doc):
                start, end = m.span()
                span_sum += end - start

    return rendered_chars, ctx.substitutions, ctx.filter_calls, span_sum


def main() -> None:
    size = DEFAULT_SIZE
    loops = DEFAULT_LOOPS
    scenario = "all"
    args = sys.argv[1:]
    i = 0
    while i < len(args):
        if args[i] == "--size" and i + 1 < len(args):
            size = int(args[i + 1])
            i += 2
        elif args[i] == "--loops" and i + 1 < len(args):
            loops = int(args[i + 1])
            i += 2
        elif args[i] == "--scenario" and i + 1 < len(args):
            scenario = args[i + 1]
            i += 2
        else:
            raise ValueError(f"Unknown argument: {args[i]}")

    t0 = time.perf_counter()
    backtrack_inputs, documents, env = prepare(size)
    prep_s = time.perf_counter() - t0

    expected = None
    bt_s = 0.0
    cb_s = 0.0
    for _ in range(loops):
        r1 = ()
        r2 = ()
        if scenario in ("all", "pathological_backtracking"):
            s0 = time.perf_counter()
            r1 = scenario_pathological_backtracking(backtrack_inputs)
            bt_s += time.perf_counter() - s0
        if scenario in ("all", "sub_with_python_callbacks"):
            s1 = time.perf_counter()
            r2 = scenario_sub_with_python_callbacks(documents, env)
            cb_s += time.perf_counter() - s1
        combined = r1 + r2
        if expected is not None and combined != expected:
            raise AssertionError("Result mismatch across loops")
        expected = combined

    total_s = (bt_s + cb_s) / loops
    print("topic\tregex/backtracking_and_callbacks")
    print(f"size\t{size}")
    print(f"loops\t{loops}")
    print(f"result\t{expected}")
    print(f"prepare\t{prep_s:.6f}")
    if scenario in ("all", "pathological_backtracking"):
        print(f"pathological_backtracking\t{bt_s / loops:.6f}")
    if scenario in ("all", "sub_with_python_callbacks"):
        print(f"sub_with_python_callbacks\t{cb_s / loops:.6f}")
    print(f"total\t{total_s:.6f}")


if __name__ == "__main__":
    main()
