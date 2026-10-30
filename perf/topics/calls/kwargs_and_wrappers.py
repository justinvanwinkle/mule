"""Complex function signatures, keyword forwarding wrappers, and lexical closure combinators.

Covers 8-10 parameter functions (positional, defaults, `*args`, keyword-only
flags) called directly with permuted keywords and through `*args, **kwargs`
decorator wrappers, plus nested closure combinators mutating `nonlocal` state.
"""

import sys
import time

DEFAULT_SIZE = 200
DEFAULT_LOOPS = 1


def _resolve_route_entry(
    endpoint: str,
    method: str,
    version: int = 2,
    prefix: str = "/api",
    timeout_ms: int | None = None,
    cache_ttl: int = 60,
    *middlewares: str,
    strict: bool = False,
    allow_alias: bool = True,
    context: dict[str, int] | None = None,
    retry_limit: int = 3,
) -> int:
    score = len(endpoint) * 3 + len(method) * 5 + version * 11 + len(prefix)
    if timeout_ms is not None:
        score += timeout_ms >> 2
    else:
        score += 25
    score += cache_ttl
    for idx, mw in enumerate(middlewares):
        score += (idx + 1) * len(mw)
    if strict:
        score = score * 2 + 7
    if allow_alias:
        score += 13
    if context is not None:
        score += context.get("tenant", 0) * 3 + context.get("priority", 0) + context.get("injected", 0)
    score += retry_limit * 9
    return score


def _with_audit_and_defaults(fn):
    def wrapper(*args, **kwargs):
        call_tag = len(args) + len(kwargs)
        if len(args) < 5 and ("timeout_ms" not in kwargs or kwargs["timeout_ms"] is None):
            kwargs["timeout_ms"] = 120 + (call_tag & 0x1F)
        if kwargs.get("strict", False):
            kwargs["retry_limit"] = kwargs.get("retry_limit", 3) + 2
        ctx = kwargs.get("context")
        if ctx is not None:
            new_ctx = dict(ctx)
            new_ctx["injected"] = (call_tag & 7) + len(args)
            kwargs["context"] = new_ctx
        return fn(*args, **kwargs) + (call_tag & 0xF)

    return wrapper


def _with_normalization_layer(fn):
    def wrapper(*args, **kwargs):
        if kwargs.get("allow_alias", True) and "prefix" in kwargs:
            kwargs["prefix"] = kwargs["prefix"].rstrip("/")
        if 2 <= len(args) < 6 and args[1] == "HEAD":
            kwargs["cache_ttl"] = kwargs.get("cache_ttl", 60) + 30
        return fn(*args, **kwargs)

    return wrapper


_WRAPPED_RESOLVE = _with_normalization_layer(_with_audit_and_defaults(_resolve_route_entry))

_ENDPOINTS = ("/users/profile", "/orders/summary", "/Search/Query", "/billing/invoices", "/auth/token")
_METHODS = ("GET", "POST", "PUT", "DELETE", "HEAD")
_PREFIXES = ("/api/", "/v2/internal/", "/public", "/edge/gateway/")


def prepare(
    size: int,
) -> tuple[
    list[tuple[str, str, int, str, int | None, int, dict[str, int]]],
    list[list[str]],
]:
    call_specs: list[tuple[str, str, int, str, int | None, int, dict[str, int]]] = []
    token_streams: list[list[str]] = []
    state = 543210987

    vocab = ("let", "ident", "=", "42", "+", "17", ";", "call", "(", "x", ",", "9", ")")
    for i in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        ep = _ENDPOINTS[state % len(_ENDPOINTS)]
        method = _METHODS[(state >> 4) % len(_METHODS)]
        ver = 1 + ((state >> 7) % 4)
        pref = _PREFIXES[(state >> 10) % len(_PREFIXES)]
        timeout = None if (state & 3) == 0 else (40 + ((state >> 12) % 400))
        ttl = 15 * (1 + ((state >> 16) % 8))
        ctx = {"tenant": (state >> 19) % 12, "priority": (state >> 23) % 5}
        call_specs.append((ep, method, ver, pref, timeout, ttl, ctx))

        stream_len = 14 + (state % 7)
        toks: list[str] = []
        for _ in range(stream_len):
            state = (state * 1103515245 + 12345) & 0x7FFFFFFF
            toks.append(vocab[(state >> 5) % len(vocab)])
        token_streams.append(toks)

    return call_specs, token_streams


def scenario_kwargs_and_default_dispatch(
    call_specs: list[tuple[str, str, int, str, int | None, int, dict[str, int]]],
) -> tuple[int, int, int, int]:
    """Call multi-parameter functions directly with varied kwargs and via decorator wrappers."""
    direct_default_sum = 0
    direct_kw_sum = 0
    wrapped_sum = 0
    dynamic_kw_sum = 0

    for round_idx in range(17800):
        flag_flip = (round_idx & 1) == 0
        for idx, (ep, method, ver, pref, timeout, ttl, ctx) in enumerate(call_specs):
            # 1. Direct call relying on positional defaults + keyword-only flags
            direct_default_sum += _resolve_route_entry(
                ep,
                method,
                strict=flag_flip,
                context=ctx,
            )

            # 2. Direct call with permuted keyword order and *args
            if (idx & 1) == 0:
                direct_kw_sum += _resolve_route_entry(
                    ep,
                    method,
                    ver,
                    pref,
                    timeout,
                    ttl,
                    "auth",
                    "rate_limit",
                    retry_limit=4,
                    allow_alias=not flag_flip,
                    strict=flag_flip,
                    context=ctx,
                )
            else:
                direct_kw_sum += _resolve_route_entry(
                    context=ctx,
                    strict=not flag_flip,
                    retry_limit=2,
                    cache_ttl=ttl,
                    timeout_ms=timeout,
                    prefix=pref,
                    version=ver,
                    method=method,
                    endpoint=ep,
                )

            # 3. Call through nested *args, **kwargs decorator wrappers
            wrapped_sum += _WRAPPED_RESOLVE(
                ep,
                method,
                ver,
                pref,
                timeout,
                ttl,
                "trace",
                strict=(idx & 3) == 0,
                allow_alias=flag_flip,
                context=ctx,
            )

            # 4. Dynamic **kwargs dictionary assembly and forwarding
            kw_bundle = {
                "version": ver + 1,
                "prefix": pref,
                "timeout_ms": timeout,
                "strict": flag_flip,
                "allow_alias": True,
                "retry_limit": 1 + (idx & 3),
                "context": ctx,
            }
            dynamic_kw_sum += _WRAPPED_RESOLVE(ep, method, **kw_bundle)

    return direct_default_sum, direct_kw_sum, wrapped_sum, dynamic_kw_sum


def _build_combinator_suite():
    steps_taken = 0
    memo_hits = 0
    error_log: list[int] = []
    memo: dict[tuple[str, int], tuple[int, int]] = {}

    def match_pred(label: str, pred, weight: int):
        def parser(tokens: list[str], pos: int) -> tuple[int, int]:
            nonlocal steps_taken, memo_hits
            steps_taken += 1
            key = (label, pos)
            cached = memo.get(key)
            if cached is not None:
                memo_hits += 1
                return cached
            if pos < len(tokens) and pred(tokens[pos]):
                res = (weight + len(tokens[pos]), pos + 1)
            else:
                error_log.append(pos)
                if len(error_log) > 32:
                    del error_log[:16]
                res = (-1, pos)
            memo[key] = res
            return res

        return parser

    def seq_combine(p1, p2, bonus: int):
        def parser(tokens: list[str], pos: int) -> tuple[int, int]:
            nonlocal steps_taken
            steps_taken += 1
            v1, pos1 = p1(tokens, pos)
            if v1 < 0:
                return (-1, pos)
            v2, pos2 = p2(tokens, pos1)
            if v2 < 0:
                return (-1, pos)
            return (v1 + v2 + bonus, pos2)

        return parser

    def alt_combine(p1, p2):
        def parser(tokens: list[str], pos: int) -> tuple[int, int]:
            nonlocal steps_taken
            steps_taken += 1
            v1, pos1 = p1(tokens, pos)
            if v1 >= 0:
                return (v1 + 1, pos1)
            return p2(tokens, pos)

        return parser

    def repeat_many(p, max_count: int):
        def parser(tokens: list[str], pos: int) -> tuple[int, int]:
            nonlocal steps_taken
            steps_taken += 1
            total = 0
            cur = pos
            for _ in range(max_count):
                v, nxt = p(tokens, cur)
                if v < 0 or nxt == cur:
                    break
                total += v
                cur = nxt
            return (total, cur)

        return parser

    def reset_memo() -> tuple[int, int, int]:
        memo.clear()
        err_sum = sum(error_log)
        error_log.clear()
        return steps_taken, memo_hits, err_sum

    p_kw = match_pred("kw", lambda tok: tok in ("let", "call"), 5)
    p_ident = match_pred("id", lambda tok: tok.isalpha(), 3)
    p_num = match_pred("num", lambda tok: tok.isdigit(), 7)
    p_sym = match_pred("sym", lambda tok: tok in ("=", "+", ";", "(", ")", ","), 2)

    p_atom = alt_combine(p_num, alt_combine(p_ident, p_sym))
    p_pair = seq_combine(p_atom, p_atom, 4)
    p_clause = alt_combine(seq_combine(p_kw, p_pair, 9), p_pair)
    p_stream = repeat_many(alt_combine(p_clause, p_atom), 12)

    return p_stream, reset_memo


def scenario_closure_combinators(token_streams: list[list[str]]) -> tuple[int, int, int, int]:
    """Run nested lexical closure combinators mutating `nonlocal` counters and memo tables."""
    parsed_value_sum = 0
    total_steps = 0
    total_memo_hits = 0
    total_err_score = 0

    p_stream, reset_memo = _build_combinator_suite()

    for _ in range(1475):
        for stream in token_streams:
            val1, end_pos = p_stream(stream, 0)
            # Re-parse from offset 0 and offset 1 to exercise closure memo table hits
            val2, _ = p_stream(stream, 0)
            val3, _ = p_stream(stream, min(1, len(stream)))
            parsed_value_sum += val1 + val2 + val3 + end_pos
            steps, hits, err_sum = reset_memo()
            total_steps = steps
            total_memo_hits = hits
            total_err_score += err_sum

    return parsed_value_sum, total_steps, total_memo_hits, total_err_score


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
    call_specs, token_streams = prepare(size)
    prep_s = time.perf_counter() - t0

    expected = None
    kw_s = 0.0
    comb_s = 0.0
    for _ in range(loops):
        r1: tuple[int, ...] = ()
        r2: tuple[int, ...] = ()
        if scenario in ("all", "kwargs_and_default_dispatch"):
            s0 = time.perf_counter()
            r1 = scenario_kwargs_and_default_dispatch(call_specs)
            kw_s += time.perf_counter() - s0
        if scenario in ("all", "closure_combinators"):
            s1 = time.perf_counter()
            r2 = scenario_closure_combinators(token_streams)
            comb_s += time.perf_counter() - s1
        combined = r1 + r2
        if expected is not None and combined != expected:
            raise AssertionError("Result mismatch across loops")
        expected = combined

    total_s = (kw_s + comb_s) / loops
    print("topic\tcalls/kwargs_and_wrappers")
    print(f"size\t{size}")
    print(f"loops\t{loops}")
    print(f"result\t{expected}")
    print(f"prepare\t{prep_s:.6f}")
    if scenario in ("all", "kwargs_and_default_dispatch"):
        print(f"kwargs_and_default_dispatch\t{kw_s / loops:.6f}")
    if scenario in ("all", "closure_combinators"):
        print(f"closure_combinators\t{comb_s / loops:.6f}")
    print(f"total\t{total_s:.6f}")


if __name__ == "__main__":
    main()
