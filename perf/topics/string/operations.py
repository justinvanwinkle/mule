"""Realistic string inspection, searching, splitting, and classification workloads.

Covers HTTP request/header/cookie/query parsing, token & slug validation,
and multi-line log/traceback scanning without micro-loop artificiality.
"""

import sys
import time

DEFAULT_SIZE = 2000
DEFAULT_LOOPS = 1

_HTTP_METHODS = ("GET", "POST", "PUT", "DELETE", "PATCH", "HEAD")
_PATHS = (
    "/api/v2/users/profile/settings",
    "/api/v2/orders/2026/summary/",
    "/static/assets/bundle.min.js",
    "/admin/auth/user/4821/change/",
    "/healthz/readiness",
    "/graphql/v1/query",
    "/accounts/login/",
    "/blog/2026/10/compiler-optimizations-in-practice/",
)
_QUERIES = (
    "page=1&limit=50&sort=-created_at&filter=active&format=json",
    "q=string+operations+benchmark&lang=en_US&debug=false&tab=overview",
    "next=/admin/auth/user/&hl=en&utm_source=dashboard&utm_medium=web",
    "",
    "id=99102&include=items,payments,shipments&currency=USD",
    "search=django__orm__queries&offset=120&limit=25&cache=1",
)
_HEADER_BLOCKS = (
    (
        "Host: app.example.com:8443\r\n"
        "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36\r\n"
        "Accept: application/json, text/plain, */*\r\n"
        "Accept-Encoding: gzip, deflate, br\r\n"
        "Cookie: sessionid=a9f81c72e4b109d3; csrftoken=Zx81Kq90Lm23; theme=dark; flags=v2\r\n"
        "X-Forwarded-For: 10.24.18.9, 172.16.0.4, 198.51.100.42\r\n"
        "Content-Type: application/json; charset=utf-8\r\n"
        "Cache-Control: no-cache, no-store, must-revalidate\r\n"
    ),
    (
        "Host: internal.service.local\r\n"
        "User-Agent: MulClient/2.4.1 (+https://example.org/bot)\r\n"
        "Accept: text/html,application/xhtml+xml,application/xml;q=0.9\r\n"
        "Cookie: sessionid=001928374655ffaa; tz=America/Chicago\r\n"
        "X-Request-Id: req-2026-10-29-8841209a-bc12\r\n"
        "Content-Type: application/x-www-form-urlencoded\r\n"
    ),
    (
        "Host: cdn.example.net\r\n"
        "Accept: */*\r\n"
        "If-None-Match: W/\"653e9a10-4f2b\"\r\n"
        "X-Forwarded-Proto: https\r\n"
        "X-Real-IP: 203.0.113.195\r\n"
    ),
)

_LOG_TEMPLATES = (
    "2026-10-29T18:12:04.118Z [ERROR] django.request: Internal Server Error: /api/v2/orders/ (status=500, duration_ms=142, user_id=4821, trace=tb_991a)",
    "2026-10-29T18:12:05.002Z [WARNING] django.security.csrf: Forbidden (CSRF token missing or incorrect.): /accounts/login/ (status=403, duration_ms=9, user_id=0)",
    "2026-10-29T18:12:05.450Z [INFO] gunicorn.access: GET /static/assets/bundle.min.js (status=200, duration_ms=2, bytes=184920)",
    "2026-10-29T18:12:06.891Z [ERROR] celery.worker: Task billing.charge[id=8812-99a1] raised RetryError('gateway timeout after 30s') in /srv/app/billing/tasks.py:214",
    "2026-10-29T18:12:07.310Z [DEBUG] django.db.backends: (duration_ms=4) SELECT \"users\".\"id\", \"users\".\"email\" FROM \"users\" WHERE \"users\".\"is_active\" = true LIMIT 25;",
    "2026-10-29T18:12:08.015Z [INFO] app.middleware: Request completed path=/graphql/v1/query (status=200, duration_ms=37, user_id=1092, cache=HIT)",
)

_IDENTIFIERS = (
    "user_profile_id__exact",
    "__init_subclass__",
    "HTTP_X_FORWARDED_FOR",
    "2026_invalid_leading_digit",
    "camelCaseMethodName123",
    "slug-with-hyphens-and-42-numbers",
    "   padded_identifier_name   \n",
    "UPPER_CASE_CONSTANT_99",
    "dotted.module.path.ClassName",
    "special$char#token!",
)


def prepare(size: int) -> tuple[list[str], str, list[str]]:
    requests: list[str] = []
    log_lines: list[str] = []
    tokens: list[str] = []
    state = 17489123
    for i in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        method = _HTTP_METHODS[state % len(_HTTP_METHODS)]
        path = _PATHS[(state >> 4) % len(_PATHS)]
        query = _QUERIES[(state >> 8) % len(_QUERIES)]
        headers = _HEADER_BLOCKS[(state >> 12) % len(_HEADER_BLOCKS)]
        if query:
            req_line = method + " " + path + "?" + query + " HTTP/1.1\r\n"
        else:
            req_line = method + " " + path + " HTTP/1.1\r\n"
        requests.append(req_line + headers + "\r\n")

        log_base = _LOG_TEMPLATES[(state >> 16) % len(_LOG_TEMPLATES)]
        log_lines.append(log_base + " seq=" + str(i))

        ident = _IDENTIFIERS[(state >> 20) % len(_IDENTIFIERS)]
        tokens.append(ident)
        tokens.append(path)
        if query:
            tokens.append(query)

    log_blob = "\n".join(log_lines) + "\n"
    return requests, log_blob, tokens


def scenario_http_inspection(requests: list[str]) -> tuple[int, int, int, int]:
    """Parse HTTP request lines, headers, cookies, and query parameters."""
    api_calls = 0
    cookie_matches = 0
    query_param_count = 0
    forwarded_hops = 0

    for _ in range(2150):
        for raw in requests:
            lines = raw.splitlines()
            if not lines:
                continue
            req_line = lines[0]
            method, _sp1, rest = req_line.partition(" ")
            target, _sp2, version = rest.rpartition(" ")
            if not version.startswith("HTTP/1."):
                continue

            path, has_q, query_str = target.partition("?")
            clean_path = path.removeprefix("/").removesuffix("/")
            if path.startswith(("/api/", "/graphql/")):
                api_calls += 1
            elif path.endswith((".js", ".css", ".png")):
                api_calls += 2

            if clean_path:
                segments = clean_path.split("/")
                for seg in segments:
                    if seg.isdigit():
                        api_calls += int(seg) & 0xFF

            if has_q and query_str:
                for pair in query_str.split("&"):
                    k, eq, v = pair.partition("=")
                    if eq:
                        query_param_count += 1
                        if k in ("filter", "search", "q") and "_" in v:
                            query_param_count += v.count("_")
                        elif v.isdigit():
                            query_param_count += int(v) & 0x3F

            for hdr in lines[1:]:
                if not hdr:
                    break
                name, colon, value = hdr.partition(":")
                if not colon:
                    continue
                norm_name = name.strip().lower()
                val = value.strip()
                if norm_name == "cookie":
                    for crumb in val.split(";"):
                        ck, _ceq, cv = crumb.strip().partition("=")
                        if ck == "sessionid" and len(cv) >= 8:
                            cookie_matches += 1
                        elif ck == "theme" and cv in ("dark", "light"):
                            cookie_matches += 2
                elif norm_name == "x-forwarded-for":
                    for ip in val.split(","):
                        ip_clean = ip.strip()
                        if ip_clean.startswith(("10.", "172.", "198.")):
                            forwarded_hops += 1
                elif norm_name == "content-type":
                    if "charset=utf-8" in val.lower():
                        cookie_matches += 1

    return api_calls, cookie_matches, query_param_count, forwarded_hops


def scenario_log_scanning(log_blob: str) -> tuple[int, int, int, int]:
    """Scan a multi-line log stream using find/rfind, slices, and predicates."""
    error_count = 0
    total_duration_ms = 0
    status_sum = 0
    sql_queries = 0

    for _ in range(9600):
        for line in log_blob.splitlines():
            if "[ERROR]" in line:
                error_count += 1
                tb_pos = line.rfind(" in /")
                if tb_pos != -1:
                    colon_pos = line.find(":", tb_pos + 4)
                    if colon_pos != -1:
                        line_no_str = line[colon_pos + 1 :].partition(" ")[0]
                        if line_no_str.isdigit():
                            error_count += int(line_no_str) & 0x1F
            elif "[WARNING]" in line:
                if "CSRF" in line or "Forbidden" in line:
                    error_count += 2

            dur_idx = line.find("duration_ms=")
            if dur_idx != -1:
                start = dur_idx + 12
                end = start
                n = len(line)
                while end < n and line[end].isdigit():
                    end += 1
                if end > start:
                    total_duration_ms += int(line[start:end])

            st_idx = line.find("status=")
            if st_idx != -1:
                code_slice = line[st_idx + 7 : st_idx + 10]
                if len(code_slice) == 3 and code_slice.isdigit():
                    status_sum += int(code_slice)

            if "SELECT " in line and " FROM " in line:
                sql_queries += line.count('"')

    return error_count, total_duration_ms, status_sum, sql_queries


def scenario_token_classification(tokens: list[str]) -> tuple[int, int, int, int]:
    """Classify identifiers, slugs, and paths with character & string predicates."""
    valid_identifiers = 0
    slug_tokens = 0
    vowel_transitions = 0
    dunder_or_private = 0

    for _ in range(2025):
        for raw_tok in tokens:
            tok = raw_tok.strip()
            if not tok:
                continue

            if tok.startswith("__") and tok.endswith("__") and len(tok) > 4:
                dunder_or_private += 2
            elif tok.startswith("_"):
                dunder_or_private += 1

            # Check identifier / slug / path shapes
            first = tok[0]
            if first.isalpha() or first == "_":
                cleaned = tok.replace("_", "")
                if cleaned.isalnum():
                    valid_identifiers += 1
                    if tok.isupper():
                        valid_identifiers += 1
                    elif tok.islower():
                        valid_identifiers += 2

            if "-" in tok and not tok.startswith("-") and not tok.endswith("-"):
                if tok.replace("-", "").isalnum():
                    slug_tokens += 1

            # Character-level lexical scan (vowel/consonant/digit transitions)
            prev_is_vowel = False
            for ch in tok:
                if ch in "aeiouAEIOU":
                    if not prev_is_vowel:
                        vowel_transitions += 1
                    prev_is_vowel = True
                else:
                    prev_is_vowel = False

    return valid_identifiers, slug_tokens, vowel_transitions, dunder_or_private


def run_all(
    data: tuple[list[str], str, list[str]],
) -> tuple[int, int, int, int, int, int, int, int, int, int, int, int]:
    requests, log_blob, tokens = data
    r1 = scenario_http_inspection(requests)
    r2 = scenario_log_scanning(log_blob)
    r3 = scenario_token_classification(tokens)
    return r1 + r2 + r3


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
    data = prepare(size)
    prep_s = time.perf_counter() - t0

    expected = None
    http_s = 0.0
    log_s = 0.0
    tok_s = 0.0
    requests, log_blob, tokens = data
    for _ in range(loops):
        r1 = ()
        r2 = ()
        r3 = ()
        if scenario in ("all", "http_inspect"):
            s0 = time.perf_counter()
            r1 = scenario_http_inspection(requests)
            http_s += time.perf_counter() - s0
        if scenario in ("all", "log_scan"):
            s1 = time.perf_counter()
            r2 = scenario_log_scanning(log_blob)
            log_s += time.perf_counter() - s1
        if scenario in ("all", "token_classify"):
            s2 = time.perf_counter()
            r3 = scenario_token_classification(tokens)
            tok_s += time.perf_counter() - s2
        combined = r1 + r2 + r3
        if expected is not None and combined != expected:
            raise AssertionError("Result mismatch across loops")
        expected = combined

    total_s = (http_s + log_s + tok_s) / loops
    print(f"topic\tstring/operations")
    print(f"size\t{size}")
    print(f"loops\t{loops}")
    print(f"result\t{expected}")
    print(f"prepare\t{prep_s:.6f}")
    if scenario in ("all", "http_inspect"):
        print(f"http_inspect\t{http_s / loops:.6f}")
    if scenario in ("all", "log_scan"):
        print(f"log_scan\t{log_s / loops:.6f}")
    if scenario in ("all", "token_classify"):
        print(f"token_classify\t{tok_s / loops:.6f}")
    print(f"total\t{total_s:.6f}")


if __name__ == "__main__":
    main()
