"""Realistic string allocation, formatting, escaping, joining, and slicing workloads.

Models Django/HTML template rendering with entity escaping, ORM SQL query
generation, and URL/path normalization & query-string assembly.
"""

import sys
import time

DEFAULT_SIZE = 1500
DEFAULT_LOOPS = 1

_USER_NAMES = (
    "Alice O'Connor & Co.",
    "Bob <script>alert(1)</script> Smith",
    "Charlie \"The Optimizer\" Johnson",
    "Dana_Van_Der_Berg",
    "Eve-Marie St. Clair",
    "Frank & Grace Partners <billing@example.com>",
)
_ROLES = ("admin", "editor", "viewer", "billing_manager", "support_engineer")
_DEPARTMENTS = (
    "Platform Engineering",
    "Search & Discovery",
    "Trust & Safety <Global>",
    "Core Runtime / Compiler",
)
_RAW_PATHS = (
    "/var//www/./app/../app/templates//users/profile.html",
    "api/v2///organizations/./42/members/../settings/",
    "/static/./build/2026/10/../10/assets//main.bundle.css",
    "/admin/auth//group/./99/permissions/change/../",
)


def prepare(size: int) -> list[dict[str, object]]:
    rows: list[dict[str, object]] = []
    state = 918273645
    for i in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        name = _USER_NAMES[state % len(_USER_NAMES)]
        role = _ROLES[(state >> 5) % len(_ROLES)]
        dept = _DEPARTMENTS[(state >> 9) % len(_DEPARTMENTS)]
        raw_path = _RAW_PATHS[(state >> 13) % len(_RAW_PATHS)]
        score = (state >> 16) % 10000
        active = (state & 1) == 0
        rows.append(
            {
                "id": i + 1000,
                "name": name,
                "role": role,
                "dept": dept,
                "path": raw_path,
                "score": score,
                "active": active,
            }
        )
    return rows


def _escape_html(text: str) -> str:
    if "&" in text:
        text = text.replace("&", "&amp;")
    if "<" in text:
        text = text.replace("<", "&lt;")
    if ">" in text:
        text = text.replace(">", "&gt;")
    if '"' in text:
        text = text.replace('"', "&quot;")
    if "'" in text:
        text = text.replace("'", "&#x27;")
    return text


def scenario_html_template_render(rows: list[dict[str, object]]) -> tuple[int, int]:
    """Render HTML table chunks with entity escaping, f-strings, and join."""
    total_chars = 0
    amp_entities = 0
    chunk: list[str] = []

    for _ in range(8750):
        for idx, row in enumerate(rows):
            uid = int(row["id"])
            name = _escape_html(str(row["name"]))
            role = str(row["role"]).upper()
            dept = _escape_html(str(row["dept"]))
            score = int(row["score"])
            active = bool(row["active"])

            badge_cls = "badge-active" if active else "badge-muted"
            score_fmt = f"{score // 100}.{str(score % 100).zfill(2)}"
            row_html = (
                f'<tr id="user-{uid}" class="row-{idx & 1}">'
                f'<td class="col-name">{name}</td>'
                f'<td class="col-role"><span class="{badge_cls}">{role}</span></td>'
                f'<td class="col-dept">{dept}</td>'
                f'<td class="col-score">{score_fmt}</td>'
                f"</tr>"
            )
            chunk.append(row_html)

            if len(chunk) == 25:
                table_html = (
                    '<table class="data-grid">\n  <tbody>\n    '
                    + "\n    ".join(chunk)
                    + "\n  </tbody>\n</table>"
                )
                total_chars += len(table_html)
                amp_entities += table_html.count("&")
                chunk.clear()

        if chunk:
            table_html = "<table>" + "".join(chunk) + "</table>"
            total_chars += len(table_html)
            amp_entities += table_html.count("&")
            chunk.clear()

    return total_chars, amp_entities


def scenario_sql_query_builder(rows: list[dict[str, object]]) -> tuple[int, int]:
    """Construct ORM-style SELECT and batched INSERT SQL queries."""
    sql_bytes = 0
    where_clauses = 0

    select_cols = ("id", "username", "role", "department", "score", "is_active")
    quoted_cols = [f'"accounts_user"."{col}"' for col in select_cols]
    projection = ", ".join(quoted_cols)

    batch_values: list[str] = []
    for _ in range(8400):
        for row in rows:
            uid = int(row["id"])
            role = str(row["role"]).replace("'", "''")
            dept = str(row["dept"]).replace("'", "''")
            score = int(row["score"])
            active_sql = "TRUE" if row["active"] else "FALSE"

            conds = [
                f'"accounts_user"."id" >= {uid}',
                f'"accounts_user"."role" = \'{role}\'',
                f'"accounts_user"."score" BETWEEN {score - 50} AND {score + 50}',
                f'"accounts_user"."is_active" = {active_sql}',
            ]
            where_sql = " AND ".join(conds)
            query = (
                "SELECT "
                + projection
                + ' FROM "accounts_user" WHERE ('
                + where_sql
                + ') ORDER BY "accounts_user"."score" DESC LIMIT 20;'
            )
            sql_bytes += len(query)
            where_clauses += query.count(" AND ")

            batch_values.append(f"({uid}, '{role}', '{dept}', {score}, {active_sql})")
            if len(batch_values) == 20:
                insert_sql = (
                    'INSERT INTO "accounts_user_audit" ("user_id", "role", "dept", "score", "active") VALUES '
                    + ", ".join(batch_values)
                    + " ON CONFLICT (\"user_id\") DO UPDATE SET \"score\" = EXCLUDED.\"score\";"
                )
                sql_bytes += len(insert_sql)
                batch_values.clear()
        batch_values.clear()

    return sql_bytes, where_clauses


def scenario_path_and_url_rewriting(rows: list[dict[str, object]]) -> tuple[int, int]:
    """Normalize POSIX paths, build slugs, and format canonical URLs."""
    url_chars = 0
    normalized_depth = 0

    for _ in range(4600):
        for row in rows:
            raw_path = str(row["path"])
            # Normalize path segments (collapse //, ., and ..)
            stack: list[str] = []
            for seg in raw_path.split("/"):
                if not seg or seg == ".":
                    continue
                if seg == "..":
                    if stack:
                        stack.pop()
                else:
                    stack.append(seg)
            clean_path = "/" + "/".join(stack)
            normalized_depth += len(stack)

            # Slugify department name
            dept_raw = str(row["dept"]).lower()
            slug_chars: list[str] = []
            prev_sep = True
            for ch in dept_raw:
                if ch.isalnum():
                    slug_chars.append(ch)
                    prev_sep = False
                elif not prev_sep:
                    slug_chars.append("-")
                    prev_sep = True
            slug = "".join(slug_chars).strip("-")

            uid_padded = str(row["id"]).zfill(6)
            role_tag = str(row["role"]).rjust(16, "_")
            canonical_url = (
                f"https://app.example.com{clean_path}"
                f"?dept={slug}&uid={uid_padded}&role={role_tag.lstrip('_')}"
            )
            url_chars += len(canonical_url)

    return url_chars, normalized_depth


def run_all(rows: list[dict[str, object]]) -> tuple[int, int, int, int, int, int]:
    r1 = scenario_html_template_render(rows)
    r2 = scenario_sql_query_builder(rows)
    r3 = scenario_path_and_url_rewriting(rows)
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
    rows = prepare(size)
    prep_s = time.perf_counter() - t0

    expected = None
    html_s = 0.0
    sql_s = 0.0
    url_s = 0.0
    for _ in range(loops):
        r1 = ()
        r2 = ()
        r3 = ()
        if scenario in ("all", "html_render"):
            s0 = time.perf_counter()
            r1 = scenario_html_template_render(rows)
            html_s += time.perf_counter() - s0
        if scenario in ("all", "sql_builder"):
            s1 = time.perf_counter()
            r2 = scenario_sql_query_builder(rows)
            sql_s += time.perf_counter() - s1
        if scenario in ("all", "url_rewrite"):
            s2 = time.perf_counter()
            r3 = scenario_path_and_url_rewriting(rows)
            url_s += time.perf_counter() - s2
        combined = r1 + r2 + r3
        if expected is not None and combined != expected:
            raise AssertionError("Result mismatch across loops")
        expected = combined

    total_s = (html_s + sql_s + url_s) / loops
    print(f"topic\tstring/allocations")
    print(f"size\t{size}")
    print(f"loops\t{loops}")
    print(f"result\t{expected}")
    print(f"prepare\t{prep_s:.6f}")
    if scenario in ("all", "html_render"):
        print(f"html_render\t{html_s / loops:.6f}")
    if scenario in ("all", "sql_builder"):
        print(f"sql_builder\t{sql_s / loops:.6f}")
    if scenario in ("all", "url_rewrite"):
        print(f"url_rewrite\t{url_s / loops:.6f}")
    print(f"total\t{total_s:.6f}")


if __name__ == "__main__":
    main()
