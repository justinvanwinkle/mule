"""Regular expression compilation and multi-pattern URL / log routing workloads.

Exercises compiling large multi-branch regex patterns (bypassing re's small cache)
and executing compiled regex tables with named capture groups and typed dispatch.
"""

import re
import sys
import time

DEFAULT_SIZE = 400
DEFAULT_LOOPS = 1

_RESOURCES = (
    "organizations",
    "workspaces",
    "deployments",
    "clusters",
    "pipelines",
    "artifacts",
    "permissions",
    "subscriptions",
    "invoices",
    "webhooks",
)

_ACTIONS = ("create", "update", "delete", "archive", "restore", "inspect", "export")


class RouteTarget:
    def __init__(self, name: str, regex: re.Pattern[str], weight: int) -> None:
        self.name = name
        self.regex = regex
        self.weight = weight

    def invoke(self, groups: dict[str, str | None]) -> int:
        score = self.weight
        org = groups.get("org")
        if org is not None:
            score += len(org) * 3
        item_id = groups.get("id")
        if item_id is not None and item_id.isdigit():
            score += int(item_id) & 0xFFFF
        action = groups.get("action")
        if action is not None:
            score += ord(action[0])
        rev = groups.get("rev")
        if rev is not None:
            score += int(rev, 16) & 0xFFF
        return score


def prepare(size: int) -> tuple[list[str], list[RouteTarget], list[str]]:
    pattern_sources: list[str] = []
    for idx in range(size):
        res1 = _RESOURCES[idx % len(_RESOURCES)]
        res2 = _RESOURCES[(idx + 3) % len(_RESOURCES)]
        act1 = _ACTIONS[idx % len(_ACTIONS)]
        act2 = _ACTIONS[(idx + 2) % len(_ACTIONS)]
        # Construct a complex multi-branch pattern with named groups, character classes, and lookaheads
        pat = (
            rf"^(?:https?://(?P<host_{idx}>[a-z0-9._-]+(?:\.[a-z]{{2,6}})?))?"
            rf"/v(?P<ver_{idx}>[1-9][0-9]*)/"
            rf"(?P<org_{idx}>[a-zA-Z0-9_-]{{3,24}})/"
            rf"(?:{res1}|{res2})/"
            rf"(?P<id_{idx}>[0-9]{{1,8}}|uuid-[0-9a-f]{{8}}-[0-9a-f]{{4}})/"
            rf"(?=(?:{act1}|{act2}|status))"
            rf"(?P<action_{idx}>{act1}|{act2}|status)"
            rf"(?:\?rev=(?P<rev_{idx}>[0-9a-f]{{6,12}})&(?P<query_{idx}>[a-z0-9_=&-]+))?$"
        )
        pattern_sources.append(pat)

    routes: list[RouteTarget] = []
    for idx in range(24):
        res = _RESOURCES[idx % len(_RESOURCES)]
        act_group = "|".join(_ACTIONS)
        route_pat = re.compile(
            rf"^/v(?P<ver>[1-4])/(?P<org>[a-z0-9_-]+)/{res}/"
            rf"(?P<id>[0-9]+)/(?P<action>{act_group})"
            rf"(?:\?rev=(?P<rev>[0-9a-f]+))?$"
        )
        routes.append(RouteTarget(f"route_{res}_{idx}", route_pat, idx + 10))

    candidate_urls: list[str] = []
    state = 987654321
    for i in range(size * 4):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        res = _RESOURCES[(state >> 4) % len(_RESOURCES)]
        act = _ACTIONS[(state >> 8) % len(_ACTIONS)]
        ver = ((state >> 12) % 4) + 1
        uid = (state >> 16) % 99999 + 1
        rev = f"{(state ^ i) & 0xFFFFFF:06x}"
        if (i & 3) == 0:
            url = f"/v{ver}/acme-corp-{i & 31}/{res}/{uid}/{act}?rev={rev}"
        elif (i & 3) == 1:
            url = f"/v{ver}/team_alpha/{res}/{uid}/{act}"
        elif (i & 3) == 2:
            url = f"/v{ver}/org-{i & 15}/{res}/{uid}/unknown_action?rev={rev}"
        else:
            url = f"/v{ver}/global-net/{res}/{uid}/{act}?rev={rev}"
        candidate_urls.append(url)

    return pattern_sources, routes, candidate_urls


def scenario_compile_large_regexes(pattern_sources: list[str]) -> tuple[int, int]:
    """Compile hundreds of complex multi-branch regex patterns from source."""
    compiled_count = 0
    group_total = 0

    probe = "https://api.example.com/v2/acme_org/deployments/48291/update?rev=9a8b7c&env=prod"
    for _ in range(71):
        re.purge()
        for src in pattern_sources:
            rx = re.compile(src, re.IGNORECASE)
            compiled_count += 1
            group_total += rx.groups
            m = rx.match(probe)
            if m is not None:
                group_total += len(m.group(0))

    return compiled_count, group_total


def scenario_url_and_log_routing(
    routes: list[RouteTarget], candidate_urls: list[str]
) -> tuple[int, int, int]:
    """Match incoming paths against compiled route regexes and dispatch handlers."""
    matched_count = 0
    unmatched_count = 0
    score_sum = 0

    for _ in range(4070):
        for url in candidate_urls:
            hit = False
            for route in routes:
                m = route.regex.match(url)
                if m is not None:
                    matched_count += 1
                    score_sum += route.invoke(m.groupdict())
                    hit = True
                    break
            if not hit:
                unmatched_count += 1

    return matched_count, unmatched_count, score_sum


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
    pattern_sources, routes, candidate_urls = prepare(size)
    prep_s = time.perf_counter() - t0

    expected = None
    comp_s = 0.0
    route_s = 0.0
    for _ in range(loops):
        r1 = ()
        r2 = ()
        if scenario in ("all", "compile_large_regexes"):
            s0 = time.perf_counter()
            r1 = scenario_compile_large_regexes(pattern_sources)
            comp_s += time.perf_counter() - s0
        if scenario in ("all", "url_and_log_routing"):
            s1 = time.perf_counter()
            r2 = scenario_url_and_log_routing(routes, candidate_urls)
            route_s += time.perf_counter() - s1
        combined = r1 + r2
        if expected is not None and combined != expected:
            raise AssertionError("Result mismatch across loops")
        expected = combined

    total_s = (comp_s + route_s) / loops
    print("topic\tregex/compilation_and_routing")
    print(f"size\t{size}")
    print(f"loops\t{loops}")
    print(f"result\t{expected}")
    print(f"prepare\t{prep_s:.6f}")
    if scenario in ("all", "compile_large_regexes"):
        print(f"compile_large_regexes\t{comp_s / loops:.6f}")
    if scenario in ("all", "url_and_log_routing"):
        print(f"url_and_log_routing\t{route_s / loops:.6f}")
    print(f"total\t{total_s:.6f}")


if __name__ == "__main__":
    main()
