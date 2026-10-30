"""Set algebra, transitive dependency closures, RBAC intersections, and inverted relation graphs.

Covers set and frozenset operations (|, &, -, ^, <=, >=, update, intersection_update,
difference_update, copy) and bidirectional relation indexing with conjunctive/disjunctive
multi-term queries and weighted result scoring.
"""

import sys
import time

DEFAULT_SIZE = 400
DEFAULT_LOOPS = 1

_PERMISSIONS = (
    "repo:read",
    "repo:write",
    "repo:admin",
    "ci:trigger",
    "ci:approve",
    "artifact:publish",
    "secret:read",
    "secret:rotate",
    "deploy:staging",
    "deploy:prod",
    "audit:export",
    "billing:view",
)
_TAGS = (
    "lang:python",
    "lang:rust",
    "lang:cpp",
    "tier:critical",
    "tier:backend",
    "tier:edge",
    "region:us",
    "region:eu",
    "region:ap",
    "storage:ssd",
    "storage:nvme",
    "runtime:llvm",
    "runtime:jit",
    "runtime:aot",
    "net:grpc",
    "net:http2",
)


DependencyEntry = tuple[int, tuple[int, ...], frozenset[str], frozenset[str]]
DocRelationSpec = tuple[int, list[tuple[str, float]]]
QuerySpec = tuple[tuple[str, str], tuple[str, str], str]


def prepare(
    size: int,
) -> tuple[list[DependencyEntry], list[DocRelationSpec], list[QuerySpec]]:
    dep_entries: list[DependencyEntry] = []
    doc_specs: list[DocRelationSpec] = []
    query_specs: list[QuerySpec] = []

    state = 314159265
    for i in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        # Direct DAG edges to earlier nodes plus bounded forward window
        deps: list[int] = []
        if i > 0:
            deps.append((state >> 3) % i)
            if i > 2 and (state & 2):
                deps.append((state >> 9) % i)
            if i > 5 and (state & 4):
                deps.append((i - 1 - ((state >> 13) & 3)) % i)
        granted: set[str] = set()
        denied: set[str] = set()
        for p_idx in range(5):
            state = (state * 1103515245 + 12345) & 0x7FFFFFFF
            perm = _PERMISSIONS[(state >> 4) % len(_PERMISSIONS)]
            if (p_idx + (state & 1)) & 1 == 0:
                granted.add(perm)
            else:
                denied.add(perm)
        dep_entries.append((i, tuple(deps), frozenset(granted), frozenset(denied)))

        # Document tag weights for bidirectional relation graph
        tag_weights: list[tuple[str, float]] = []
        seen_tags: set[str] = set()
        for _ in range(5):
            state = (state * 1103515245 + 12345) & 0x7FFFFFFF
            tag = _TAGS[(state >> 5) % len(_TAGS)]
            if tag not in seen_tags:
                seen_tags.add(tag)
                weight = round(0.25 + ((state >> 11) % 350) / 100.0, 2)
                tag_weights.append((tag, weight))
        doc_specs.append((i, tag_weights))

        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        must_a = _TAGS[state % len(_TAGS)]
        must_b = _TAGS[(state >> 4) % len(_TAGS)]
        any_a = _TAGS[(state >> 8) % len(_TAGS)]
        any_b = _TAGS[(state >> 12) % len(_TAGS)]
        exclude = _TAGS[(state >> 16) % len(_TAGS)]
        query_specs.append(((must_a, must_b), (any_a, any_b), exclude))

    return dep_entries, doc_specs, query_specs


def scenario_set_algebra_closure(
    dep_entries: list[DependencyEntry],
) -> tuple[int, int, int, int]:
    """Compute transitive closures, RBAC permission intersections, and frontier diffs."""
    closure_edges = 0
    rbac_matches = 0
    symdiff_count = 0
    frontier_size = 0

    required_baseline = frozenset(("repo:read", "ci:trigger"))
    elevated_scope = frozenset(("deploy:prod", "repo:admin", "secret:rotate"))

    for pass_idx in range(3255):
        reach: list[set[int]] = []
        effective_perms: list[frozenset[str]] = []

        for node_id, direct_deps, granted, denied in dep_entries:
            node_reach: set[int] = {node_id}
            active_perms: set[str] = granted.copy() if isinstance(granted, set) else set(granted)
            active_perms.difference_update(denied)

            for dep_id in direct_deps:
                node_reach.update(reach[dep_id])
                dep_perms = effective_perms[dep_id]
                if (node_id + pass_idx) & 1 == 0:
                    active_perms.intersection_update(dep_perms | granted)
                else:
                    active_perms.update(dep_perms - denied)

            closure_edges += len(node_reach)
            reach.append(node_reach)

            frozen_active = frozenset(active_perms)
            effective_perms.append(frozen_active)

            if required_baseline <= frozen_active:
                rbac_matches += 1
            if frozen_active >= elevated_scope:
                rbac_matches += 2

        # Compute pairwise dominance-like frontiers and symmetric differences
        n = len(reach)
        for idx in range(1, n):
            prev_set = reach[idx - 1].copy()
            cur_set = reach[idx]
            common = prev_set & cur_set
            only_either = prev_set ^ cur_set
            symdiff_count += len(only_either)

            frontier = (cur_set - prev_set) | (common - {idx - 1})
            if (idx + pass_idx) % 4 == 0:
                prev_set.difference_update(cur_set)
                frontier_size += len(prev_set) + len(frontier)
            else:
                frontier_size += len(frontier)

    return closure_edges, rbac_matches, symdiff_count, frontier_size


def scenario_inverted_relation_index(
    doc_specs: list[DocRelationSpec],
    query_specs: list[QuerySpec],
) -> tuple[int, int, int, int]:
    """Build a bidirectional relation index and evaluate multi-term scored queries."""
    candidate_total = 0
    matched_docs = 0
    score_cents = 0
    mutation_deltas = 0

    for round_idx in range(543):
        inverted: dict[str, set[int]] = {}
        forward: dict[int, list[tuple[str, float]]] = {}

        for doc_id, tag_weights in doc_specs:
            forward[doc_id] = list(tag_weights)
            for tag, _w in tag_weights:
                bucket = inverted.get(tag)
                if bucket is None:
                    bucket = set()
                    inverted[tag] = bucket
                bucket.add(doc_id)

        # Dynamically re-tag a rotating slice of documents
        num_docs = len(doc_specs)
        for step in range(0, num_docs, 5):
            target_doc = (step + round_idx) % num_docs
            new_tag = _TAGS[(step + round_idx * 3) % len(_TAGS)]
            inv_set = inverted.setdefault(new_tag, set())
            if target_doc not in inv_set:
                inv_set.add(target_doc)
                forward[target_doc].append((new_tag, 1.25))
                mutation_deltas += 1
            elif len(inv_set) > 1:
                inv_set.remove(target_doc)
                mutation_deltas += 1

        # Execute conjunctive + disjunctive + negative set queries
        for (must_a, must_b), (any_a, any_b), exclude in query_specs:
            set_ma = inverted.get(must_a, set())
            set_mb = inverted.get(must_b, set())
            set_aa = inverted.get(any_a, set())
            set_ab = inverted.get(any_b, set())
            set_ex = inverted.get(exclude, set())

            conj = set_ma & set_mb
            disj = set_aa | set_ab
            candidates = (conj | disj) - set_ex
            candidate_total += len(candidates)

            best_score = 0.0
            for doc_id in candidates:
                matched_docs += 1
                doc_score = 0.0
                for tag, weight in forward[doc_id]:
                    if tag == must_a or tag == must_b:
                        doc_score += weight * 2.0
                    elif tag == any_a or tag == any_b:
                        doc_score += weight
                if doc_score > best_score:
                    best_score = doc_score
            score_cents += int(round(best_score * 100.0))

    return candidate_total, matched_docs, score_cents, mutation_deltas


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
    dep_entries, doc_specs, query_specs = prepare(size)
    prep_s = time.perf_counter() - t0

    expected = None
    algebra_s = 0.0
    index_s = 0.0
    for _ in range(loops):
        r1: tuple[int, ...] = ()
        r2: tuple[int, ...] = ()
        if scenario in ("all", "set_algebra_closure"):
            s0 = time.perf_counter()
            r1 = scenario_set_algebra_closure(dep_entries)
            algebra_s += time.perf_counter() - s0
        if scenario in ("all", "inverted_relation_index"):
            s1 = time.perf_counter()
            r2 = scenario_inverted_relation_index(doc_specs, query_specs)
            index_s += time.perf_counter() - s1
        combined = r1 + r2
        if expected is not None and combined != expected:
            raise AssertionError("Result mismatch across loops")
        expected = combined

    total_s = (algebra_s + index_s) / loops
    print("topic\tcollections/sets_and_relations")
    print(f"size\t{size}")
    print(f"loops\t{loops}")
    print(f"result\t{expected}")
    print(f"prepare\t{prep_s:.6f}")
    if scenario in ("all", "set_algebra_closure"):
        print(f"set_algebra_closure\t{algebra_s / loops:.6f}")
    if scenario in ("all", "inverted_relation_index"):
        print(f"inverted_relation_index\t{index_s / loops:.6f}")
    print(f"total\t{total_s:.6f}")


if __name__ == "__main__":
    main()
