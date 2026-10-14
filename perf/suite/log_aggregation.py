"""Parse access logs and aggregate routes, statuses, bytes, and latency.

Generated records vary at runtime; every record contributes to the returned
summary. Input generation is separate from the measured parsing and grouping.
"""

from perfutils import run_case

NAME = "log_aggregation"
DESCRIPTION = "Access-log parsing, numeric conversion, and dictionary aggregation"
DEFAULT_SIZE = 100_000
DEFAULT_SEED = 42


def prepare(size, seed):
    """Produce deterministic access logs with varied clients and routes."""
    if size < 0:
        raise ValueError("size must be nonnegative")
    state = seed & 0xFFFFFFFF
    logs = []
    statuses = (200, 201, 304, 404, 500)
    for _ in range(size):
        state = (state * 1664525 + 1013904223) & 0xFFFFFFFF
        route = (state >> 8) % 32
        client = (state >> 16) % 254 + 1
        method = "POST" if state & 4 else "GET"
        status = statuses[(state >> 24) % len(statuses)]
        latency = (state >> 3) % 2000 + 1
        response_bytes = (state >> 10) % 8192
        logs.append(
            f"10.0.0.{client} {method} /api/items/{route} "
            f"{status} {response_bytes} {latency}"
        )
    return logs


def run(data: list[str]):
    routes = {}
    statuses = {}
    count = 0
    byte_total = 0
    latency_total = 0
    latency_max = 0
    client_checksum = 0
    for line in data:
        fields = line.split()
        client = int(fields[0].rsplit(".", 1)[1])
        method_weight = 2 if fields[1] == "POST" else 1
        route = fields[2]
        status = int(fields[3])
        response_bytes = int(fields[4])
        latency = int(fields[5])
        group = routes.get(route)
        if group is None:
            group = [0, 0, 0]
            routes[route] = group
        group[0] += 1
        group[1] += latency
        group[2] = max(group[2], latency)
        statuses[status] = statuses.get(status, 0) + 1
        count += 1
        byte_total += response_bytes
        latency_total += latency
        latency_max = max(latency_max, latency)
        client_checksum += client * method_weight
    route_checksum = 0
    for route, group in routes.items():
        route_number = int(route.rsplit("/", 1)[1]) + 1
        route_checksum += route_number * (group[0] + group[1] + group[2])
    status_checksum = 0
    for status, status_count in statuses.items():
        status_checksum += status * status_count
    return (
        count,
        byte_total,
        latency_total,
        latency_max,
        route_checksum,
        status_checksum,
        client_checksum,
    )


def main():
    run_case(NAME, DESCRIPTION, DEFAULT_SIZE, DEFAULT_SEED, prepare, run)


if __name__ == "__main__":
    main()
