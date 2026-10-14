"""Parse and route generated HTTP requests in process, without socket overhead.

This handles only the deliberately bounded HTTP request forms prepared below;
it is a performance workload, not a general-purpose HTTP parser or server.
All generated text is ASCII, so response character counts equal byte counts.
"""

from perfutils import run_case

NAME = "http_routing"
DESCRIPTION = "HTTP request parsing, query extraction, and handler dispatch"
DEFAULT_SIZE = 50_000
DEFAULT_SEED = 42


def prepare(size, seed):
    if size < 0:
        raise ValueError("size must be nonnegative")
    requests = []
    state = seed & 0xFFFFFFFF
    for index in range(size):
        state = (state * 1664525 + 1013904223) & 0xFFFFFFFF
        selection = (state >> 16) % 6
        identifier = str(state % 100_000)
        body = ""
        if selection == 0:
            method, target = "GET", "/health"
        elif selection == 1:
            method, target = "GET", "/items?id=" + identifier + "&view=compact"
        elif selection == 2 or selection == 3:
            method, target = "POST", "/events"
            body = "event=" + identifier + "&source=worker" + str(index % 8)
        elif selection == 4:
            method, target = "GET", "/missing/" + identifier
        else:
            method, target = "DELETE", "/items?id=" + identifier
        content_length = len(body)
        if selection == 3:
            content_length += 1
        requests.append(
            method
            + " "
            + target
            + " HTTP/1.1\r\nHost: localhost\r\nX-Request-ID: "
            + identifier
            + "\r\nContent-Length: "
            + str(content_length)
            + "\r\nConnection: keep-alive\r\n\r\n"
            + body
        )
    return requests


def handle(request: str) -> tuple[int, int, str]:
    head: str
    _separator: str
    body: str
    head, _separator, body = request.partition("\r\n\r\n")
    lines: list[str] = head.split("\r\n")
    method: str
    target: str
    _version: str
    method, target, _version = lines[0].split(" ")
    path: str
    query: str
    path, _separator, query = target.partition("?")
    content_length: int = 0
    name: str
    value: str
    for line in lines[1:]:
        name, _separator, value = line.partition(":")
        if name.lower() == "content-length":
            content_length = int(value.strip())
    if method == "GET" and path == "/health":
        return 1, 200, "ok\n"
    if path == "/items":
        if method != "GET":
            return 2, 405, "method not allowed\n"
        identifier: str = ""
        for field in query.split("&"):
            name, _separator, value = field.partition("=")
            if name == "id":
                identifier = value
        return 2, 200, '{"id":' + identifier + "}\n"
    if method == "POST" and path == "/events":
        if content_length != len(body):
            return 3, 400, "invalid content length\n"
        return 3, 202, '{"accepted":' + str(content_length) + "}\n"
    return 4, 404, "not found\n"


def run(data: list[str]) -> tuple[int, int, int]:
    routes: int = 0
    statuses: int = 0
    response_bytes: int = 0
    request: str
    route: int
    status: int
    response: str
    for request in data:
        route, status, response = handle(request)
        routes += route
        statuses += status
        response_bytes += len(response)
    return routes, statuses, response_bytes


def main():
    run_case(NAME, DESCRIPTION, DEFAULT_SIZE, DEFAULT_SEED, prepare, run)


if __name__ == "__main__":
    main()
