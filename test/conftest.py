import signal
from contextlib import contextmanager

import pytest

DEFAULT_TEST_TIMEOUT_SECONDS = 60.0
INTEGRATION_TEST_TIMEOUT_SECONDS = 180.0


def pytest_addoption(parser):
    parser.addoption(
        "--corpus-strict",
        action="store_true",
        help="Treat documented corpus compiler gaps as failures rather than xfails.",
    )
    parser.addoption(
        "--conformance-strict",
        action="store_true",
        help="Treat recorded conformance gaps as failures rather than xfails.",
    )
    parser.addoption(
        "--test-timeout",
        type=float,
        default=None,
        help="Per-test timeout in seconds (0 disables; defaults to 60s, or 180s for integration).",
    )


def _timeout_for_item(item: pytest.Item) -> float:
    configured = item.config.getoption("--test-timeout")
    if configured is not None:
        return configured
    if item.get_closest_marker("integration") is not None:
        return INTEGRATION_TEST_TIMEOUT_SECONDS
    return DEFAULT_TEST_TIMEOUT_SECONDS


@contextmanager
def _per_test_alarm(item: pytest.Item, phase: str):
    timeout = _timeout_for_item(item)
    if timeout <= 0 or not hasattr(signal, "SIGALRM"):
        yield
        return

    def _handle_timeout(_signum, _frame):
        pytest.fail(
            f"test timed out after {timeout:g}s during {phase}: {item.nodeid}",
            pytrace=True,
        )

    previous_handler = signal.signal(signal.SIGALRM, _handle_timeout)
    signal.setitimer(signal.ITIMER_REAL, timeout, 1.0)
    try:
        yield
    finally:
        signal.setitimer(signal.ITIMER_REAL, 0.0)
        signal.signal(signal.SIGALRM, previous_handler)


@pytest.hookimpl(hookwrapper=True)
def pytest_runtest_setup(item: pytest.Item):
    with _per_test_alarm(item, "setup"):
        yield


@pytest.hookimpl(hookwrapper=True)
def pytest_runtest_call(item: pytest.Item):
    with _per_test_alarm(item, "call"):
        yield


@pytest.hookimpl(hookwrapper=True)
def pytest_runtest_teardown(item: pytest.Item):
    with _per_test_alarm(item, "teardown"):
        yield
