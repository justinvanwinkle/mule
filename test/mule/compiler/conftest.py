import pytest

from mule.compiler.driver import Toolchain, builtins_object


@pytest.fixture(scope="session", autouse=True)
def builtins_unit():
    """Build the builtins unit every native test links with once, before the
    tests: concurrent workers wait on its cache entry instead of each racing
    to build it."""
    builtins_object(Toolchain(optimize=False))
