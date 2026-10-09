from collections.abc import Callable
from importlib import import_module

import pytest

from mule.ast import Node


@pytest.fixture
def parse() -> Callable[..., Node]:
    """Resolve the production entry point without preventing test collection."""
    module = import_module("mule.parser")
    if not hasattr(module, "parse"):
        pytest.fail("mule.parser.parse is not implemented yet", pytrace=False)
    return module.parse
