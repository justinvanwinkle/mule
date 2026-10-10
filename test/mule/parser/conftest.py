from collections.abc import Callable

import pytest

from mule.ast import Node
from mule.parser import parse as parse_source


@pytest.fixture
def parse() -> Callable[..., Node]:
    """Expose the production entry point to parser tests."""
    return parse_source
