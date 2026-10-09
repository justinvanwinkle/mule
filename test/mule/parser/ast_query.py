"""Concise queries and structural assertions for Mule's own syntax tree."""

from dataclasses import fields, is_dataclass
from typing import Any

from mule.ast import Node, walk


def nodes[T: Node](tree: Node, node_type: type[T], **fields: object) -> list[T]:
    return [
        node
        for node in walk(tree)
        if isinstance(node, node_type)
        and all(getattr(node, key) == value for key, value in fields.items())
    ]


def one[T: Node](tree: Node, node_type: type[T], **fields: object) -> T:
    matches = nodes(tree, node_type, **fields)
    assert len(matches) == 1, (
        f"Expected one {node_type.__name__} matching {fields!r}; found {len(matches)}"
    )
    return matches[0]


def at(tree: Node, path: str) -> Any:
    """Follow fields and list indices, e.g. 'statements.0.value.left'."""
    value: Any = tree
    for segment in path.split("."):
        value = value[int(segment)] if segment.isdecimal() else getattr(value, segment)
    return value


def structure(value: object, *, locations: bool = False) -> object:
    """Keep node and literal types distinct when comparing tree structure."""
    if is_dataclass(value) and not isinstance(value, type):
        return {
            "node": type(value).__name__,
            **{
                item.name: structure(getattr(value, item.name), locations=locations)
                for item in fields(value)
                if locations or item.name != "span"
            },
        }
    if isinstance(value, list):
        return [structure(item, locations=locations) for item in value]
    return type(value).__name__, value


def assert_ast_equal(actual: Node, expected: Node, *, locations: bool = False) -> None:
    assert isinstance(actual, Node)
    assert structure(actual, locations=locations) == structure(
        expected, locations=locations
    )
