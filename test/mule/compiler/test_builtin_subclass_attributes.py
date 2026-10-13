"""Builtin subclass attributes share their value's existing owned storage."""

import pytest

from test.mule.compiler.test_modules import execute

CONTAINERS = [("tuple", "(1, 2)"), ("list", "(1, 2)"), ("bytes", "b'ab'")]


def assert_output(tmp_path, source, expected):
    result = execute(tmp_path, {}, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == expected


@pytest.mark.parametrize(("base", "values"), CONTAINERS)
def test_subclass_attributes_and_dictionary_share_live_storage(tmp_path, base, values):
    source = (
        f"class Container({base}):\n"
        " def __init__(self, values):\n"
        "  self.label = 'initial'\n"
        f"value = Container({values})\n"
        "attributes = value.__dict__\n"
        "print(value.label, attributes['label'])\n"
        "attributes['label'] = 'dictionary'\n"
        "print(value.label)\n"
        "value.label = 'attribute'\n"
        "print(attributes['label'])\n"
        "del value.label\n"
        "print('label' in attributes, hasattr(value, 'label'))\n"
        "attributes['other'] = None\n"
        "print(value.other is None)\n"
        "del attributes['other']\n"
        "print('other' in attributes, hasattr(value, 'other'))\n"
    )
    assert_output(
        tmp_path,
        source,
        "initial initial\ndictionary\nattribute\nFalse False\nTrue\nFalse False\n",
    )


@pytest.mark.parametrize(("base", "values"), CONTAINERS)
def test_subclass_attribute_hooks_and_explicit_bypass(tmp_path, base, values):
    source = (
        f"class Container({base}):\n"
        " def __init__(self, values):\n"
        "  object.__setattr__(self, 'events', [])\n"
        " def __getattribute__(self, name):\n"
        "  if name == 'computed':\n"
        "   return 'custom getter'\n"
        "  return object.__getattribute__(self, name)\n"
        " def __getattr__(self, name):\n"
        "  return 'fallback ' + name\n"
        " def __setattr__(self, name, value):\n"
        "  object.__getattribute__(self, 'events').append('set ' + name)\n"
        "  object.__setattr__(self, name, value)\n"
        " def __delattr__(self, name):\n"
        "  object.__getattribute__(self, 'events').append('del ' + name)\n"
        "  object.__delattr__(self, name)\n"
        f"value = Container({values})\n"
        "print(value.computed, value.absent)\n"
        "try:\n"
        " object.__getattribute__(value, 'computed')\n"
        "except AttributeError:\n"
        " print('getter bypass')\n"
        "value.saved = 7\n"
        "value.__dict__['saved'] = 8\n"
        "print(value.saved, value.events)\n"
        "del value.saved\n"
        "print(value.saved, value.events)\n"
        "object.__setattr__(value, 'saved', 9)\n"
        "object.__delattr__(value, 'saved')\n"
        "print(value.events)\n"
    )
    assert_output(
        tmp_path,
        source,
        "custom getter fallback absent\ngetter bypass\n8 ['set saved']\n"
        "fallback saved ['set saved', 'del saved']\n['set saved', 'del saved']\n",
    )


@pytest.mark.parametrize(("base", "values"), CONTAINERS)
def test_subclass_data_descriptors_take_precedence_over_dictionary_entries(
    tmp_path, base, values
):
    source = (
        f"class Container({base}):\n"
        " def __init__(self, values):\n"
        "  self.stored = 10\n"
        " @property\n"
        " def item(self):\n"
        "  return self.stored\n"
        " @item.setter\n"
        " def item(self, value):\n"
        "  self.stored = value * 2\n"
        " @item.deleter\n"
        " def item(self):\n"
        "  self.stored = -1\n"
        f"value = Container({values})\n"
        "value.item = 7\n"
        "print(value.item)\n"
        "value.__dict__['item'] = 99\n"
        "print(value.item, value.__dict__['item'])\n"
        "del value.item\n"
        "print(value.item, value.__dict__['item'])\n"
        "del value.__dict__['item']\n"
        "print(value.item, 'item' in value.__dict__)\n"
        "value.__dict__['stored'] = 25\n"
        "print(value.item)\n"
    )
    assert_output(tmp_path, source, "14\n14 99\n-1 99\n-1 False\n25\n")


@pytest.mark.parametrize("value", ["(1, 2)", "[1, 2]", "b'ab'"])
def test_exact_builtin_values_reject_attributes_and_instance_dictionaries(
    tmp_path, value
):
    source = (
        f"value = {value}\n"
        "try:\n"
        " value.label = 'invalid'\n"
        "except AttributeError:\n"
        " print('attribute rejected')\n"
        "try:\n"
        " value.__dict__\n"
        "except AttributeError:\n"
        " print('no instance dictionary')\n"
    )
    assert_output(tmp_path, source, "attribute rejected\nno instance dictionary\n")


@pytest.mark.parametrize(("base", "values"), CONTAINERS)
def test_empty_slots_subclass_rejects_attributes_but_unslotted_child_allows_them(
    tmp_path, base, values
):
    source = (
        f"class Slotted({base}):\n"
        " __slots__ = ()\n"
        f"value = Slotted({values})\n"
        "try:\n"
        " value.label = 'invalid'\n"
        "except AttributeError:\n"
        " print('attribute rejected')\n"
        "try:\n"
        " value.__dict__\n"
        "except AttributeError:\n"
        " print('no instance dictionary')\n"
        "class Child(Slotted):\n"
        " pass\n"
        f"child = Child({values})\n"
        "child.label = 'allowed'\n"
        "print(child.label, child.__dict__['label'])\n"
    )
    assert_output(
        tmp_path,
        source,
        "attribute rejected\nno instance dictionary\nallowed allowed\n",
    )


def test_bytes_subclass_construction_preserves_source_identity_and_attributes(tmp_path):
    source = (
        "class First(bytes):\n"
        " pass\n"
        "class Second(bytes):\n"
        " pass\n"
        "source = b'abc'\n"
        "first = First(source)\n"
        "first.label = ['first']\n"
        "second = Second(first)\n"
        "print(type(source) is bytes, type(first) is First, type(second) is Second)\n"
        "print(first is not source, second is not first, first == source, second == source)\n"
        "try:\n"
        " second.label\n"
        "except AttributeError:\n"
        " print('source attributes not copied')\n"
        "second.label = ['second']\n"
        "first.label.append('changed')\n"
        "print(first.label, second.label)\n"
    )
    assert_output(
        tmp_path,
        source,
        "True True True\nTrue True True True\nsource attributes not copied\n"
        "['first', 'changed'] ['second']\n",
    )


@pytest.mark.parametrize(("base", "values"), CONTAINERS)
def test_subclass_attribute_references_are_released_on_delete_and_destruction(
    tmp_path, base, values
):
    source = (
        "import weakref\n"
        "class Marker:\n"
        " pass\n"
        f"class Container({base}):\n"
        " pass\n"
        f"value = Container({values})\n"
        "value.held = Marker()\n"
        "deleted = weakref.ref(value.held)\n"
        "del value.__dict__['held']\n"
        "print(deleted() is None)\n"
        "value.held = Marker()\n"
        "destroyed = weakref.ref(value.held)\n"
        "del value\n"
        "print(destroyed() is None)\n"
    )
    assert_output(tmp_path, source, "True\nTrue\n")
