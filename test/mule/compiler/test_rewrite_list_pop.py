"""Exact-list pop must remove dispatch without changing stack ownership."""

import re

import pytest

from test.mule.compiler.rewrite_support import build_case

pytestmark = pytest.mark.integration


SOURCE = """
def drain_worklist(depth):
    pending = [depth]
    visited = 0
    checksum = 0
    while pending:
        level = pending.pop()
        visited += 1
        checksum += level
        if level > 0:
            pending.append(level - 1)
            pending.append(level - 1)
    return visited, checksum, len(pending)


def transfer_alias(text):
    marker = [text]
    pending = [marker]
    result = pending.pop()
    marker.append('mutated after pop')
    return result is marker, result[0], result[1], len(pending)


def pop_empty():
    pending = []
    try:
        pending.pop()
    except IndexError:
        return 'empty', len(pending)
    return 'unexpected success', len(pending)


def indexed_pop(seed):
    pending = [seed, seed + 1, seed + 2]
    first = pending.pop(0)
    last = pending.pop(-1)
    return first, last, pending


def keyword_pop(seed):
    pending = [seed, seed + 1]
    try:
        result = pending.pop(index=0)
    except TypeError:
        return 'keyword rejected', pending
    return 'keyword accepted', result, pending


def evaluated_index(index):
    print('evaluated index', index)
    return index


def wrong_arity_pop(seed):
    pending = [seed, seed + 1]
    try:
        pending.pop(evaluated_index(0), evaluated_index(1))
    except TypeError:
        return 'arity rejected', pending
    return 'arity accepted', pending


class Override(list):
    def pop(self, index=-1):
        print('override', index)
        return 700


def overridden_pop(seed):
    pending = Override([seed, seed + 1])
    result = pending.pop()
    return result, pending


class Forwarding(list):
    def pop(self):
        print('before super')
        result = super().pop()
        print('after super')
        return result


def forwarded_pop(seed):
    pending = Forwarding([seed, seed + 1])
    result = pending.pop()
    return result, pending


mode = input()
text = input()
seed = int(text)
if mode == 'keyword':
    print(keyword_pop(seed))
else:
    print(drain_worklist(seed))
    print(transfer_alias(text))
    print(pop_empty())
    print(indexed_pop(seed))
    print(wrong_arity_pop(seed))
    print(overridden_pop(seed))
    print(forwarded_pop(seed))
"""


@pytest.fixture(scope="module")
def pop_case(tmp_path_factory):
    return build_case(tmp_path_factory.mktemp("rewrite-list-pop"), SOURCE)


@pytest.mark.parametrize("text", ["-2", "0", "1", "4", "6"])
def test_list_pop_runtime_worklist_preserves_results_effects_and_errors(pop_case, text):
    # Runtime depth changes the number of iterations and the worklist contents.
    pop_case.compare(stdin="ordinary\n" + text + "\n")


def test_list_pop_keyword_index_preserves_positional_only_error(pop_case):
    # Keep the keyword boundary separate so its failure cannot hide the stack,
    # identity, exception, subclass, and super observations in the other mode.
    pop_case.compare(stdin="keyword\n4\n")


def test_list_pop_exact_receivers_remove_method_dispatch_in_optimized_code(pop_case):
    for function in ("drain_worklist", "transfer_alias", "pop_empty"):
        body = pop_case.function_ir(rf"mule_fn_{function}(?:\.|$)")
        assert "@mule_storage_pop(" in body, body
        assert not re.search(
            r"@mule(?:_resolve_method|_call_resolved|\.call\.resolved)", body
        ), body


def test_list_pop_storage_helper_moves_the_actual_last_slot_without_copying(pop_case):
    assembly = pop_case.function_assembly(r"^mule_storage_pop$")
    # Follow the selected Rust helper. Its successful storage path must load an
    # indexed pointer and shorten the vector, not hide the operation in dispatch.
    first_return = re.search(r"\bretq?\b", assembly)
    assert first_return is not None, assembly
    storage_path = assembly[: first_return.start()]
    assert re.search(r"\bmov\w*\s+[^\n]*\([^\n)]*,[^\n)]*,8\)", storage_path), assembly
    assert re.search(r"\b(?:dec\w*|sub\w*|lea\w*)\b", storage_path), assembly
    assert not re.search(r"\bcallq?\b", storage_path), assembly
    # Error construction may call into the runtime after the storage return.
    # Retaining or releasing the popped element is unnecessary on every path.
    assert not re.search(
        r"\bcallq?\b[^\n]*<(?:[^>]*)(?:retain|release|resolve_method|call_resolved)",
        assembly,
    ), assembly


def test_list_pop_unsupported_call_shapes_do_not_select_zero_arg_storage_pop(pop_case):
    for function in ("indexed_pop", "keyword_pop", "wrong_arity_pop", "overridden_pop"):
        body = pop_case.function_ir(rf"mule_fn_{function}(?:\.|$)")
        assert "@mule_storage_pop(" not in body, body
    # RESOLVE_SUPER_METHOD is deliberately outside the exact-list matcher.
    super_body = pop_case.function_ir(r"mule_fn_Forwarding\.pop(?:\.|$)")
    assert "@mule_storage_pop(" not in super_body, super_body
