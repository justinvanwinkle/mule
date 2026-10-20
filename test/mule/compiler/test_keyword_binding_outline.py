"""General argument binding must not impose its stack storage on plain calls."""

import re

from mule.compiler import emit_llvm


def test_plain_entry_keeps_keyword_binding_storage_in_separate_function():
    llvm = emit_llvm("def add(left, right): return left + right\n")
    entries = dict(
        re.findall(
            r'^define[^\n]*@"(mule_fn_add\.[^"]+)"\([^\n]*\)[^\n]*\{\n(.*?)^\}',
            llvm,
            re.MULTILINE | re.DOTALL,
        )
    )
    plain = entries["mule_fn_add.fn"]
    binding = entries["mule_fn_add.fn.binding"]
    assert 'tail call ptr @"mule_fn_add.fn.binding"' in plain
    assert "alloca" not in plain
    assert "call i64 @mule_binding_view" in binding
    assert "alloca" in binding
