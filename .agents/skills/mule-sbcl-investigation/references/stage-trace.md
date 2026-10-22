# Worked trace: single-use slice comparison

Use the current `test/mule/compiler/test_slice_compare_fusion.py` as the
source of truth for this example. Recheck APIs and symbol mapping when they
change. This is a diagnostic trace, not proof of a new performance improvement.

The existing source has ordinary calls and no manually injected analysis facts:

```python
SOURCE = '''\
def scan_word(text: str, i: int, end: int) -> int:
    word = text[i + 1 : end]
    i = end + 1
    if word == "tag":
        return i
    return 0

scan_word('"tag"', 0, 4)
scan_word('"other"', 1, 5)
'''
```

## IR1 facts, entry context, VOP, and storage

Run these statements with the source above in the worktree's `uv run python`
interpreter. This path performs compiler analysis and lowering, not a native
executable build or a timing comparison:

```python
from mule.compiler.codegen import compile_component
from mule.compiler.ir1util import known_calls
from mule.compiler.ltn import EntryContext
from mule.compiler.main import compile_source

compilation = compile_source(SOURCE, "slice-trace.py")
function = next(
    item for item in compilation.lambdas
    if item.source_name == "scan_word"
)
facts = compilation.facts[function]
print("general:", facts.parameters)
for variant in facts.variants:
    print(variant.id, variant.parameters, variant.return_type)

observed = next(item for item in facts.variants if item.id == "observed")
for operation in ("%getslice", "%compare_equal"):
    call = next(known_calls(function, operation))
    print(operation, observed.types.get(call))

context = EntryContext(
    observed.id,
    observed.types,
    observed.targets,
    observed.parameters,
    observed.return_type,
)
component = compile_component(function, context)
for block in component.blocks:
    for vop in block.vops:
        if vop.info.name == "text-slice-compare":
            print(vop.info.name, vop.info.generator.__name__)
            for argument in vop.args:
                tn = argument.tn
                print("argument:", tn.type, tn.primitive_type.name, tn.sc.name)
            for result in vop.results:
                tn = result.tn
                print("result:", tn.type, tn.primitive_type.name, tn.sc.name)
```

`compile_source` runs IR1 analysis and publishes `Compilation.facts`.
`LambdaFacts.parameters` describes the general entry, not every specialized body.
Select a real variant from `facts.variants` and inspect its `types` and `targets`.
The `EntryContext` above copies compiler-derived facts, following the existing
diagnostic pattern in `test/mule/compiler/test_rewrite_flow_activation.py`.
It does not establish that every caller selects that context.

`compile_component` performs LTN, IR2, representation selection, and coercions.
`VOP.info` identifies its template/generator. Each TN exposes its type,
primitive type, and storage class.

The original analysis-only probe observed unknown general parameters and an
observed text/integer variant. Slice and equality results had text and boolean
facts. IR2 contained `text-slice-compare` with descriptor/immediate TNs.
The decisive fusion currently occurs in `ir2tran.py`, not an IR1 transform.
An IR1-only inspector would therefore miss the actual rewrite.

Specialized emission can clone and replace IR1 nodes, then restore the canonical
graph. Do not interpret a failed identity lookup in old `facts.types` as missing
inference. Inspect TN facts at that boundary. If original-to-clone correspondence
is needed, add a scoped diagnostic at the clone/rewrite boundary.
Do not invent an introspection API or manually restore compiler-owned state.

## Normal module emission and its limits

The existing single-use regression calls `mule.compiler.emit_llvm(SOURCE)`.
It locates `mule_fn_scan_word.spec.0` and checks for one `mule_text_slice_eq`
call without generic getslice or heap retain in that body.
The multi-use regression keeps the slice and rejects that fusion.
Read the actual emission mapping and callers before reusing the symbol.

Run the focused existing regression when checking this emitted pattern:

```sh
uv run pytest test/mule/compiler/test_slice_compare_fusion.py \
  -k single_use_local_slice_equality
```

The historical probe ran analysis/IR2 only; it read, but did not execute, the
emission and native assertions. The source-level example contains literal call
inputs, so it is not a runtime-input performance regression by itself.

For a new optimization claim, add the real runtime-input case, inspect optimized
per-unit LLVM and linked helpers, check Python behavior, and measure the original
workload. Use `rewrite_support.py` for actual graph units and linked assembly.
Check the selected `.spec`, numeric, or unpacked entry, not merely its wrapper.
Neither the helper call's presence nor its absence alone proves the whole path
became cheaper: inspect the helper and remaining dynamic work.
