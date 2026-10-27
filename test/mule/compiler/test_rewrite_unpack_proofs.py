"""Structural tuple proofs protect capture ownership across ordinary unpacking."""

import re

import pytest

from mule.compiler.codegen import compile_component
from mule.compiler.ctype import UNKNOWN
from mule.compiler.ltn import EntryContext
from mule.compiler.main import compile_source
from mule.compiler.vops import VALUE_BUFFER_DATA_OFFSET
from test.mule.compiler.rewrite_support import build_case


def _component(source):
    compilation = compile_source(source)
    function = next(
        fn for fn in compilation.lambdas if fn.source_name == "unpack_record"
    )
    facts = compilation.facts[function]
    context = EntryContext(
        None,
        facts.types,
        facts.targets,
        facts.parameters or (UNKNOWN,) * len(function.vars),
        facts.return_type,
    )
    return compile_component(function, context)


def _parent_pointer_aliases(lines, root):
    """Follow pointer-preserving SSA operations and spilled parent references.

    The object's reference count starts at offset zero. A field load is not a
    parent alias: in particular, tuple-slot results own elements, not the tuple.
    """
    aliases = {root}
    spilled = set()
    changed = True
    while changed:
        changed = False
        for line in lines:
            store = re.search(r"store ptr (%[\w.$-]+), ptr (%[\w.$-]+)", line)
            if store and store[1] in aliases and store[2] not in spilled:
                spilled.add(store[2])
                changed = True
            definition = re.match(r"\s*(%[\w.$-]+) = (.*)", line)
            if not definition or definition[1] in aliases:
                continue
            operation = definition[2]
            operands = set(re.findall(r"%[\w.$-]+", operation))
            preserves = operation.startswith(("phi ptr ", "select ", "freeze ptr "))
            preserves |= operation.startswith(("bitcast ptr ", "addrspacecast ptr "))
            if operation.startswith("getelementptr "):
                offsets = re.findall(r", i\d+ (-?\d+)", operation)
                preserves = bool(offsets) and all(
                    int(offset) == 0 for offset in offsets
                )
            if "call" in operation and re.search(r"@[^\s(]*retain[^\s(]*\(", operation):
                preserves = True
            load = re.match(r"load ptr, ptr (%[\w.$-]+)", operation)
            if (preserves and operands & aliases) or (load and load[1] in spilled):
                aliases.add(definition[1])
                changed = True
    return aliases


def _physical_tuple_reads(body, parent_aliases):
    """Trace an inlined tuple borrow from the parent through its data buffer."""
    addresses = set()
    definitions = list(re.finditer(r"^\s*(%[\w.$-]+) = (.*)", body, re.MULTILINE))
    for definition in definitions:
        field = re.match(
            r"getelementptr (?:[a-z]+ )*i8, ptr (%[\w.$-]+), i64 (-?\d+)",
            definition[2],
        )
        if (
            field
            and field[1] in parent_aliases
            and int(field[2]) == VALUE_BUFFER_DATA_OFFSET
        ):
            addresses.add(definition[1])
    data = set()
    for definition in definitions:
        load = re.match(r"load ptr, ptr (%[\w.$-]+)", definition[2])
        if load and load[1] in addresses:
            data.update(_parent_pointer_aliases(body.splitlines(), definition[1]))
    slots = {pointer: 0 for pointer in data}
    for definition in definitions:
        slot = re.match(
            r"getelementptr (?:[a-z]+ )*(i8|ptr), ptr (%[\w.$-]+), i64 (-?\d+)",
            definition[2],
        )
        if slot and slot[2] in data:
            offset = int(slot[3]) * (8 if slot[1] == "ptr" else 1)
            slots[definition[1]] = offset
    reads = {}
    for definition in definitions:
        load = re.match(r"load ptr, ptr (%[\w.$-]+)", definition[2])
        if load and load[1] in slots:
            reads.setdefault(slots[load[1]], definition.end())
    return reads


def _assert_single_capture_acquisition(
    body, *, arity=2, producer="mule_sequence_tuple_owned"
):
    constructions = list(
        re.finditer(
            rf"^\s*(%[\w.$-]+) = [^\n]*@{re.escape(producer)}\(", body, re.MULTILINE
        )
    )
    assert constructions, "Trace the allocated tuple, not an unrelated reference count"
    inspected = False
    for construction in constructions:
        aliases = _parent_pointer_aliases(body.splitlines(), construction[1])
        reads = _physical_tuple_reads(body[construction.start() :], aliases)
        required_offsets = {8 * index for index in range(arity)}
        if not required_offsets <= reads.keys():
            continue
        inspected = True
        # Inspect the first physical acquisition of every unpacked child. Later
        # indexing and printing can legitimately copy the named tuple's owner.
        last_read = max(reads[offset] for offset in required_offsets)
        region = body[construction.start() : construction.start() + last_read]
        acquisitions = []
        for line in region.splitlines():
            increment = re.search(r"atomicrmw(?: volatile)? add ptr (%[\w.$-]+),", line)
            retain = re.search(
                r"@[^\s(]*retain[^\s(]*\(ptr(?:\s+\w+)*\s+(%[\w.$-]+)\)", line
            )
            if (increment and increment[1] in aliases) or (
                retain and retain[1] in aliases
            ):
                acquisitions.append(line.strip())
        # The named source local owns the tuple. Its unpack capture can acquire
        # one additional owner. Each slot must borrow that capture instead of
        # acquiring a separate owner, whether retain stays a call or is inlined.
        assert len(acquisitions) <= 1, (
            "Per-slot parent copies remain in the full extraction region: "
            + "\n".join(acquisitions)
        )
    assert inspected, "Inspect multiple structural reads from the allocated tuple"


@pytest.mark.parametrize(
    ("producer", "proved"),
    [
        ("record = (value, value)", True),
        ("record = values", False),
        ("record = (value, value) if flag else (value,)", False),
    ],
    ids=["fixed-tuple-local", "unknown-iterable", "different-arity-join"],
)
def test_unpack_shape_proof_borrows_capture_but_owns_extracted_elements(
    producer, proved
):
    # The named source local prevents IR1's direct-display unpack rewrite.
    component = _component(
        f"""
def unpack_record(value, values, flag):
    {producer}
    left, right = record
    return left is right
"""
    )
    operations = [vop for block in component.blocks for vop in block.vops]
    captures = [vop for vop in operations if vop.info.name == "structural-unpack"]
    reads = [vop for vop in operations if vop.info.name == "unpack-ref"]
    assert captures, "The test must exercise structural unpack, not display elision"
    assert captures[0].codegen_info.proved is proved
    assert reads
    for read in reads:
        assert read.codegen_info.proved
        assert read.codegen_info.borrow_parent
        parent = read.args[0].tn
        assert parent.leaf.expression_temporary
        # A direct owning capture TN replaces a retain-producing local copy.
        assert not any(
            op.info.name == "load-local"
            and any(result.tn is parent for result in op.results)
            for op in operations
        )
        assert read.results[0].tn.borrowed_from is None


@pytest.mark.integration
def test_unpack_known_named_tuple_omits_shape_checks_and_parent_retain_per_slot(
    tmp_path,
):
    source = """
def unpack_record(text):
    record = (text, text + '!')
    print('before', len(record))
    left, right = record
    print(left, right, record[0] is left)

unpack_record(input())
"""
    case = build_case(tmp_path, source)
    # Clang inlines the general body into its external wrapper. The runtime
    # input call uses the surviving specialized entry, not that wrapper.
    caller = case.function_ir(r"^mule_module\.fn$")
    assert re.search(r'@"?mule_fn_unpack_record\.spec\.\d+', caller)
    body = case.function_ir(r"^mule_fn_unpack_record\.spec\.\d+$")
    assert "mule_tuple_shape_matches" not in body
    assert "mule_fast_unpack" not in body
    assert "mule.fast.unpack" not in body
    _assert_single_capture_acquisition(body)
    case.compare(stdin="runtime payload\n")


@pytest.mark.integration
def test_unpack_runtime_rows_reads_slots_without_rechecking_each_element(tmp_path):
    source = """
def unpack_record(rows):
    total = 0
    for key, value in rows:
        total += len(key) + len(value)
    return total

text = input()
rows = [(text, text + '!'), (text + '?', text)]
print(unpack_record(rows))
"""
    case = build_case(tmp_path, source)
    body = case.function_ir(r"^mule_fn_unpack_record\.")
    assert "mule_fast_next_unpack" in body
    assert "mule_storage_read" not in body
    # Follow the actual iterator primitive: it must perform physical reads and
    # out-slot writes, rather than hide generic indexing in another helper.
    iterator = case.function_assembly(r"^mule_fast_next_unpack$")
    assert re.search(r"\bmov\w*\s+[^\n]*\([^\n]*\)", iterator)
    assert "mule_storage_read" not in iterator
    assert "mule_tuple_slot" not in iterator
    assert "mule_call" not in iterator
    for text in ("alpha", "", "界🙂"):
        case.compare(stdin=text + "\n")


@pytest.mark.integration
def test_unpack_partition_result_keeps_tuple_owner_and_uses_unchecked_slots(tmp_path):
    source = """
def unpack_record(text):
    record = text.partition(':')
    # Observing the tuple separately prevents direct partition-into lowering.
    print('shape', len(record))
    left, separator, right = record
    print(left, separator, right)
    print(record[0] is left, record[1] is separator, record[2] is right)

unpack_record(input())
"""
    case = build_case(tmp_path, source)
    # input() supplies text, so the guarded call selects this exact-text entry.
    # The ordinary object entry must retain its arbitrary-method fallback.
    caller = case.function_ir(r"^mule_module\.fn$")
    assert re.search(r'@"?mule_fn_unpack_record\.spec\.\d+', caller)
    body = case.function_ir(r"^mule_fn_unpack_record\.spec\.\d+$")
    assert "mule_text_partition" in body
    assert "mule_tuple_shape_matches" not in body
    assert "mule_fast_unpack" not in body
    assert "mule.fast.unpack" not in body
    _assert_single_capture_acquisition(body, arity=3, producer="mule_text_partition")
    for text in ("key:value:tail", "no separator", ":", "界:🙂"):
        case.compare(stdin=text + "\n")


@pytest.mark.integration
def test_unpack_iterator_capture_precedes_target_mutation_and_reports_shape_errors(
    tmp_path,
):
    source = """
class Items:
    def __init__(self, limit, fail):
        self.index = 0
        self.limit = limit
        self.fail = fail
    def __iter__(self):
        print('iter')
        return self
    def __next__(self):
        print('next', self.index)
        if self.index == self.fail:
            raise RuntimeError('iterator failed')
        if self.index == self.limit:
            raise StopIteration
        self.index += 1
        return self.index

def target(container):
    print('target', container)
    return 0

def unpack_record(values):
    container = ['old']
    tail = 'old-tail'
    try:
        container[target(container)], tail = values
    except ValueError:
        print('shape error', container, tail)
    except RuntimeError:
        print('iterator error', container, tail)
    else:
        print('stored', container, tail)

limit = int(input())
unpack_record(Items(limit, -1))
unpack_record(Items(2, 1))
# List input must be snapshotted before the first target mutates its source.
values = [1, 2]
values[1], values[0] = values
print('snapshot', values)
"""
    case = build_case(tmp_path, source)
    # The uncertain source must retain a shape-validation path. The exact tuple
    # arm and the compiled arbitrary-iterator protocol are both valid outcomes.
    body = case.function_ir(r"^mule_fn_unpack_record\.")
    assert re.search(r"mule_fast_unpack|mule\.fast\.unpack|shape_matches", body)
    for limit in (0, 1, 2, 3):
        case.compare(stdin=f"{limit}\n")


@pytest.mark.integration
def test_unpack_failed_stores_release_capture_before_handler_and_preserve_earlier_store(
    tmp_path,
):
    source = """
from _weakref import ref
refs = []
class Token:
    def __init__(self, label):
        self.label = label
        def destroyed(unused):
            print('destroy', label)
        refs.append(ref(self, destroyed))

def produce(label):
    yield Token(label + '-first')
    yield Token(label + '-second')

def unpack_record(label):
    sink = []
    survivor = 'unchanged'
    try:
        sink[0], survivor = produce(label)
    except IndexError:
        print('caught', survivor)
    print('after handler')
    outer = 'old-outer'
    inner = 'old-inner'
    try:
        outer, (inner, missing) = [Token(label + '-outer'), [Token(label + '-inner')]]
    except ValueError:
        print('nested', outer.label, inner)
    print('leaving')
    del outer

unpack_record(input())
print('done')
"""
    case = build_case(tmp_path, source)
    case.compare(stdin="owned\n")
