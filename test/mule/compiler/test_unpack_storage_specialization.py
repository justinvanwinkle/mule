"""Storage unpacking preserves iteration overrides and prefetches before stores."""

from mule.compiler import emit_llvm
from test.mule.compiler.test_exception_groups import compare


def test_unknown_sequence_unpack_checks_storage_before_iteration():
    llvm = emit_llvm("def unpack(value):\n a, b = value\n return a + b\n")
    # The slot helper checks exact tuple/list shape and prefetches all children
    # before the compiled iterator fallback or any assignment target runs.
    assert "call i32 @mule_fast_unpack" in llvm
    assert 'call ptr @"mule.protocol.unpack"' in llvm


def test_unpack_subclasses_and_wrong_lengths_use_iteration(tmp_path):
    compare(
        tmp_path,
        """
class CustomTuple(tuple):
    def __iter__(self):
        return iter((8, 9))
class CustomList(list):
    def __iter__(self):
        return iter((10, 11))
for value in ((1, 2), [3, 4], CustomTuple((5, 6)), CustomList([5, 6]), [], [1], [1, 2, 3]):
    try:
        a, b = value
        print(a, b)
    except ValueError:
        print('wrong length')
class Traced:
    def __iter__(self):
        for value in (1, 2, 3):
            print('yield', value)
            yield value
try:
    a, b = Traced()
except ValueError:
    print('checked extra value')
""",
    )


def test_list_unpack_prefetches_before_target_setter_mutates_source(tmp_path):
    compare(
        tmp_path,
        """
values = [1, 2]
class Sink:
    def __init__(self):
        self._first = 0
    @property
    def first(self):
        return self._first
    @first.setter
    def first(self, value):
        values.clear()
        self._first = value
sink = Sink()
sink.first, b = values
print(sink.first, b, values)
source = [[1, 2], [3, 4]]
(a, b), (c, d) = source
print(a, b, c, d)
""",
    )


def test_graph_paths_patterns_specialize_list_repeat_sift_and_resolved_unpack():
    llvm = emit_llvm(
        """
def sift(heap, pos):
    end = len(heap)
    item = heap[pos]
    while pos < end:
        heap[pos] = item
        pos += 1

def step(graph: list[list[tuple[int, int]]], mod):
    inf = len(graph) * 97 + 1
    distances = [inf] * len(graph)
    pending = [(0, 0)]
    sift(pending, 0)
    d, u = mod.pop(pending)
    for v, w in graph[u]:
        if d + w < distances[v]:
            distances[v] = d + w
            pending.append((d + w, v))
    return distances[0]

def main(mod):
    return step([[(0, 1)]], mod)
"""
    )
    assert "call ptr @mule_list_getitem" in llvm
    assert 'call ptr @"mule.protocol._list_fill"' in llvm
    assert "call i32 @mule_fast_unpack" in llvm
    assert "call ptr @mule_storage_append_move" in llvm
    assert "call ptr @mule_fast_iterate_move" in llvm
    assert "call ptr @mule.fast.unpack.slots.2" in llvm


def test_nested_list_unpack_avoids_temporary_tuple_snapshot(tmp_path):
    source = """
def pair_deltas(pairs):
    total = 0.0
    for (([x1, y1, z1], v1, m1), ([x2, y2, z2], v2, m2)) in pairs:
        dx = x1 - x2
        dy = y1 - y2
        dz = z1 - z2
        v1[0] -= dx * m2
        v2[0] += dx * m1
        total += dx + dy + dz
    return total

b1 = ([1.0, 2.0, 3.0], [0.0, 0.0, 0.0], 2.0)
b2 = ([0.5, 1.0, 1.5], [0.0, 0.0, 0.0], 4.0)
print(pair_deltas([(b1, b2)]), b1[1][0], b2[1][0])
"""
    llvm = emit_llvm(source)
    assert "@mule_list_shape_matches" in llvm
    assert "@mule_storage_tuple" not in llvm
    compare(tmp_path, source)


def test_unannotated_range_loop_parameter_specializes_fixnum_entry():
    llvm = emit_llvm(
        """
def accumulate(n):
    counts = [0] * n
    total = 0
    for i in range(1, n):
        counts[i] = i
        total += counts[i]
    return total
"""
    )
    assert 'define internal ptr @"mule_fn_accumulate.spec.0"' in llvm
    spec_start = llvm.index('define internal ptr @"mule_fn_accumulate.spec.0"')
    spec_end = llvm.index("\n}", spec_start)
    spec_body = llvm[spec_start:spec_end]
    assert 'call ptr @"mule.protocol._list_fill"' in spec_body
    assert "icmp slt i64" in spec_body
