"""Physical lookup is callback-free, but destroying another receiver is not."""

from mule.compiler.codegen import ModuleGenerator, Program, compile_component
from mule.compiler.protocols import compile_protocol


def test_bounded_scalar_field_does_not_retain_its_immediate_value():
    compilation = compile_protocol(
        """
class Counter:
    __slots__ = ('count',)
    def __init__(self):
        self.count = 0
    def bump(self):
        value = _instance_field(self, 'count')
        _set_instance_field(self, 'count', (value + 1) & 0xFFFF)
""",
        "<physical-scalar-lifetime>",
        namespace="$mule.physical_scalar_lifetime",
    )
    function = next(
        function
        for function in compilation.lambdas
        if function.code is not None and function.code.qualname == "Counter.bump"
    )
    module = ModuleGenerator(Program(compilation.lambdas, None, compilation.facts))
    _, variants = module.contexts(function)
    assert variants
    component = compile_component(function, variants[0])
    reads = [
        step
        for block in component.blocks
        for step in block.vops
        if step.info.name.startswith("physical-field-read")
    ]
    assert reads
    assert all(not step.results[0].tn.sc.owned for step in reads)
    assert all(step.args[0].tn.borrowed_from is not None for step in reads)
    body = module.emit_body(function, variants[0])
    assert "physical.loaded" in body
    assert "call ptr @mule.fast.retain(" not in body


def test_other_physical_read_keeps_first_field_owned():
    compilation = compile_protocol(
        """
class Holder:
    __slots__ = ('value',)
    def compare(self, other):
        same = _instance_field(self, 'value') is _instance_field(other, 'value')
        return same, self
""",
        "<physical-lifetime>",
        namespace="$mule.physical_lifetime",
    )
    function = next(
        function
        for function in compilation.lambdas
        if function.code is not None and function.code.qualname == "Holder.compare"
    )
    module = ModuleGenerator(Program(compilation.lambdas, None, compilation.facts))
    _, variants = module.contexts(function)
    assert variants
    component = compile_component(function, variants[0])
    reads = [
        step
        for block in component.blocks
        for step in block.vops
        if step.info.name.startswith("physical-field-read")
    ]
    assert reads
    first = reads[0]
    assert first.info.name == "physical-field-read"
    assert first.results[0].tn.borrowed_from is None


def test_physical_field_roots_callback_free_element_borrow():
    compilation = compile_protocol(
        """
class Holder:
    __slots__ = ('items',)
    def __init__(self, value):
        self.items = [value]
    def matches(self, index, other):
        return _storage_read(_instance_field(self, 'items'), index & 0xFFFF) is other
""",
        "<physical-element-lifetime>",
        namespace="$mule.physical_element_lifetime",
    )
    function = next(
        function
        for function in compilation.lambdas
        if function.code is not None and function.code.qualname == "Holder.matches"
    )
    module = ModuleGenerator(Program(compilation.lambdas, None, compilation.facts))
    _, variants = module.contexts(function)
    assert variants
    component = compile_component(function, variants[0])
    reads = [
        step
        for block in component.blocks
        for step in block.vops
        if step.info.name == "primitive-_storage_read"
    ]
    assert reads
    assert all(step.codegen_info == "storage_borrow" for step in reads)


def test_element_stays_owned_across_unrelated_binding_destruction():
    compilation = compile_protocol(
        """
class Holder:
    __slots__ = ('items',)
    def __init__(self, value):
        self.items = [value]
    def matches(self, index, other):
        return _storage_read(_instance_field(self, 'items'), index & 0xFFFF) is (other := None)
""",
        "<physical-element-rebinding>",
        namespace="$mule.physical_element_rebinding",
    )
    function = next(
        function
        for function in compilation.lambdas
        if function.code is not None and function.code.qualname == "Holder.matches"
    )
    module = ModuleGenerator(Program(compilation.lambdas, None, compilation.facts))
    _, variants = module.contexts(function)
    component = compile_component(function, variants[0])
    reads = [
        step
        for block in component.blocks
        for step in block.vops
        if step.info.name == "primitive-_storage_read"
    ]
    assert reads
    assert all(step.codegen_info != "storage_borrow" for step in reads)


def test_physical_array_borrows_into_terminal_storage_write():
    compilation = compile_protocol(
        """
class Holder:
    __slots__ = ('items',)
    def __init__(self, value):
        self.items = [value]
    def store(self, index, value):
        _storage_write(_instance_field(self, 'items'), index & 0xFFFF, value)
""",
        "<physical-terminal-storage>",
        namespace="$mule.physical_terminal_storage",
    )
    function = next(
        function
        for function in compilation.lambdas
        if function.code is not None and function.code.qualname == "Holder.store"
    )
    module = ModuleGenerator(Program(compilation.lambdas, None, compilation.facts))
    _, variants = module.contexts(function)
    component = compile_component(function, variants[0])
    stores = [
        step
        for block in component.blocks
        for step in block.vops
        if step.info.name == "primitive-_storage_write"
    ]
    assert stores
    assert all(step.args[0].tn.borrowed_from is not None for step in stores)


def test_physical_field_element_local_keeps_its_receiver_root():
    compilation = compile_protocol(
        """
class Holder:
    __slots__ = ('items',)
    def __init__(self, value):
        self.items = [value]
    def matches(self, index, other):
        value = _storage_read(_instance_field(self, 'items'), index & 0xFFFF)
        return value is other
""",
        "<physical-element-local>",
        namespace="$mule.physical_element_local",
    )
    function = next(
        function
        for function in compilation.lambdas
        if function.code is not None and function.code.qualname == "Holder.matches"
    )
    module = ModuleGenerator(Program(compilation.lambdas, None, compilation.facts))
    _, variants = module.contexts(function)
    component = compile_component(function, variants[0])
    reads = [
        step
        for block in component.blocks
        for step in block.vops
        if step.info.name == "primitive-_storage_read"
    ]
    assert reads
    assert all(step.codegen_info == "storage_borrow" for step in reads)
