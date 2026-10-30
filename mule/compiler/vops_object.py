"""Attribute, physical field, class, closure, and function allocation VOP definitions."""

from __future__ import annotations

from . import fndb
from .ctype import (
    UNKNOWN,
)
from .vmdef import define_vop
from .vops_core import (
    ATTR_FIELD,
    ATTR_KIND_MASK,
    ATTR_SHIFT,
    COLD_BRANCH,
    HOT_BRANCH,
    INSTANCE_FIELDS_OFFSET,
    OBJECT,
)


@define_vop("getattr", translate=fndb.GETATTR, args=(OBJECT,), results=(OBJECT,))
def getattr_(gen, vop):
    receiver = gen.take(vop.args[0])
    result = vop.results[0]
    name = vop.codegen_info.name
    receiver_type = vop.args[0].tn.type or UNKNOWN
    value = _read_attribute(
        gen,
        receiver,
        name,
        result.tn,
        slot_index=receiver_type.fixed_slot_index(name),
    )
    gen.publish(result, value)
    gen.release(receiver)
    gen.check_pointer(value)
    gen.refine(result.tn, value)


@define_vop("getattr-borrow", args=(OBJECT,), results=(OBJECT, OBJECT))
def getattr_borrow(gen, vop):
    receiver = gen.take(vop.args[0])
    name = vop.codegen_info.name
    slot_index = (vop.args[0].tn.type or UNKNOWN).fixed_slot_index(name)
    cache = gen.module.attribute_cache()
    site = _AttributeSite(gen, receiver, cache, ATTR_FIELD, slot_index=slot_index)
    slot = _attribute_slot(gen, receiver, site.encoded, slot_index)
    value = gen.operation(f"load ptr, ptr {slot}")
    unset = gen.operation(f"icmp eq ptr {value}, null")
    hit = gen.fresh("attr.borrowed")[1:]
    gen.line(f"br i1 {unset}, label %{site.slow}, label %{hit}, {COLD_BRANCH}")
    gen.lines.append(f"{hit}:")
    gen.line(f"br label %{site.join}")
    gen.lines.append(f"{site.slow}:")
    with gen.cold_block():
        slow = gen.operation(
            f"call ptr @mule_get_attr_cached(ptr {receiver}, ptr {gen.module.text(name)}, ptr {cache})"
        )
        gen.line(f"br label %{site.join}")
    gen.lines.append(f"{site.join}:")
    owner = gen.operation(f"phi ptr [null, %{hit}], [{slow}, %{site.slow}]")
    borrowed = gen.operation(f"phi ptr [{value}, %{hit}], [{slow}, %{site.slow}]")
    # Ownership is tracked by SSA identity. Publish the nullable owner first,
    # before the borrowed alias or any error edge can observe the result.
    gen.publish(vop.results[1], owner)
    gen.publish(vop.results[0], borrowed)
    gen.release(receiver)
    gen.check_pointer(borrowed)
    gen.refine(vop.results[0].tn, borrowed)


@define_vop("slot-borrow-end", args=(OBJECT, OBJECT), arg_escapes=(False, False))
def slot_borrow_end(gen, vop):
    gen.release(gen.take(vop.args[0]))
    # The explicit receiver operand extends the root's live range.
    gen.load(vop.args[1])


def _read_physical_field(
    gen,
    receiver,
    name,
    index,
    *,
    borrowed=False,
    retain_value=True,
    result_type=UNKNOWN,
    join=None,
):
    slot = _attribute_slot(gen, receiver, None, index)
    value = gen.operation(f"load ptr, ptr {slot}")
    unset = gen.operation(f"icmp eq ptr {value}, null")
    missing = gen.fresh("physical.missing")[1:]
    loaded = gen.fresh("physical.borrowed" if borrowed else "physical.loaded")[1:]
    done = gen.fresh("physical.join")[1:] if join is None else join
    gen.line(f"br i1 {unset}, label %{missing}, label %{loaded}, {COLD_BRANCH}")
    gen.lines.append(loaded + ":")
    if retain_value and not borrowed:
        retain = "heap.retain" if result_type.is_heap_object else "fast.retain"
        gen.line(f"call ptr @mule.{retain}(ptr {value})")
    gen.line(f"br label %{done}")
    gen.lines.append(missing + ":")
    with gen.cold_block():
        gen.line(
            f"call void @mule_instance_field_missing(ptr {receiver}, "
            f"ptr {gen.module.text(name)}, i64 {len(name.encode('utf-8'))})"
        )
        gen.line(f"br label %{done}")
    gen.lines.append(done + ":")
    # The error arm reports this load's null observation; it never rereads a
    # slot that another thread or callback could initialize in the meantime.
    return gen.operation(f"phi ptr [{value}, %{loaded}], [null, %{missing}]")


def physical_field_read(gen, vop, values):
    """Physical primitive emitter: operands are borrowed, result is owned."""
    return gen.operation(
        f"call ptr @mule_instance_field(ptr {values[0]}, ptr {values[1]})"
    )


def _write_physical_field(gen, receiver, value, name, index):
    slot = _attribute_slot(gen, receiver, None, index)
    old = gen.operation(f"load ptr, ptr {slot}")
    retained = gen.retain(value)
    gen.line(f"store ptr {retained}, ptr {slot}")
    gen.release(old)
    return "@mule.none"


def physical_field_write(gen, vop, values):
    return gen.operation(
        f"call ptr @mule_set_instance_field(ptr {values[0]}, "
        f"ptr {values[1]}, ptr {values[2]})"
    )


@define_vop("physical-field-read", args=(OBJECT,), results=(OBJECT,))
def physical_field_read_fixed(gen, vop):
    receiver = gen.take(vop.args[0])
    plan = vop.codegen_info
    value = _read_physical_field(
        gen,
        receiver,
        plan.name,
        plan.index,
        retain_value=vop.results[0].tn.sc.owned,
        result_type=vop.results[0].tn.type or UNKNOWN,
    )
    gen.publish(vop.results[0], value)
    gen.release(receiver)
    gen.check_pointer(value)
    gen.refine(vop.results[0].tn, value)


@define_vop("physical-field-write", args=(OBJECT, OBJECT), results=(OBJECT,))
def physical_field_write_fixed(gen, vop):
    receiver, value = (gen.take(ref) for ref in vop.args)
    plan = vop.codegen_info
    result = _write_physical_field(gen, receiver, value, plan.name, plan.index)
    gen.publish(vop.results[0], result)
    gen.release(receiver)
    gen.release(value)
    gen.refine(vop.results[0].tn, result)


@define_vop("physical-field-read-borrow", args=(OBJECT,), results=(OBJECT,))
def physical_field_read_borrow(gen, vop):
    receiver = gen.take(vop.args[0])
    plan = vop.codegen_info
    value = _read_physical_field(gen, receiver, plan.name, plan.index, borrowed=True)
    gen.publish(vop.results[0], value)
    gen.release(receiver)
    gen.check_pointer(value)
    gen.refine(vop.results[0].tn, value)


@define_vop("physical-field-borrow-end", args=(OBJECT,), arg_escapes=(False,))
def physical_field_borrow_end(gen, vop):
    gen.load(vop.args[0])


def _physical_name_call(gen, receiver, plan, value=None):
    arguments = (
        f"ptr {receiver}, ptr {gen.module.text(plan.name)}, "
        f"i64 {len(plan.name.encode('utf-8'))}"
    )
    function = "mule_instance_field_name"
    if value is not None:
        function = "mule_set_instance_field_name"
        arguments += f", ptr {value}"
    return gen.operation(f"call ptr @{function}({arguments})")


def _cached_physical_name_field(gen, receiver, plan, value=None):
    """A named physical field on a receiver of unproven class, through the
    site's attribute cache: the cache maps the receiver's class layout to the
    field's slot, so only the first receiver of each class pays the by-name
    lookup (the slot-access caches SBCL's PCL keeps per call site). A class
    with an instance dictionary, an epoch change, or an unbound field takes
    the runtime path, which fills the cache or raises."""
    cache = gen.module.attribute_cache()
    site = _AttributeSite(gen, receiver, cache, ATTR_FIELD)
    slot = _attribute_slot(gen, receiver, site.encoded, None)
    arguments = (
        f"ptr {receiver}, ptr {gen.module.text(plan.name)}, "
        f"i64 {len(plan.name.encode('utf-8'))}"
    )
    if value is None:
        field = gen.operation(f"load ptr, ptr {slot}")
        unset = gen.operation(f"icmp eq ptr {field}, null")
        loaded = gen.fresh("field.cached")[1:]
        gen.line(f"br i1 {unset}, label %{site.slow}, label %{loaded}, {COLD_BRANCH}")
        gen.lines.append(f"{loaded}:")
        gen.line(f"call ptr @mule.fast.retain(ptr {field})")
        gen.line(f"br label %{site.join}")
        gen.lines.append(f"{site.slow}:")
        with gen.cold_block():
            slow = gen.operation(
                f"call ptr @mule_instance_field_name_cached({arguments}, ptr {cache})"
            )
            gen.line(f"br label %{site.join}")
        gen.lines.append(f"{site.join}:")
        return gen.operation(f"phi ptr [{field}, %{loaded}], [{slow}, %{site.slow}]")
    old = gen.operation(f"load ptr, ptr {slot}")
    retained = gen.retain(value)
    gen.line(f"store ptr {retained}, ptr {slot}")
    gen.release(old)
    stored = gen.fresh("field.stored")[1:]
    gen.line(f"br label %{stored}")
    gen.lines.append(f"{stored}:")
    gen.line(f"br label %{site.join}")
    gen.lines.append(f"{site.slow}:")
    slow = gen.operation(
        f"call ptr @mule_set_instance_field_name_cached({arguments}, "
        f"ptr {value}, ptr {cache})"
    )
    gen.line(f"br label %{site.join}")
    gen.lines.append(f"{site.join}:")
    return gen.operation(f"phi ptr [@mule.none, %{stored}], [{slow}, %{site.slow}]")


def _guarded_physical_field(gen, receiver, plan, value=None):
    actual = gen.operation(f"call i64 @mule_reference_layout_id(ptr {receiver})")
    matches = "false"
    for identity in plan.identities:
        expected = gen.operation(
            f"load i64, ptr {gen.module.class_layout_slot(identity)}"
        )
        known = gen.operation(f"icmp ne i64 {expected}, 0")
        same = gen.operation(f"icmp eq i64 {actual}, {expected}")
        same = gen.operation(f"and i1 {known}, {same}")
        matches = gen.operation(f"or i1 {matches}, {same}")
    hit = gen.fresh("physical.guard.hit")[1:]
    miss = gen.fresh("physical.guard.miss")[1:]
    done = gen.fresh("physical.guard.join")[1:]
    gen.line(f"br i1 {matches}, label %{hit}, label %{miss}, {HOT_BRANCH}")
    gen.lines.append(hit + ":")
    if value is None:
        fast_block = gen.fresh("physical.read.join")[1:]
        fast = _read_physical_field(
            gen, receiver, plan.name, plan.index, join=fast_block
        )
    else:
        fast = _write_physical_field(gen, receiver, value, plan.name, plan.index)
        fast_block = hit
    gen.line(f"br label %{done}")
    gen.lines.append(miss + ":")
    with gen.cold_block():
        slow = _physical_name_call(gen, receiver, plan, value)
        gen.line(f"br label %{done}")
    gen.lines.append(done + ":")
    return gen.operation(f"phi ptr [{fast}, %{fast_block}], [{slow}, %{miss}]")


def _emit_planned_physical(gen, vop, *, write=False, guarded=False):
    values = [gen.take(ref) for ref in vop.args]
    receiver = values[0]
    value = values[1] if write else None
    plan = vop.codegen_info
    result = (
        _guarded_physical_field(gen, receiver, plan, value)
        if guarded
        else _cached_physical_name_field(gen, receiver, plan, value)
    )
    gen.publish(vop.results[0], result)
    for argument in values:
        gen.release(argument)
    gen.check_pointer(result)
    gen.refine(vop.results[0].tn, result)


@define_vop("physical-field-read-name", args=(OBJECT,), results=(OBJECT,))
def physical_field_read_name(gen, vop):
    _emit_planned_physical(gen, vop)


@define_vop("physical-field-write-name", args=(OBJECT, OBJECT), results=(OBJECT,))
def physical_field_write_name(gen, vop):
    _emit_planned_physical(gen, vop, write=True)


@define_vop("physical-field-read-guarded", args=(OBJECT,), results=(OBJECT,))
def physical_field_read_guarded(gen, vop):
    _emit_planned_physical(gen, vop, guarded=True)


@define_vop("physical-field-write-guarded", args=(OBJECT, OBJECT), results=(OBJECT,))
def physical_field_write_guarded(gen, vop):
    _emit_planned_physical(gen, vop, write=True, guarded=True)


def _read_attribute(gen, receiver, name, result_tn=None, *, slot_index=None):
    """An owned field value, with guarded physical access and dynamic lookup."""
    cache = gen.module.attribute_cache()
    if slot_index is None:
        return gen.operation(
            f"call ptr @mule.attr.get(ptr {receiver}, ptr {gen.module.text(name)}, ptr {cache})"
        )
    labels = _AttributeSite(gen, receiver, cache, ATTR_FIELD, slot_index=slot_index)
    slot = _attribute_slot(gen, receiver, labels.encoded, slot_index)
    field = gen.operation(f"load ptr, ptr {slot}")
    unset = gen.operation(f"icmp eq ptr {field}, null")
    loaded = gen.fresh("attr.loaded")[1:]
    gen.line(f"br i1 {unset}, label %{labels.slow}, label %{loaded}, {COLD_BRANCH}")
    gen.lines.append(f"{loaded}:")
    gen.line(f"call ptr @mule.fast.retain(ptr {field})")
    gen.line(f"br label %{labels.join}")
    gen.lines.append(f"{labels.slow}:")
    with gen.cold_block():
        slow = gen.operation(
            f"call ptr @mule_get_attr_cached(ptr {receiver}, ptr {gen.module.text(name)}, "
            f"ptr {cache})"
        )
        gen.line(f"br label %{labels.join}")
    gen.lines.append(f"{labels.join}:")
    return gen.operation(f"phi ptr [{field}, %{loaded}], [{slow}, %{labels.slow}]")


class _AttributeSite:
    """The inline cache probe opening one attribute site: falls through to
    the hit block when the site's cache holds an encoding of ``kind`` for
    the receiver's class, else branches to ``slow``. ``join`` is the label
    the caller merges both paths at."""

    def __init__(self, gen, receiver: str, cache: str, kind: int, *, slot_index=None):
        self.encoded = gen.operation(
            f"call i64 @mule.attr.hit(ptr {receiver}, ptr {cache})"
        )
        if slot_index is None:
            found = gen.operation(f"and i64 {self.encoded}, {ATTR_KIND_MASK}")
            matched = gen.operation(f"icmp eq i64 {found}, {kind}")
        else:
            expected = (slot_index << ATTR_SHIFT) | kind
            matched = gen.operation(f"icmp eq i64 {self.encoded}, {expected}")
        hit = gen.fresh("attr.fixed" if slot_index is not None else "attr.hit")[1:]
        self.slow = gen.fresh("attr.slow")[1:]
        self.join = gen.fresh("attr.join")[1:]
        gen.line(f"br i1 {matched}, label %{hit}, label %{self.slow}, {HOT_BRANCH}")
        gen.lines.append(f"{hit}:")


def _attribute_slot(gen, receiver, encoded, slot_index):
    if slot_index is None:
        return gen.operation(
            f"call ptr @mule.attr.field(ptr {receiver}, i64 {encoded})"
        )
    fields_addr = gen.operation(
        f"getelementptr i8, ptr {receiver}, i64 {INSTANCE_FIELDS_OFFSET}"
    )
    fields = gen.operation(f"load ptr, ptr {fields_addr}")
    return gen.operation(f"getelementptr ptr, ptr {fields}, i64 {slot_index}")


@define_vop(
    "getattr-default",
    translate=fndb.GETATTR_DEFAULT,
    args=(OBJECT, OBJECT),
    results=(OBJECT,),
)
def getattr_default(gen, vop):
    """``getattr(x, "name", default)``: a per-site cache that also records
    misses, so a repeated miss returns the default after one class check."""
    receiver, default = gen.take(vop.args[0]), gen.take(vop.args[1])
    info = vop.codegen_info
    gen.pointer_result(
        vop.results[0],
        f"call ptr @mule_getattr_default(ptr {receiver}, ptr {gen.module.text(info.name)}, "
        f"ptr {default}, ptr {gen.module.attribute_cache()})",
        (receiver, default),
    )


@define_vop("hasattr", translate=fndb.HASATTR, args=(OBJECT,), results=(OBJECT,))
def hasattr_(gen, vop):
    receiver = gen.take(vop.args[0])
    info = vop.codegen_info
    gen.pointer_result(
        vop.results[0],
        f"call ptr @mule_hasattr(ptr {receiver}, ptr {gen.module.text(info.name)}, "
        f"ptr {gen.module.attribute_cache()})",
        (receiver,),
    )


@define_vop("setattr", translate=fndb.SETATTR, args=(OBJECT, OBJECT))
def setattr_(gen, vop):
    receiver, value = gen.take(vop.args[0]), gen.take(vop.args[1])
    move_value = gen.owns_reference(value) and value != receiver
    status = _write_attribute(
        gen,
        receiver,
        vop.codegen_info.name,
        value,
        move_value=move_value,
        value_tn=vop.args[1].tn,
        slot_index=(vop.args[0].tn.type or UNKNOWN).fixed_slot_index(
            vop.codegen_info.name
        ),
    )
    gen.release(receiver)
    if not move_value:
        gen.release(value)
    gen.check_status(status)


def _write_attribute(
    gen,
    receiver,
    name,
    value,
    *,
    move_value: bool = False,
    value_tn=None,
    slot_index=None,
):
    """Assign a borrowed or moved value, completing old-value destruction before return."""
    cache = gen.module.attribute_cache()
    if slot_index is None:
        helper = "@mule.attr.set.move" if move_value else "@mule.attr.set"
        return gen.operation(
            f"call i32 {helper}(ptr {receiver}, ptr {gen.module.text(name)}, ptr {value}, ptr {cache})"
        )
    site = _AttributeSite(gen, receiver, cache, ATTR_FIELD, slot_index=slot_index)
    slot = _attribute_slot(gen, receiver, site.encoded, slot_index)
    old = gen.operation(f"load ptr, ptr {slot}")
    if not move_value:
        gen.line(f"call ptr @mule.fast.retain(ptr {value})")
    gen.line(f"store ptr {value}, ptr {slot}")
    gen.line(f"call void @mule.fast.release(ptr {old})")
    stored = gen.current_block()
    gen.line(f"br label %{site.join}")
    gen.lines.append(f"{site.slow}:")
    with gen.cold_block():
        slow = gen.operation(
            f"call i32 @mule_set_attr_cached(ptr {receiver}, ptr {gen.module.text(name)}, "
            f"ptr {value}, ptr {cache})"
        )
        if move_value:
            gen.release(value)
        gen.line(f"br label %{site.join}")
    gen.lines.append(f"{site.join}:")
    return gen.operation(f"phi i32 [0, %{stored}], [{slow}, %{site.slow}]")


@define_vop("value-cell-ref", args=(OBJECT,), results=(OBJECT,), arg_escapes=(False,))
def value_cell_ref(gen, vop):
    """SBCL's value-cell-ref, guarded for Python's mutable class attributes."""
    cell = gen.load(vop.args[0])
    helper = gen.module.cell_ref_helper(optional=bool(vop.codegen_info))
    gen.pointer_result(
        vop.results[0],
        f"call ptr {helper}(ptr {cell})",
    )


@define_vop("value-cell-set", args=(OBJECT, OBJECT), arg_escapes=(False, True))
def value_cell_set(gen, vop):
    cell = gen.load(vop.args[0])
    value = gen.take(vop.args[1])
    helper = gen.module.cell_set_helper()
    status = gen.operation(f"call i32 {helper}(ptr {cell}, ptr {value})")
    gen.release(value)
    gen.check_status(status)


@define_vop("value-cell-clear", args=(OBJECT,), arg_escapes=(False,))
def value_cell_clear(gen, vop):
    cell = gen.load(vop.args[0])
    helper = gen.module.cell_clear_helper()
    status = gen.operation(f"call i32 {helper}(ptr {cell})")
    gen.check_status(status)


@define_vop("isinstance", translate=fndb.ISINSTANCE, results=(OBJECT,))
def isinstance_(gen, vop):
    """Single-class or multi-class ``isinstance`` without a tuple. A single
    class checks exact instance identity inline before calling the runtime;
    multiple classes are passed in a stack buffer."""
    value, *classes = (gen.take(arg) for arg in vop.args)
    if len(classes) == 1:
        gen.pointer_result(
            vop.results[0],
            f"call ptr @mule.fast.isinstance.1(ptr {value}, ptr {classes[0]})",
            (value, classes[0]),
        )
        return
    gen.argv(classes)
    gen.pointer_result(
        vop.results[0],
        f"call ptr @mule_isinstance_any(ptr {value}, ptr %argv, i64 {len(classes)})",
        (value, *classes),
    )


@define_vop(
    "resolve-method",
    translate=fndb.RESOLVE_METHOD,
    args=(OBJECT,),
    results=(OBJECT, OBJECT),
)
def resolve_method(gen, vop):
    receiver = gen.take(vop.args[0])
    method, bound = vop.results
    info = vop.codegen_info
    cache = gen.module.attribute_cache()
    out = gen.out_pointer()
    value = gen.operation(
        f"call ptr @mule.attr.method(ptr {receiver}, ptr {gen.module.text(info.name)}, "
        f"ptr {cache}, ptr {out})"
    )
    receiver_bound = gen.operation(f"load ptr, ptr {out}")
    gen.publish(method, value)
    gen.publish(bound, receiver_bound)
    gen.release(receiver)
    gen.check_pointer(value)
    gen.refine(method.tn, value)


@define_vop(
    "resolve-super-method",
    translate=fndb.RESOLVE_SUPER_METHOD,
    args=(OBJECT, OBJECT, OBJECT),
    results=(OBJECT, OBJECT),
)
def resolve_super_method(gen, vop):
    super_callee = gen.take(vop.args[0])
    thisclass = gen.take(vop.args[1])
    receiver = gen.take(vop.args[2])
    method, bound = vop.results
    info = vop.codegen_info
    out = gen.out_pointer()
    value = gen.operation(
        f"call ptr @mule_resolve_super_method(ptr {super_callee}, ptr {thisclass}, "
        f"ptr {receiver}, ptr {gen.module.text(info.name)}, "
        f"ptr {gen.module.attribute_cache()}, ptr {out})"
    )
    gen.publish(method, value)
    gen.publish(bound, gen.operation(f"load ptr, ptr {out}"))
    gen.release(super_callee)
    gen.release(thisclass)
    gen.release(receiver)
    gen.check_pointer(value)
    gen.refine(method.tn, value)


@define_vop("call-resolved", translate=fndb.CALL_RESOLVED, results=(OBJECT,))
def call_resolved(gen, vop):
    method, bound, *arguments = vop.args
    function = gen.take(method)
    receiver = gen.take(bound)
    values = [gen.take(argument) for argument in arguments]
    value = gen.call_method(
        vop.codegen_info,
        function,
        receiver,
        values,
        unpack_arity=gen.unpacked_call_arity(vop),
        argument_refs=arguments,
        result_contract=gen.call_result_contract(vop),
    )
    gen.publish(vop.results[0], value)
    gen.release(function)
    gen.check_pointer(value)
    gen.refine(vop.results[0].tn, value)


@define_vop(
    "make-function", translate=fndb.MAKE_FUNCTION, results=(OBJECT, OBJECT, OBJECT)
)
def make_function(gen, vop):
    result = vop.results[0]
    definition = vop.codegen_info
    target = definition.lambda_
    info = definition.info
    module = gen.module
    closure_count = len(target.environment.closure) if target.environment else 0
    defaults_refs = vop.args[: len(vop.args) - closure_count]
    closure_refs = vop.args[len(vop.args) - closure_count :]
    native = module.functions[target]
    name = module.text(info.public_name)
    defaults_slot, closure_slot = vop.results[1:]
    defaults = gen.tuple_of(defaults_slot, defaults_refs)
    closure = gen.tuple_of(closure_slot, closure_refs)
    if defaults is not None:
        defaults = gen.take(defaults_slot)
    if closure is not None:
        closure = gen.take(closure_slot)
    if defaults is None and target.binding is not None:
        defaults = "null"
    consumed = [value for value in (defaults, closure) if value not in (None, "null")]
    # A function taking keywords gets its keyword entry from
    # ``mule_function_defaults``, even without defaults ("null").
    if defaults is not None:
        call = (
            f"call ptr @mule_function_defaults(ptr %globals, ptr {native}, ptr {name}, "
            f"ptr {defaults})"
        )
    else:
        call = f"call ptr @mule_function(ptr %globals, ptr {native}, ptr {name})"
    if closure is not None:
        # Assigned captured variables live in cells; the others are captured
        # by value. ``__closure__`` presents every entry as a cell.
        cells = f"@mule.closure.cells.{len(module.auxiliary)}"
        flags = ", ".join(
            f"i8 {int(var.indirect)}" for var in target.environment.closure
        )
        module.auxiliary.append(
            f"{cells} = private constant [{closure_count} x i8] [{flags}], align 1"
        )
        function = gen.operation(call)
        call = f"call ptr @mule_function_closure(ptr {function}, ptr {closure}, ptr {cells})"
    gen.pointer_result(result, call, consumed)

    metadata = gen.operation(gen.constant(module.code_metadata(target)))
    gen.check_pointer(metadata)
    function = gen.load(result)
    gen.line(
        f"call void @mule_set_function_metadata(ptr {function}, ptr {module.code_descriptor(target)}, ptr {metadata})"
    )
    gen.release(metadata)
    if info.docstring is not None:
        docstring = gen.operation(gen.constant(info.docstring))
        gen.check_pointer(docstring)
        status = gen.operation(
            f"call i32 @mule_set_attr_cached(ptr {function}, "
            f"ptr {module.text('__doc__')}, ptr {docstring}, ptr null)"
        )
        gen.release(docstring)
        gen.check_status(status)


@define_vop("make-class", translate=fndb.MAKE_CLASS, results=(OBJECT,))
def make_class(gen, vop):
    info = vop.codegen_info
    module = gen.module
    operands = [gen.take(operand) for operand in vop.args]
    bases, namespace, keywords = operands
    layout = module.layouts.get(info.class_id)
    name = module.text(info.public_name)
    if layout is not None:
        gen.pointer_result(
            vop.results[0],
            f"call ptr @mule_native_class(ptr {name}, "
            f"ptr {layout.schema}, ptr %globals, i64 {len(info.fields)}, ptr {module.names(info.fields)}, "
            f"ptr {namespace}, ptr {layout.constructor}, ptr {layout.getter}, ptr {layout.setter})",
            operands,
        )
        return
    gen.pointer_result(
        vop.results[0],
        f"call ptr @mule_class(ptr {name}, ptr {bases}, "
        f"i64 {len(info.fields)}, ptr {module.names(info.fields)}, ptr {namespace}, "
        f"ptr {keywords})",
        operands,
    )
    # Record the class object's layout for receiver guards on its instances.
    # A metaclass may return something other than a class; the slot then
    # holds zero and every guard fails.
    created = gen.load(vop.results[0])
    layout = gen.operation(f"call i64 @mule.class.layout(ptr {created})")
    gen.line(f"store i64 {layout}, ptr {module.class_layout_slot(info.identity)}")


@define_vop(
    "class-namespace",
    translate=fndb.CLASS_NAMESPACE,
    args=(OBJECT, OBJECT, OBJECT),
    results=(OBJECT,),
)
def class_namespace(gen, vop):
    bases, keywords, name = (gen.take(operand) for operand in vop.args)
    gen.pointer_result(
        vop.results[0],
        f"call ptr @mule_class_prepare(ptr {name}, ptr {bases}, ptr {keywords})",
        (bases, keywords, name),
    )


@define_vop("class-load", translate=fndb.CLASS_LOAD, args=(OBJECT,), results=(OBJECT,))
def class_load(gen, vop):
    namespace = gen.take(vop.args[0])
    gen.pointer_result(
        vop.results[0],
        f"call ptr @mule_class_load(ptr {namespace}, ptr %globals, "
        f"ptr {gen.module.text(vop.codegen_info.name)})",
        (namespace,),
    )


@define_vop(
    "class-load-or",
    translate=fndb.CLASS_LOAD_OR,
    args=(OBJECT, OBJECT),
    results=(OBJECT,),
)
def class_load_or(gen, vop):
    namespace, fallback = gen.take(vop.args[0]), gen.take(vop.args[1])
    gen.pointer_result(
        vop.results[0],
        f"call ptr @mule_class_load_or(ptr {namespace}, ptr {fallback}, "
        f"ptr {gen.module.text(vop.codegen_info.name)})",
        (namespace, fallback),
    )


@define_vop("class-store", translate=fndb.CLASS_STORE, args=(OBJECT, OBJECT))
def class_store(gen, vop):
    namespace, value = gen.take(vop.args[0]), gen.take(vop.args[1])
    status = gen.operation(
        f"call i32 @mule_class_store(ptr {namespace}, "
        f"ptr {gen.module.text(vop.codegen_info.name)}, ptr {value})"
    )
    gen.release(namespace)
    gen.release(value)
    gen.check_status(status)


@define_vop("class-delete", translate=fndb.CLASS_DELETE, args=(OBJECT,))
def class_delete(gen, vop):
    namespace = gen.take(vop.args[0])
    info = vop.codegen_info
    status = gen.operation(
        f"call i32 @mule_class_delete(ptr {namespace}, "
        f"ptr {gen.module.text(info.name)}, i32 {int(info.missing_ok)})"
    )
    gen.release(namespace)
    gen.check_status(status)


@define_vop("import", translate=fndb.IMPORT, results=(OBJECT,))
def import_(gen, vop):
    symbol = gen.module.import_symbol(vop.codegen_info)
    gen.pointer_result(vop.results[0], f"call ptr {symbol}()")


@define_vop("yield", translate=fndb.YIELD, args=(OBJECT,), results=(OBJECT,))
def yield_(gen, vop):
    value = gen.take(vop.args[0])
    gen.sync_coroutine_locals()
    gen.pointer_result(
        vop.results[0], f"call ptr @mule_coroutine_yield(ptr {value})", (value,)
    )


@define_vop("import-star", translate=fndb.IMPORT_STAR, args=(OBJECT,))
def import_star(gen, vop):
    module = gen.take(vop.args[0])
    status = gen.operation(f"call i32 @mule_import_star(ptr {module}, ptr %globals)")
    gen.release(module)
    gen.check_status(status)


@define_vop(
    "has-binding", translate=fndb.HAS_BINDING, args=(OBJECT,), results=(OBJECT,)
)
def has_binding(gen, vop):
    module = gen.take(vop.args[0])
    status = gen.operation(
        f"call i32 @mule_namespace_bound(ptr {module}, "
        f"ptr {gen.module.text(vop.codegen_info)})"
    )
    gen.release(module)
    gen.check_status(status)
    bound = gen.operation(f"icmp ne i32 {status}, 0")
    result = gen.operation(f"select i1 {bound}, ptr @mule.true, ptr @mule.false")
    gen.native_result(vop.results[0], result)


@define_vop("current-globals", translate=fndb.CURRENT_GLOBALS, results=(OBJECT,))
def current_globals(gen, vop):
    # The entry's globals argument, retained for the result's owner.
    gen.pointer_result(vop.results[0], "call ptr @mule.fast.retain(ptr %globals)")
