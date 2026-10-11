//! Bootstrap fixed-layout class storage and attribute dispatch.
//!
//! A class is built from the namespace its body filled: every binding becomes
//! a class attribute, merged over the base's attributes. Instances store the
//! fixed fields the compiler found assigned through method receivers. Lookup
//! follows Python's order: a data descriptor on the class, then an instance
//! field, then the class attribute (binding functions and calling `__get__`),
//! then `__getattr__`. Descriptor behavior itself (`property`, `classmethod`,
//! `staticmethod`, `super`) is compiled Mule Python; this file only applies
//! the protocol. Native raw-slot protocols are compiler-generated; Rust
//! forwards to their hooks.
use crate::object::{self, Data, Value};
use crate::text::Text;
use std::collections::HashMap;
use std::ffi::{CStr, c_char, c_void};
use std::sync::atomic::{AtomicUsize, Ordering};

static NEXT_CLASS_ID: AtomicUsize = AtomicUsize::new(1);
pub struct ClassLayout {
    pub id: usize,
    pub name: String,
    pub base: Value,
    pub fields: Vec<String>,
    /// Own and inherited class attributes; own bindings replace inherited ones.
    pub attributes: Vec<(String, Value)>,
    field_lookup: HashMap<String, usize>,
    attribute_lookup: HashMap<String, usize>,
    initializer: Option<usize>,
    boolean: Option<usize>,
    length: Option<usize>,
    callable: Option<usize>,
    getattribute: Option<usize>,
    getattr: Option<usize>,
    pub(crate) native: Option<NativeLayout>,
}

impl ClassLayout {
    /// The raw class attribute, borrowed, or NULL when absent.
    pub fn method(&self, name: &str) -> Value {
        self.attribute_lookup
            .get(name)
            .map_or(std::ptr::null_mut(), |index| self.attributes[*index].1)
    }
}

pub(crate) struct NativeLayout {
    pub(crate) schema: *const c_void,
    // As with native function environments, the executing module owns globals.
    pub(crate) globals: Value,
    constructor: NativeConstructor,
    getter: NativeGetter,
    setter: NativeSetter,
}

/// Compatibility descriptor allocation. Native layout protocols are generated
/// by the compiler; this bootstrap metadata path only records their entry points.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_native_class(
    class_name: *const c_char,
    schema: *const c_void,
    globals: Value,
    field_count: isize,
    field_names: *const *const c_char,
    namespace: Value,
    constructor: Option<NativeConstructor>,
    getter: Option<NativeGetter>,
    setter: Option<NativeSetter>,
) -> Value {
    let (Some(constructor), Some(getter), Some(setter)) = (constructor, getter, setter) else {
        object::error("SystemError", "NULL native storage protocol");
        return std::ptr::null_mut();
    };
    if schema.is_null() {
        object::error("SystemError", "NULL native layout schema");
        return std::ptr::null_mut();
    }
    unsafe {
        create_class(
            class_name,
            std::ptr::null_mut(),
            field_count,
            field_names,
            namespace,
            Some(NativeLayout {
                schema,
                globals,
                constructor,
                getter,
                setter,
            }),
        )
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_class(
    class_name: *const c_char,
    base: Value,
    field_count: isize,
    field_names: *const *const c_char,
    namespace: Value,
) -> Value {
    unsafe { create_class(class_name, base, field_count, field_names, namespace, None) }
}

pub type NativeConstructor = unsafe extern "C" fn(Value, Value, isize, *const Value) -> Value;
pub type NativeGetter = unsafe extern "C" fn(Value, Value, i64) -> Value;
pub type NativeSetter = unsafe extern "C" fn(Value, Value, i64, Value) -> i32;

/// LLVM reserves two zero-initialized machine words per attribute site.
/// IDs never repeat, so an expired class cannot make a cache entry valid again.
/// The encoding is a property of the class and name alone: whether the
/// receiver is an instance or the class itself is applied when it is used.
#[repr(C)]
#[derive(Default)]
pub struct AttrCache {
    pub class_id: usize,
    pub encoded: usize,
}
const FIELD: usize = 1;
const METHOD: usize = 2;
const VALUE: usize = 3;
const DESCRIPTOR: usize = 4;
const DATA_DESCRIPTOR: usize = 5;
const KIND: usize = 7;
const SHIFT: u32 = 3;

unsafe fn name<'a>(value: *const c_char) -> Option<&'a str> {
    unsafe {
        if value.is_null() {
            object::error("SystemError", "NULL attribute name");
            return None;
        }
        CStr::from_ptr(value)
            .to_str()
            .map_err(|_| {
                object::error("UnicodeDecodeError", "invalid attribute name");
            })
            .ok()
    }
}

/// The receiver's class layout, whether the receiver is an instance, and the
/// class object itself.
unsafe fn layout<'a>(receiver: Value) -> Option<(&'a ClassLayout, bool, Value)> {
    unsafe {
        if receiver.is_null() || object::is_fixnum(receiver) {
            return None;
        }
        match &(*receiver).data {
            Data::Class(layout) => Some((layout, false, receiver)),
            Data::Instance { ty, .. } | Data::NativeInstance { ty, .. } => match &(**ty).data {
                Data::Class(layout) => Some((layout, true, *ty)),
                _ => None,
            },
            _ => None,
        }
    }
}

/// The class layout of an instance of a compiled class.
unsafe fn instance_layout<'a>(value: Value) -> Option<&'a ClassLayout> {
    unsafe { layout(value).and_then(|(layout, instance, _)| instance.then_some(layout)) }
}

/// Whether a class attribute value defines `__set__` or `__delete__`.
unsafe fn is_data_descriptor(value: Value) -> bool {
    unsafe {
        instance_layout(value).is_some_and(|layout| {
            layout.attribute_lookup.contains_key("__set__")
                || layout.attribute_lookup.contains_key("__delete__")
        })
    }
}

unsafe fn attribute_kind(value: Value) -> usize {
    unsafe {
        if object::is_fixnum(value) {
            return VALUE;
        }
        if matches!(&(*value).data, Data::Function { .. }) {
            return METHOD;
        }
        match instance_layout(value) {
            Some(layout) if layout.attribute_lookup.contains_key("__get__") => {
                if is_data_descriptor(value) {
                    DATA_DESCRIPTOR
                } else {
                    DESCRIPTOR
                }
            }
            _ => VALUE,
        }
    }
}

fn encoded(layout: &ClassLayout, attribute: &str, cache: *mut AttrCache) -> Option<usize> {
    unsafe {
        let class_attribute = layout.attribute_lookup.get(attribute).map(|index| {
            let kind = attribute_kind(layout.attributes[*index].1);
            (index << SHIFT) | kind
        });
        let resolved = match class_attribute {
            Some(encoded) if encoded & KIND == DATA_DESCRIPTOR => Some(encoded),
            _ => layout
                .field_lookup
                .get(attribute)
                .map(|index| (index << SHIFT) | FIELD)
                .or(class_attribute),
        };
        if let Some(encoded) = resolved {
            if !cache.is_null() {
                (*cache).encoded = encoded;
                (*cache).class_id = layout.id;
            }
        }
        resolved
    }
}

#[inline]
unsafe fn cached(layout: &ClassLayout, cache: *mut AttrCache) -> Option<usize> {
    unsafe {
        if !cache.is_null() && (*cache).class_id == layout.id {
            return Some((*cache).encoded);
        }
        None
    }
}

/// Apply the descriptor protocol to a class attribute: functions bind to an
/// instance, objects defining `__get__` compute the value, and other values
/// are returned as they are. A NULL instance means access through the class.
pub unsafe fn bind_class_attribute(value: Value, instance: Value, owner: Value) -> Value {
    unsafe {
        match attribute_kind(value) {
            METHOD if !instance.is_null() => object::new_value(Data::BoundMethod {
                receiver: object::retain(instance),
                method: object::retain(value),
            }),
            DESCRIPTOR | DATA_DESCRIPTOR => {
                let instance = if instance.is_null() {
                    object::none_value()
                } else {
                    instance
                };
                special_method(value, "__get__", &[instance, owner])
            }
            _ => object::retain(value),
        }
    }
}

/// Resolve class attribute `index` for the receiver. Instance method lookups
/// return the function with the receiver in `bound`, avoiding a bound method.
unsafe fn class_attribute(
    layout: &ClassLayout,
    index: usize,
    kind: usize,
    receiver: Value,
    instance: bool,
    owner: Value,
    bound: *mut Value,
) -> Value {
    unsafe {
        let value = layout.attributes[index].1;
        if kind == METHOD && instance {
            *bound = object::retain(receiver);
            return object::retain(value);
        }
        bind_class_attribute(value, if instance { receiver } else { std::ptr::null_mut() }, owner)
    }
}

/// The value for a resolved encoding, or None when an instance field is
/// unset and the class has no attribute of that name either.
unsafe fn resolved(
    receiver: Value,
    layout: &ClassLayout,
    instance: bool,
    owner: Value,
    encoded: usize,
    attribute: *const c_char,
    bound: *mut Value,
) -> Option<Value> {
    unsafe {
        let index = encoded >> SHIFT;
        let kind = encoded & KIND;
        if kind != FIELD {
            let value = class_attribute(layout, index, kind, receiver, instance, owner, bound);
            return (!value.is_null()).then_some(value);
        }
        if instance {
            if matches!(&(*receiver).data, Data::NativeInstance { .. }) {
                let native = layout.native.as_ref().expect("native allocation layout");
                let value = (native.getter)(native.globals, receiver, index as i64);
                return (!value.is_null()).then_some(value);
            }
            let Data::Instance { fields, .. } = &(*receiver).data else {
                unreachable!();
            };
            let value = fields[index];
            if !value.is_null() {
                return Some(object::retain(value));
            }
        }
        // An unset field, or a field name read through the class, falls back
        // to a class attribute of the same name.
        let attribute = name(attribute)?;
        let index = *layout.attribute_lookup.get(attribute)?;
        let kind = attribute_kind(layout.attributes[index].1);
        let value = class_attribute(layout, index, kind, receiver, instance, owner, bound);
        (!value.is_null()).then_some(value)
    }
}

unsafe fn missing(layout: Option<&ClassLayout>, instance: bool, attribute: &str) -> Value {
    let message = match layout {
        Some(layout) if instance => {
            format!("'{}' object has no attribute '{attribute}'", layout.name)
        }
        Some(layout) => format!("type object '{}' has no attribute '{attribute}'", layout.name),
        None => format!("object has no attribute '{attribute}'"),
    };
    object::error("AttributeError", &message);
    std::ptr::null_mut()
}

unsafe fn text(value: &str) -> Value {
    unsafe { object::new_value(Data::Text(Text::from_rust_str(value))) }
}

/// Call class attribute `index` as a method of `receiver`, as Python's
/// implicit special-method calls do: through the class, with the descriptor
/// protocol applied to anything that is not a plain function.
unsafe fn call_attribute(
    layout: &ClassLayout,
    index: usize,
    receiver: Value,
    owner: Value,
    args: &[Value],
) -> Value {
    unsafe {
        let value = layout.attributes[index].1;
        if attribute_kind(value) == METHOD {
            return object::call_with_receiver(value, receiver, args);
        }
        let callable = bind_class_attribute(value, receiver, owner);
        if callable.is_null() {
            return callable;
        }
        let result = object::call(callable, args);
        object::release(callable);
        result
    }
}

/// Call an instance's `__getattribute__` or `__getattr__` hook with the name.
unsafe fn call_hook(
    layout: &ClassLayout,
    index: usize,
    receiver: Value,
    owner: Value,
    attribute: *const c_char,
) -> Value {
    unsafe {
        let Some(attribute) = name(attribute) else {
            return std::ptr::null_mut();
        };
        let attribute = text(attribute);
        let result = call_attribute(layout, index, receiver, owner, &[attribute]);
        object::release(attribute);
        result
    }
}

/// Resolve an attribute before arguments are evaluated. The returned callable
/// and optional receiver each carry ownership independent of class/slot changes.
unsafe fn resolve(
    receiver: Value,
    attribute: *const c_char,
    cache: *mut AttrCache,
    bound: *mut Value,
) -> Value {
    unsafe {
        *bound = std::ptr::null_mut();
        if object::has_error() {
            return std::ptr::null_mut();
        }
        if let Some((layout, instance, owner)) = layout(receiver) {
            if instance {
                if let Some(index) = layout.getattribute {
                    return call_hook(layout, index, receiver, owner, attribute);
                }
            }
            let encoded = match cached(layout, cache) {
                Some(encoded) => Some(encoded),
                None => {
                    let Some(attribute) = name(attribute) else {
                        return std::ptr::null_mut();
                    };
                    encoded(layout, attribute, cache)
                }
            };
            if let Some(encoded) = encoded {
                if let Some(value) =
                    resolved(receiver, layout, instance, owner, encoded, attribute, bound)
                {
                    return value;
                }
                if object::has_error() {
                    return std::ptr::null_mut();
                }
            }
            if instance {
                if let Some(index) = layout.getattr {
                    return call_hook(layout, index, receiver, owner, attribute);
                }
            }
            let Some(attribute) = name(attribute) else {
                return std::ptr::null_mut();
            };
            return missing(Some(layout), instance, attribute);
        }
        let Some(attribute) = name(attribute) else {
            return std::ptr::null_mut();
        };
        if !receiver.is_null() && !object::is_fixnum(receiver) {
            let (attributes, instance) = match &(*receiver).data {
                Data::Namespace(_) => (receiver, false),
                Data::Type { attributes, .. } => (*attributes, false),
                Data::Instance { ty, .. } => match &(**ty).data {
                    Data::Type { attributes, .. } => (*attributes, true),
                    _ => unreachable!(),
                },
                _ => {
                    let method = crate::sequences::binding(receiver, attribute);
                    if !method.is_null() {
                        *bound = object::retain(receiver);
                        return object::retain(method);
                    }
                    return missing(None, false, attribute);
                }
            };
            let method = object::lookup(attributes, attribute);
            if method.is_null() {
                return missing(None, false, attribute);
            }
            if object::is_fixnum(method) {
                return object::retain(method);
            }
            if matches!(&(*method).data, Data::Function { .. }) && instance {
                *bound = object::retain(receiver);
            }
            return object::retain(method);
        }
        missing(None, false, attribute)
    }
}

pub unsafe fn construct(class: Value, args: &[Value]) -> Value {
    unsafe {
        let Data::Class(layout) = &(*class).data else {
            unreachable!();
        };
        if let Some(native) = &layout.native {
            return (native.constructor)(native.globals, class, args.len() as isize, args.as_ptr());
        }
        let mut fields = Vec::new();
        if fields.try_reserve_exact(layout.fields.len()).is_err() {
            object::error("MemoryError", "instance slot allocation failed");
            return std::ptr::null_mut();
        }
        fields.resize(layout.fields.len(), std::ptr::null_mut());
        let instance = object::new_value(Data::Instance {
            ty: object::retain(class),
            fields: fields.into_boxed_slice(),
        });
        if let Some(index) = layout.initializer {
            let result = call_attribute(layout, index, instance, class, args);
            if result.is_null() {
                object::release(instance);
                return result;
            }
            let valid = result == object::none_value();
            object::release(result);
            if !valid {
                object::error("TypeError", "__init__() should return None");
                object::release(instance);
                return std::ptr::null_mut();
            }
        } else if !args.is_empty() {
            object::error(
                "TypeError",
                &format!("{}() takes no arguments", layout.name),
            );
            object::release(instance);
            return std::ptr::null_mut();
        }
        instance
    }
}

/// Special lookup bypasses instance slots, matching the language's method protocol.
pub unsafe fn special_binding(receiver: Value, attribute: &str) -> Value {
    unsafe {
        let Some((layout, _, _)) = layout(receiver) else {
            return std::ptr::null_mut();
        };
        layout.method(attribute)
    }
}

/// An owned callable for a special method of an instance, with the receiver
/// bound, or NULL when the class lacks it.
pub unsafe fn special_bound(receiver: Value, attribute: &str) -> Value {
    unsafe {
        let Some((layout, true, owner)) = layout(receiver) else {
            return std::ptr::null_mut();
        };
        let value = layout.method(attribute);
        if value.is_null() {
            return value;
        }
        bind_class_attribute(value, receiver, owner)
    }
}

pub unsafe fn special_method(receiver: Value, attribute: &str, args: &[Value]) -> Value {
    unsafe {
        let Some((layout, _, owner)) = layout(receiver) else {
            return std::ptr::null_mut();
        };
        let index = match attribute {
            "__bool__" => layout.boolean,
            "__len__" => layout.length,
            "__call__" => layout.callable,
            _ => layout.attribute_lookup.get(attribute).copied(),
        };
        let Some(index) = index else {
            return std::ptr::null_mut();
        };
        call_attribute(layout, index, receiver, owner, args)
    }
}

unsafe fn create_class(
    class_name: *const c_char,
    base: Value,
    field_count: isize,
    field_names: *const *const c_char,
    namespace: Value,
    native: Option<NativeLayout>,
) -> Value {
    unsafe {
        if object::has_error() {
            return std::ptr::null_mut();
        }
        if field_count < 0
            || (field_count > 0 && field_names.is_null())
            || namespace.is_null()
            || object::is_fixnum(namespace)
        {
            object::error("SystemError", "invalid class metadata");
            return std::ptr::null_mut();
        }
        let Data::Namespace(bindings) = &(*namespace).data else {
            object::error("SystemError", "class namespace storage is required");
            return std::ptr::null_mut();
        };
        let Some(class_name) = name(class_name) else {
            return std::ptr::null_mut();
        };
        let inherited = if base.is_null() {
            None
        } else {
            if object::is_fixnum(base) || !matches!(&(*base).data, Data::Class(_)) {
                object::error("TypeError", "base must be a fixed-layout Mule class");
                return std::ptr::null_mut();
            }
            let Data::Class(layout) = &(*base).data else {
                unreachable!();
            };
            if layout.native.is_some() {
                // This bootstrap allocator only builds reference-slot layouts;
                // it cannot inherit a compiler-defined raw-payload allocation.
                object::error(
                    "TypeError",
                    "base has an incompatible native allocation layout",
                );
                return std::ptr::null_mut();
            }
            Some(layout)
        };
        // Validate all names before taking ownership, making every rejection atomic.
        let mut fields = inherited.map_or_else(Vec::new, |layout| layout.fields.clone());
        let mut field_lookup: HashMap<String, usize> = inherited
            .map_or_else(HashMap::new, |layout| layout.field_lookup.clone());
        for index in 0..field_count as usize {
            let Some(attribute) = name(*field_names.add(index)) else {
                return std::ptr::null_mut();
            };
            if !fields.iter().any(|field| field == attribute) {
                field_lookup.insert(attribute.into(), fields.len());
                fields.push(attribute.into());
            }
        }
        let mut attributes: Vec<(String, Value)> = inherited.map_or_else(Vec::new, |layout| {
            layout
                .attributes
                .iter()
                .map(|(n, value)| (n.clone(), object::retain(*value)))
                .collect()
        });
        let mut attribute_lookup: HashMap<String, usize> = attributes
            .iter()
            .enumerate()
            .map(|(i, (n, _))| (n.clone(), i))
            .collect();
        for (attribute, value) in bindings {
            if let Some(index) = attribute_lookup.get(attribute) {
                let old = attributes[*index].1;
                attributes[*index].1 = object::retain(*value);
                object::release(old);
            } else {
                attribute_lookup.insert(attribute.clone(), attributes.len());
                attributes.push((attribute.clone(), object::retain(*value)));
            }
        }
        // A data descriptor takes precedence over an instance field of the
        // same name, so the field's slot is never reached.
        if native.is_none() {
            field_lookup.retain(|field, _| {
                attribute_lookup
                    .get(field)
                    .is_none_or(|index| !is_data_descriptor(attributes[*index].1))
            });
        }
        let id = NEXT_CLASS_ID.fetch_add(1, Ordering::Relaxed);
        assert_ne!(id, 0, "class identity space exhausted");
        let special = |name: &str| attribute_lookup.get(name).copied();
        let initializer = special("__init__");
        let boolean = special("__bool__");
        let length = special("__len__");
        let callable = special("__call__");
        let getattribute = special("__getattribute__");
        let getattr = special("__getattr__");
        object::new_value(Data::Class(Box::new(ClassLayout {
            id,
            name: class_name.into(),
            base: object::retain(base),
            fields,
            attributes,
            field_lookup,
            attribute_lookup,
            initializer,
            boolean,
            length,
            callable,
            getattribute,
            getattr,
            native,
        })))
    }
}

/// Read a class body's name: its namespace, then the module globals and
/// builtins (Python's `LOAD_NAME`).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_class_load(
    namespace: Value,
    globals: Value,
    name: *const c_char,
) -> Value {
    unsafe {
        let Some(attribute) = object::name_from_c(name) else {
            return std::ptr::null_mut();
        };
        let value = object::lookup(namespace, attribute);
        if !value.is_null() {
            return object::retain(value);
        }
        if object::has_error() {
            return std::ptr::null_mut();
        }
        object::mule_load_global(globals, name)
    }
}

/// Delete a class body's name from its namespace (Python's `DELETE_NAME`).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_class_delete(
    namespace: Value,
    name: *const c_char,
    missing_ok: i32,
) -> i32 {
    unsafe {
        let Some(attribute) = object::name_from_c(name) else {
            return -1;
        };
        if namespace.is_null() || object::is_fixnum(namespace) {
            object::error("SystemError", "class namespace storage is required");
            return -1;
        }
        let Data::Namespace(bindings) = &mut (*namespace).data else {
            object::error("SystemError", "class namespace storage is required");
            return -1;
        };
        match bindings.iter().position(|(key, _)| key == attribute) {
            Some(index) => {
                let (_, value) = bindings.remove(index);
                object::release(value);
                0
            }
            None if missing_ok != 0 => 0,
            None => {
                object::error("NameError", &format!("name '{attribute}' is not defined"));
                -1
            }
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_resolve_method(
    receiver: Value,
    attribute: *const c_char,
    cache: *mut AttrCache,
    bound: *mut Value,
) -> Value {
    unsafe {
        if bound.is_null() {
            object::error("SystemError", "NULL receiver output");
            return std::ptr::null_mut();
        }
        *bound = std::ptr::null_mut();
        resolve(receiver, attribute, cache, bound)
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_call_resolved(
    callable: Value,
    receiver: Value,
    count: isize,
    args: *const Value,
) -> Value {
    unsafe {
        if count < 0 || (count > 0 && args.is_null()) {
            object::error("SystemError", "invalid method argument buffer");
            return std::ptr::null_mut();
        }
        let args = if count == 0 {
            &[]
        } else {
            std::slice::from_raw_parts(args, count as usize)
        };
        if receiver.is_null() {
            object::call(callable, args)
        } else {
            object::call_with_receiver(callable, receiver, args)
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_get_attr_cached(
    receiver: Value,
    attribute: *const c_char,
    cache: *mut AttrCache,
) -> Value {
    unsafe {
        let mut bound = std::ptr::null_mut();
        let method = mule_resolve_method(receiver, attribute, cache, &mut bound);
        if bound.is_null() {
            return method;
        }
        // Resolution owns both references; transfer them into the bound object.
        object::new_value(Data::BoundMethod {
            receiver: bound,
            method,
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_get_attr(receiver: Value, attribute: *const c_char) -> Value {
    unsafe { mule_get_attr_cached(receiver, attribute, std::ptr::null_mut()) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_set_attr_cached(
    receiver: Value,
    attribute: *const c_char,
    value: Value,
    cache: *mut AttrCache,
) -> i32 {
    unsafe {
        if object::has_error() {
            return -1;
        }
        if value.is_null() {
            object::error("SystemError", "NULL slot value");
            return -1;
        }
        if let Some((layout, instance, _)) = layout(receiver) {
            if !instance {
                object::error("TypeError", "compiled classes are immutable");
                return -1;
            }
            let encoded = match cached(layout, cache) {
                Some(encoded) => Some(encoded),
                None => {
                    let Some(attribute) = name(attribute) else {
                        return -1;
                    };
                    encoded(layout, attribute, cache)
                }
            };
            let index = encoded.map(|encoded| encoded >> SHIFT);
            match encoded.map(|encoded| encoded & KIND) {
                Some(FIELD) => {
                    let index = index.unwrap();
                    if matches!(&(*receiver).data, Data::NativeInstance { .. }) {
                        let native = layout.native.as_ref().expect("native allocation layout");
                        return (native.setter)(native.globals, receiver, index as i64, value);
                    }
                    let Data::Instance { fields, .. } = &mut (*receiver).data else {
                        unreachable!();
                    };
                    let old = fields[index];
                    fields[index] = object::retain(value);
                    object::release(old);
                    return 0;
                }
                Some(DATA_DESCRIPTOR) => {
                    let descriptor = layout.attributes[index.unwrap()].1;
                    let result = special_method(descriptor, "__set__", &[receiver, value]);
                    if result.is_null() {
                        if !object::has_error() {
                            object::error("AttributeError", "read-only attribute");
                        }
                        return -1;
                    }
                    object::release(result);
                    return 0;
                }
                _ => {
                    // Without per-instance dictionaries, only fixed fields
                    // and data descriptors accept instance assignment.
                    let Some(attribute) = name(attribute) else {
                        return -1;
                    };
                    missing(Some(layout), true, attribute);
                    return -1;
                }
            }
        }
        let Some(attribute) = name(attribute) else {
            return -1;
        };
        if !receiver.is_null() && !object::is_fixnum(receiver) {
            if let Data::Namespace(_) = &(*receiver).data {
                return if object::bind(receiver, attribute, value) { 0 } else { -1 };
            }
            if let Data::Type { attributes, .. } = &(*receiver).data {
                return if object::bind(*attributes, attribute, value) {
                    0
                } else {
                    -1
                };
            }
        }
        missing(None, false, attribute);
        -1
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_set_attr(
    receiver: Value,
    attribute: *const c_char,
    value: Value,
) -> i32 {
    unsafe { mule_set_attr_cached(receiver, attribute, value, std::ptr::null_mut()) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_call_method_cached(
    receiver: Value,
    attribute: *const c_char,
    count: isize,
    args: *const Value,
    cache: *mut AttrCache,
) -> Value {
    unsafe {
        let mut bound = std::ptr::null_mut();
        let method = mule_resolve_method(receiver, attribute, cache, &mut bound);
        if method.is_null() {
            return method;
        }
        let result = mule_call_resolved(method, bound, count, args);
        object::release(method);
        object::release(bound);
        result
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_call_method(
    receiver: Value,
    attribute: *const c_char,
    count: isize,
    args: *const Value,
) -> Value {
    unsafe { mule_call_method_cached(receiver, attribute, count, args, std::ptr::null_mut()) }
}

fn fail(kind: &str, message: &str) -> Value {
    object::error(kind, message);
    std::ptr::null_mut()
}

unsafe fn operands<const N: usize>(args: Value) -> Option<[Value; N]> {
    unsafe {
        if object::has_error() {
            return None;
        }
        let count = object::argument_count(args)?;
        if count != N || crate::arguments::keyword_count(args) != 0 {
            object::error("TypeError", &format!("expected {N} positional arguments"));
            return None;
        }
        let mut values = [std::ptr::null_mut(); N];
        for (index, value) in values.iter_mut().enumerate() {
            *value = object::argument(args, index)?;
        }
        Some(values)
    }
}

unsafe fn class_layout<'a>(value: Value) -> Option<&'a ClassLayout> {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::Class(layout) = &(*value).data {
                return Some(layout);
            }
        }
        object::error("TypeError", "a compiled class is required");
        None
    }
}

unsafe fn text_argument(value: Value) -> Option<String> {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::Text(text) = &(*value).data {
                return text.to_rust_string();
            }
        }
        object::error("TypeError", "attribute name must be a string");
        None
    }
}

unsafe extern "C" fn is_class(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let class = !value.is_null()
            && !object::is_fixnum(value)
            && matches!(&(*value).data, Data::Class(_));
        object::bool_value(class)
    }
}

unsafe extern "C" fn class_of(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands(args) else {
            return std::ptr::null_mut();
        };
        match layout(value) {
            Some((_, true, owner)) => object::retain(owner),
            _ => fail("TypeError", "an instance of a compiled class is required"),
        }
    }
}

unsafe extern "C" fn class_base(_: Value, args: Value) -> Value {
    unsafe {
        let Some([class]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Some(layout) = class_layout(class) else {
            return std::ptr::null_mut();
        };
        if layout.base.is_null() {
            object::none_value()
        } else {
            object::retain(layout.base)
        }
    }
}

unsafe extern "C" fn class_lookup(_: Value, args: Value) -> Value {
    unsafe {
        let Some([class, attribute]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Some(layout) = class_layout(class) else {
            return std::ptr::null_mut();
        };
        let Some(attribute) = text_argument(attribute) else {
            return std::ptr::null_mut();
        };
        let value = layout.method(&attribute);
        if value.is_null() {
            return missing(Some(layout), false, &attribute);
        }
        object::retain(value)
    }
}

unsafe extern "C" fn bind_attribute(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value, instance, owner]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let instance = if instance == object::none_value() {
            std::ptr::null_mut()
        } else {
            instance
        };
        bind_class_attribute(value, instance, owner)
    }
}

unsafe extern "C" fn bound_method(_: Value, args: Value) -> Value {
    unsafe {
        let Some([method, receiver]) = operands(args) else {
            return std::ptr::null_mut();
        };
        object::new_value(Data::BoundMethod {
            receiver: object::retain(receiver),
            method: object::retain(method),
        })
    }
}

unsafe extern "C" fn instance_field(_: Value, args: Value) -> Value {
    unsafe {
        let Some([instance, attribute]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Some(attribute) = text_argument(attribute) else {
            return std::ptr::null_mut();
        };
        if let Some((layout, true, _)) = layout(instance) {
            if let Data::Instance { fields, .. } = &(*instance).data {
                if let Some(index) = layout.field_lookup.get(&attribute) {
                    let value = fields[*index];
                    if !value.is_null() {
                        return object::retain(value);
                    }
                }
                return missing(Some(layout), true, &attribute);
            }
        }
        fail("TypeError", "an instance with reference fields is required")
    }
}

/// Bind the class storage operations compiled descriptor and `super` code
/// uses into the compiler-owned native namespace.
pub unsafe fn populate(globals: Value) -> bool {
    unsafe {
        if object::has_error() {
            return false;
        }
        let exports: [(&str, object::Native); 7] = [
            ("is_class", is_class),
            ("class_of", class_of),
            ("class_base", class_base),
            ("class_lookup", class_lookup),
            ("bind_attribute", bind_attribute),
            ("bound_method", bound_method),
            ("instance_field", instance_field),
        ];
        for (name, entry) in exports {
            let function = object::function(std::ptr::null_mut(), entry, name);
            let bound = object::bind(globals, name, function);
            object::release(function);
            if !bound {
                return false;
            }
        }
        true
    }
}
