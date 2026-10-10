//! Bootstrap fixed-layout class metadata and compatibility dispatch.
//! Native raw-slot protocols are compiler-generated; Rust forwards to their hooks.
use crate::object::{self, Data, Value};
use std::collections::HashMap;
use std::ffi::{CStr, c_char, c_void};
use std::sync::atomic::{AtomicUsize, Ordering};

static NEXT_CLASS_ID: AtomicUsize = AtomicUsize::new(1);
pub struct ClassLayout {
    pub id: usize,
    pub name: String,
    pub base: Value,
    pub fields: Vec<String>,
    pub methods: Vec<(String, Value)>,
    field_lookup: HashMap<String, usize>,
    method_lookup: HashMap<String, usize>,
    initializer: Option<usize>,
    boolean: Option<usize>,
    length: Option<usize>,
    callable: Option<usize>,
    pub(crate) native: Option<NativeLayout>,
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
    method_count: isize,
    method_names: *const *const c_char,
    methods: *const Value,
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
            method_count,
            method_names,
            methods,
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
    method_count: isize,
    method_names: *const *const c_char,
    methods: *const Value,
) -> Value {
    unsafe {
        create_class(
            class_name,
            base,
            field_count,
            field_names,
            method_count,
            method_names,
            methods,
            None,
        )
    }
}

pub type NativeConstructor = unsafe extern "C" fn(Value, Value, isize, *const Value) -> Value;
pub type NativeGetter = unsafe extern "C" fn(Value, Value, i64) -> Value;
pub type NativeSetter = unsafe extern "C" fn(Value, Value, i64, Value) -> i32;

/// LLVM reserves two zero-initialized machine words per attribute site.
/// IDs never repeat, so an expired class cannot make a cache entry valid again.
#[repr(C)]
#[derive(Default)]
pub struct AttrCache {
    pub class_id: usize,
    pub encoded: usize,
}
const FIELD: usize = 1;
const METHOD: usize = 2;
const STATIC: usize = 3;

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

unsafe fn layout<'a>(receiver: Value) -> Option<(&'a ClassLayout, bool)> {
    unsafe {
        if receiver.is_null() || object::is_fixnum(receiver) {
            return None;
        }
        match &(*receiver).data {
            Data::Class(layout) => Some((layout, false)),
            Data::Instance { ty, .. } | Data::NativeInstance { ty, .. } => match &(**ty).data {
                Data::Class(layout) => Some((layout, true)),
                _ => None,
            },
            _ => None,
        }
    }
}

unsafe fn encoded(
    layout: &ClassLayout,
    instance: bool,
    attribute: &str,
    cache: *mut AttrCache,
) -> Option<usize> {
    unsafe {
        let resolved = if instance {
            layout
                .field_lookup
                .get(attribute)
                .map(|index| (index << 2) | FIELD)
        } else {
            None
        }
        .or_else(|| {
            layout.method_lookup.get(attribute).map(|index| {
                let method = layout.methods[*index].1;
                let kind = if matches!(&(*method).data, Data::StaticMethod(_)) {
                    STATIC
                } else {
                    METHOD
                };
                (index << 2) | kind
            })
        });
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
unsafe fn cached(layout: &ClassLayout, instance: bool, cache: *mut AttrCache) -> Option<usize> {
    unsafe {
        if !cache.is_null() && (*cache).class_id == layout.id {
            let encoded = (*cache).encoded;
            if instance || encoded & 3 != FIELD {
                return Some(encoded);
            }
        }
        None
    }
}

#[inline]
unsafe fn resolved(
    receiver: Value,
    layout: &ClassLayout,
    instance: bool,
    encoded: usize,
    bound: *mut Value,
) -> Option<Value> {
    unsafe {
        let index = encoded >> 2;
        if encoded & 3 == FIELD {
            if matches!(&(*receiver).data, Data::NativeInstance { .. }) {
                let native = layout.native.as_ref().expect("native allocation layout");
                let value = (native.getter)(native.globals, receiver, index as i64);
                return (!value.is_null()).then_some(value);
            }
            let Data::Instance { fields, .. } = &(*receiver).data else {
                unreachable!();
            };
            let value = fields[index];
            return if value.is_null() {
                None
            } else {
                Some(object::retain(value))
            };
        }
        let method = layout.methods[index].1;
        if encoded & 3 == STATIC {
            let Data::StaticMethod(method) = &(*method).data else {
                unreachable!();
            };
            return Some(object::retain(*method));
        }
        if instance {
            *bound = object::retain(receiver);
        }
        Some(object::retain(method))
    }
}

unsafe fn missing(attribute: &str) -> Value {
    object::error(
        "AttributeError",
        &format!("object has no attribute '{attribute}'"),
    );
    std::ptr::null_mut()
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
        if let Some((layout, instance)) = layout(receiver) {
            if let Some(encoded) = cached(layout, instance, cache) {
                if let Some(value) = resolved(receiver, layout, instance, encoded, bound) {
                    return value;
                }
                let Some(attribute) = name(attribute) else {
                    return std::ptr::null_mut();
                };
                return missing(attribute);
            }
            let Some(attribute) = name(attribute) else {
                return std::ptr::null_mut();
            };
            let Some(encoded) = encoded(layout, instance, attribute, cache) else {
                return missing(attribute);
            };
            return resolved(receiver, layout, instance, encoded, bound)
                .unwrap_or_else(|| missing(attribute));
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
                _ => return missing(attribute),
            };
            let method = object::lookup(attributes, attribute);
            if method.is_null() {
                return missing(attribute);
            }
            if object::is_fixnum(method) {
                return object::retain(method);
            }
            match &(*method).data {
                Data::StaticMethod(method) => return object::retain(*method),
                Data::Function { .. } if instance => *bound = object::retain(receiver),
                _ => (),
            }
            return object::retain(method);
        }
        missing(attribute)
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
            let method = layout.methods[index].1;
            let result = if let Data::StaticMethod(method) = &(*method).data {
                object::call(*method, args)
            } else {
                object::call_with_receiver(method, instance, args)
            };
            if result.is_null() {
                object::release(instance);
                return result;
            }
            let valid = result == object::none_value();
            object::release(result);
            if !valid {
                object::error("TypeError", "__init__ must return None");
                object::release(instance);
                return std::ptr::null_mut();
            }
        } else if !args.is_empty() {
            object::error("TypeError", "instance constructor takes no arguments");
            object::release(instance);
            return std::ptr::null_mut();
        }
        instance
    }
}

/// Special lookup bypasses instance slots, matching the language's method protocol.
pub unsafe fn special_method(receiver: Value, attribute: &str, args: &[Value]) -> Value {
    unsafe {
        let Some((layout, _)) = layout(receiver) else {
            return std::ptr::null_mut();
        };
        let index = match attribute {
            "__bool__" => layout.boolean,
            "__len__" => layout.length,
            "__call__" => layout.callable,
            _ => layout.method_lookup.get(attribute).copied(),
        };
        let Some(index) = index else {
            return std::ptr::null_mut();
        };
        let method = layout.methods[index].1;
        if let Data::StaticMethod(method) = &(*method).data {
            object::call(*method, args)
        } else {
            object::call_with_receiver(method, receiver, args)
        }
    }
}

unsafe fn create_class(
    class_name: *const c_char,
    base: Value,
    field_count: isize,
    field_names: *const *const c_char,
    method_count: isize,
    method_names: *const *const c_char,
    methods: *const Value,
    native: Option<NativeLayout>,
) -> Value {
    unsafe {
        if object::has_error() {
            return std::ptr::null_mut();
        }
        if field_count < 0
            || method_count < 0
            || (field_count > 0 && field_names.is_null())
            || (method_count > 0 && (method_names.is_null() || methods.is_null()))
        {
            object::error("SystemError", "invalid class metadata");
            return std::ptr::null_mut();
        }
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
        let mut field_lookup: HashMap<String, usize> = fields
            .iter()
            .enumerate()
            .map(|(i, n)| (n.clone(), i))
            .collect();
        let mut own_methods = Vec::new();
        for index in 0..field_count as usize {
            let Some(attribute) = name(*field_names.add(index)) else {
                return std::ptr::null_mut();
            };
            if !field_lookup.contains_key(attribute) {
                field_lookup.insert(attribute.into(), fields.len());
                fields.push(attribute.into());
            }
        }
        for index in 0..method_count as usize {
            let Some(attribute) = name(*method_names.add(index)) else {
                return std::ptr::null_mut();
            };
            let method = *methods.add(index);
            if method.is_null()
                || object::is_fixnum(method)
                || !matches!(
                    &(*method).data,
                    Data::Function { .. } | Data::StaticMethod(_)
                )
            {
                object::error("TypeError", "class method must be a native function");
                return std::ptr::null_mut();
            }
            own_methods.push((attribute.to_owned(), method));
        }
        if own_methods
            .iter()
            .any(|(n, _)| field_lookup.contains_key(n))
            || inherited.is_some_and(|layout| {
                layout
                    .methods
                    .iter()
                    .any(|(n, _)| field_lookup.contains_key(n))
            })
        {
            object::error("TypeError", "fixed-layout fields cannot shadow methods");
            return std::ptr::null_mut();
        }
        let mut inherited_methods = inherited.map_or_else(Vec::new, |layout| {
            layout
                .methods
                .iter()
                .map(|(n, m)| (n.clone(), object::retain(*m)))
                .collect()
        });
        let mut method_lookup: HashMap<String, usize> = inherited_methods
            .iter()
            .enumerate()
            .map(|(i, (n, _))| (n.clone(), i))
            .collect();
        for (attribute, method) in own_methods {
            if let Some(index) = method_lookup.get(&attribute) {
                let old = inherited_methods[*index].1;
                inherited_methods[*index].1 = object::retain(method);
                object::release(old);
            } else {
                method_lookup.insert(attribute.clone(), inherited_methods.len());
                inherited_methods.push((attribute, object::retain(method)));
            }
        }
        let id = NEXT_CLASS_ID.fetch_add(1, Ordering::Relaxed);
        assert_ne!(id, 0, "class identity space exhausted");
        let initializer = method_lookup.get("__init__").copied();
        let boolean = method_lookup.get("__bool__").copied();
        let length = method_lookup.get("__len__").copied();
        let callable = method_lookup.get("__call__").copied();
        object::new_value(Data::Class(Box::new(ClassLayout {
            id,
            name: class_name.into(),
            base: object::retain(base),
            fields,
            methods: inherited_methods,
            field_lookup,
            method_lookup,
            initializer,
            boolean,
            length,
            callable,
            native,
        })))
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
        if let Some((layout, instance)) = layout(receiver) {
            if !instance {
                object::error("TypeError", "compiled classes are immutable");
                return -1;
            }
            let encoded = if let Some(encoded) = cached(layout, instance, cache) {
                encoded
            } else {
                let Some(attribute) = name(attribute) else {
                    return -1;
                };
                let Some(encoded) = encoded(layout, instance, attribute, cache) else {
                    missing(attribute);
                    return -1;
                };
                encoded
            };
            if encoded & 3 != FIELD {
                object::error("AttributeError", "fixed-layout methods cannot be replaced");
                return -1;
            }
            if matches!(&(*receiver).data, Data::NativeInstance { .. }) {
                let native = layout.native.as_ref().expect("native allocation layout");
                return (native.setter)(native.globals, receiver, (encoded >> 2) as i64, value);
            }
            let Data::Instance { fields, .. } = &mut (*receiver).data else {
                unreachable!();
            };
            let old = fields[encoded >> 2];
            fields[encoded >> 2] = object::retain(value);
            object::release(old);
            return 0;
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
        missing(attribute);
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
