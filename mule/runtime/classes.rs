//! Bootstrap fixed-layout class storage and attribute dispatch.
//!
//! A class is built from the namespace its body filled: every binding becomes
//! one of its own attributes, and lookup continues through its base. Assigning
//! a class attribute afterwards changes that table and advances a global
//! epoch that invalidates every attribute cache, as CPython's type attribute
//! cache is invalidated by type version tags. Instances store the
//! fixed fields the compiler found assigned through method receivers. Lookup
//! follows Python's order: a data descriptor on the class, then an instance
//! field, then the class attribute (binding functions and calling `__get__`),
//! then `__getattr__`. Descriptor behavior itself (`property`, `classmethod`,
//! `staticmethod`, `super`) is compiled Mule Python; this file only applies
//! the protocol. Native raw-slot protocols are compiler-generated; Rust
//! forwards to their hooks.
use crate::object::{self, Data, Value};
use crate::text::Text;
use std::cell::Cell;
use std::collections::HashMap;
use std::ffi::{CStr, c_char, c_void};
use std::sync::atomic::{AtomicUsize, Ordering};

static NEXT_CLASS_ID: AtomicUsize = AtomicUsize::new(1);
/// Advanced by every class attribute assignment; caches record the epoch.
static ATTRIBUTE_EPOCH: AtomicUsize = AtomicUsize::new(1);

/// Compiled primary/fallback lookup. Arguments are borrowed, the result owned.
/// Registered after sequence and print initialization; sequence shutdown clears
/// it before releasing the protocol globals it accesses.
pub type AttributeLookup = unsafe extern "C" fn(Value, Value) -> Value;
thread_local! {
    static ATTRIBUTE_LOOKUP: Cell<Option<AttributeLookup>> = const { Cell::new(None) };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_set_attribute_lookup(entry: *mut c_void) {
    ATTRIBUTE_LOOKUP.with(|current| {
        current.set(if entry.is_null() {
            None
        } else {
            Some(unsafe { std::mem::transmute::<*mut c_void, AttributeLookup>(entry) })
        })
    });
}

pub struct ClassLayout {
    pub id: usize,
    pub name: String,
    pub base: Value,
    /// Every direct base, retained. `base` is the first of these.
    pub bases: Vec<Value>,
    /// Borrowed direct children. A child unlinks before releasing its bases.
    subclasses: Vec<Value>,
    /// Ancestors in C3 method-resolution order, excluding this class.
    /// Not retained: each is reachable through the direct bases' chains.
    pub mro: Vec<Value>,
    /// The class's own class: `type` or a metaclass derived from it.
    pub metaclass: Value,
    pub fields: Vec<String>,
    /// The class's own attributes, in definition order.
    pub attributes: Vec<(String, Value)>,
    field_lookup: HashMap<String, usize>,
    attribute_lookup: HashMap<String, usize>,
    /// Whether instances may hold attributes beyond their fields (a
    /// `__dict__`): false for `object` itself, native layouts, and classes
    /// whose `__slots__` omit `__dict__` all the way up their bases.
    dict_allowed: bool,
    pub(crate) native: Option<NativeLayout>,
}

impl ClassLayout {
    /// Borrow an attribute through the complete MRO, including object defaults.
    pub fn attribute(&self, name: &str) -> Value {
        self.find(name).map_or(std::ptr::null_mut(), |(owner, _, index)| owner.attributes[index].1)
    }
    pub(crate) fn remove_subclass(&mut self, child: Value) {
        self.subclasses.retain(|entry| *entry != child);
    }

    pub fn allows_instance_dictionary(&self) -> bool {
        self.dict_allowed
    }

    /// The class defining `name` (this class or a base), how many bases up
    /// it is, and the attribute's index in that class's own table.
    fn find(&self, name: &str) -> Option<(&ClassLayout, usize, usize)> {
        if let Some(index) = self.attribute_lookup.get(name) {
            return Some((self, 0, *index));
        }
        for (position, ancestor) in self.mro.iter().enumerate() {
            let layout = unsafe { layout_of_class(*ancestor) }?;
            if let Some(index) = layout.attribute_lookup.get(name) {
                return Some((layout, position + 1, *index));
            }
        }
        None
    }

    /// The raw class attribute, borrowed, or NULL when absent.
    pub fn method(&self, name: &str) -> Value {
        self.find(name)
            .map_or(std::ptr::null_mut(), |(layout, _, index)| layout.attributes[index].1)
    }

    /// The special method the runtime invokes implicitly for `name`, or NULL
    /// when an inherited object default has an equivalent bootstrap primitive.
    /// Inequality must retain its compiled default when equality is overridden:
    /// it delegates to that descriptor rather than comparing identities.
    pub fn special(&self, name: &str) -> Value {
        let Some((owner, _, index)) = self.find(name) else {
            return std::ptr::null_mut();
        };
        let object = builtin_type(Builtin::Object);
        if !object.is_null() && std::ptr::eq(owner, unsafe { layout_of_class(object) }.unwrap()) {
            let overridden_equality = self.find("__eq__")
                .is_some_and(|(equality_owner, _, _)| !std::ptr::eq(equality_owner, owner));
            if name != "__ne__" || !overridden_equality {
                return std::ptr::null_mut();
            }
        }
        owner.attributes[index].1
    }

    /// The class `depth` bases up from this one.
    fn ancestor(&self, depth: usize) -> &ClassLayout {
        if depth == 0 {
            return self;
        }
        unsafe { layout_of_class(self.mro[depth - 1]) }.expect("cached MRO")
    }
}

/// The classes of values that are not compiled-class instances, registered
/// by the compiled builtins. `object` and `type` are the defaults for every
/// class's base and metaclass.
#[derive(Clone, Copy)]
pub enum Builtin {
    Object = 0,
    Type,
    NoneType,
    Bool,
    Int,
    Float,
    Str,
    Bytes,
    List,
    Tuple,
    Function,
    BuiltinFunction,
    Method,
    Module,
    Traceback,
    Frame,
}
const BUILTIN_COUNT: usize = 16;
const BUILTIN_NAMES: [&str; BUILTIN_COUNT] = [
    "object",
    "type",
    "NoneType",
    "bool",
    "int",
    "float",
    "str",
    "bytes",
    "list",
    "tuple",
    "function",
    "builtin_function_or_method",
    "method",
    "module",
    "traceback",
    "frame",
];

thread_local! {
    static BUILTIN_TYPES: std::cell::RefCell<[Value; BUILTIN_COUNT]> =
        const { std::cell::RefCell::new([std::ptr::null_mut(); BUILTIN_COUNT]) };
}

pub unsafe fn register_builtin(kind: Builtin, class: Value) {
    unsafe {
        BUILTIN_TYPES.with_borrow_mut(|types| {
            object::release(std::mem::replace(&mut types[kind as usize], object::retain(class)));
        });
    }
}

thread_local! {
    static NOT_IMPLEMENTED: std::cell::Cell<Value> = const { std::cell::Cell::new(std::ptr::null_mut()) };
}

/// The `NotImplemented` singleton, borrowed; NULL before the builtins exist.
pub fn not_implemented() -> Value {
    NOT_IMPLEMENTED.with(std::cell::Cell::get)
}

unsafe extern "C" fn register_not_implemented(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands(args) else {
            return std::ptr::null_mut();
        };
        object::release(NOT_IMPLEMENTED.with(|slot| slot.replace(object::retain(value))));
        object::none_value()
    }
}

pub fn builtin_type(kind: Builtin) -> Value {
    BUILTIN_TYPES.with_borrow(|types| types[kind as usize])
}

/// Release the registered builtin classes at executable shutdown.
pub unsafe fn finish_builtin_types() {
    unsafe {
        object::release(NOT_IMPLEMENTED.with(|slot| slot.replace(std::ptr::null_mut())));
        let types = BUILTIN_TYPES.with_borrow_mut(|types| std::mem::replace(types, [std::ptr::null_mut(); BUILTIN_COUNT]));
        for class in types {
            object::release(class);
        }
    }
}

/// The class of any value, borrowed; NULL only before the builtins exist.
pub unsafe fn class_of_value(value: Value) -> Value {
    unsafe {
        if object::is_fixnum(value) {
            return builtin_type(Builtin::Int);
        }
        if value.is_null() {
            return std::ptr::null_mut();
        }
        let kind = match &(*value).data {
            Data::Instance { ty, .. } | Data::NativeInstance { ty, .. } => return *ty,
            Data::Class(layout) => {
                return if layout.metaclass.is_null() {
                    builtin_type(Builtin::Type)
                } else {
                    layout.metaclass
                };
            }
            Data::None => Builtin::NoneType,
            Data::Bool(_) => Builtin::Bool,
            Data::BigInt(_, ty, ..) | Data::Float(_, ty, ..) | Data::Text(_, ty, ..) if !ty.is_null() => {
                return *ty;
            }
            Data::Bytes(_, ty, ..) | Data::List(_, ty, ..) | Data::Args(_, ty, ..) if !ty.is_null() => {
                return *ty;
            }
            Data::BigInt(..) => Builtin::Int,
            Data::Float(..) => Builtin::Float,
            Data::Text(..) => Builtin::Str,
            Data::Bytes(..) => Builtin::Bytes,
            Data::List(..) => Builtin::List,
            Data::Args(..) => Builtin::Tuple,
            Data::Function { globals, .. } if globals.is_null() => Builtin::BuiltinFunction,
            Data::Function { .. } => Builtin::Function,
            Data::BoundMethod { .. } => Builtin::Method,
            Data::Namespace(_) => Builtin::Module,
            Data::Traceback { .. } => Builtin::Traceback,
            Data::Frame { .. } => Builtin::Frame,
            _ => Builtin::Object,
        };
        builtin_type(kind)
    }
}

unsafe fn layout_of_class<'a>(class: Value) -> Option<&'a ClassLayout> {
    unsafe {
        if class.is_null() || object::is_fixnum(class) {
            return None;
        }
        match &(*class).data {
            Data::Class(layout) => Some(layout),
            _ => None,
        }
    }
}

/// A hook such as `__getattribute__` or `__setattr__`, unless the class only
/// inherits `object`'s default, which is the built-in behavior itself.
fn hook(layout: &ClassLayout, name: &str) -> Value {
    layout.special(name)
}

unsafe fn base_layout<'a>(layout: &ClassLayout) -> Option<&'a ClassLayout> {
    unsafe {
        if layout.base.is_null() {
            return None;
        }
        match &(*layout.base).data {
            Data::Class(base) => Some(base),
            _ => None,
        }
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

/// Finish a class statement. Without keywords, with at most one base whose
/// metaclass is `type`, and with nothing that needs Python-level class
/// creation (`__init_subclass__`, `__set_name__`, a prepared mapping), the
/// class is built directly; otherwise the compiled `build_class` calls the
/// metaclass, as CPython's `__build_class__` does.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_class(
    class_name: *const c_char,
    bases: Value,
    field_count: isize,
    field_names: *const *const c_char,
    namespace: Value,
    keywords: Value,
) -> Value {
    unsafe {
        if object::has_error() {
            return std::ptr::null_mut();
        }
        let Data::Args(base_values, ..) = &(*bases).data else {
            object::error("SystemError", "class bases must be a tuple");
            return std::ptr::null_mut();
        };
        let base = base_values.first().copied().unwrap_or(std::ptr::null_mut());
        if direct_class(base_values, namespace, keywords) {
            return create_class(class_name, base, field_count, field_names, namespace, None);
        }
        let Some(name) = name(class_name) else {
            return std::ptr::null_mut();
        };
        let mut fields = Vec::with_capacity(field_count.max(0) as usize);
        for index in 0..field_count.max(0) as usize {
            let Some(field) = self::name(*field_names.add(index)) else {
                for value in fields {
                    object::release(value);
                }
                return std::ptr::null_mut();
            };
            fields.push(text(field));
        }
        let fields_tuple = object::mule_sequence_tuple(fields.len() as isize, fields.as_ptr());
        for value in fields {
            object::release(value);
        }
        let builder = object::lookup(crate::sequences::mule_protocol_globals(), "build_class");
        if builder.is_null() {
            object::release(fields_tuple);
            object::error("SystemError", &format!("compiled class construction is unavailable for {name}"));
            return std::ptr::null_mut();
        }
        let name = text(name);
        let result = object::call(builder, &[name, bases, namespace, keywords, fields_tuple]);
        object::release(name);
        object::release(fields_tuple);
        result
    }
}

/// The namespace a class body runs against: the runtime's own for plain
/// classes, else whatever the metaclass's `__prepare__` returns through the
/// compiled `prepare_class`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_class_prepare(name: Value, bases: Value, keywords: Value) -> Value {
    unsafe {
        if object::has_error() {
            return std::ptr::null_mut();
        }
        let Data::Args(base_values, ..) = &(*bases).data else {
            object::error("SystemError", "class bases must be a tuple");
            return std::ptr::null_mut();
        };
        let plain = keywords == object::none_value()
            && base_values.len() <= 1
            && base_values
                .iter()
                .all(|base| class_of_value(*base) == builtin_type(Builtin::Type));
        if plain {
            return object::namespace();
        }
        let prepare = object::lookup(crate::sequences::mule_protocol_globals(), "prepare_class");
        if prepare.is_null() {
            object::error("SystemError", "compiled class preparation is unavailable");
            return std::ptr::null_mut();
        }
        object::call(prepare, &[name, bases, keywords])
    }
}

unsafe fn direct_class(bases: &[Value], namespace: Value, keywords: Value) -> bool {
    unsafe {
        if keywords != object::none_value() || bases.len() > 1 {
            return false;
        }
        let Data::Namespace(bindings) = &(*namespace).data else {
            return false;
        };
        if let Some(base) = bases.first() {
            let Some(layout) = layout_of_class(*base) else {
                return false;
            };
            if class_of_value(*base) != builtin_type(Builtin::Type)
                || !hook(layout, "__init_subclass__").is_null()
            {
                return false;
            }
        }
        // `object` itself is created before the compiled class protocol exists.
        if builtin_type(Builtin::Object).is_null() {
            return true;
        }
        // Implicit class methods and __set_name__ are applied by type.__new__.
        !bindings.iter().any(|(name, value)| {
            name == "__init_subclass__"
                || name == "__class_getitem__"
                || layout_of_class(class_of_value(value))
                    .is_some_and(|layout| layout.find("__set_name__").is_some())
        })
    }
}

pub type NativeConstructor = unsafe extern "C" fn(Value, Value, isize, *const Value) -> Value;
pub type NativeGetter = unsafe extern "C" fn(Value, Value, i64) -> Value;
pub type NativeSetter = unsafe extern "C" fn(Value, Value, i64, Value) -> i32;

/// LLVM reserves three zero-initialized machine words per attribute site.
/// IDs never repeat, so an expired class cannot make a cache entry valid again.
/// The encoding is a property of the class and name alone: whether the
/// receiver is an instance or the class itself is applied when it is used.
#[repr(C)]
#[derive(Default)]
pub struct AttrCache {
    pub class_id: usize,
    pub encoded: usize,
    pub epoch: usize,
}
const FIELD: usize = 1;
const METHOD: usize = 2;
const VALUE: usize = 3;
const DESCRIPTOR: usize = 4;
const DATA_DESCRIPTOR: usize = 5;
const KIND: usize = 7;
const SHIFT: u32 = 3;
// Class attribute encodings pack the defining class's depth above the index.
const DEPTH_SHIFT: u32 = 24;
const INDEX_MASK: usize = (1 << DEPTH_SHIFT) - 1;

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
            Data::BigInt(_, ty, ..)
            | Data::Float(_, ty, ..)
            | Data::Text(_, ty, ..)
            | Data::Bytes(_, ty, ..)
            | Data::List(_, ty, ..)
            | Data::Args(_, ty, ..) if !ty.is_null() => {
                layout_of_class(*ty).map(|layout| (layout, true, *ty))
            }
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
            layout.find("__set__").is_some() || layout.find("__delete__").is_some()
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
            Some(layout) if layout.find("__get__").is_some() => {
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
        let class_attribute = layout.find(attribute).map(|(owner, depth, index)| {
            let kind = attribute_kind(owner.attributes[index].1);
            (((depth << DEPTH_SHIFT) | index) << SHIFT) | kind
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
                (*cache).epoch = ATTRIBUTE_EPOCH.load(Ordering::Relaxed);
            }
        }
        resolved
    }
}

#[inline]
unsafe fn cached(layout: &ClassLayout, cache: *mut AttrCache) -> Option<usize> {
    unsafe {
        if !cache.is_null()
            && (*cache).class_id == layout.id
            && (*cache).epoch == ATTRIBUTE_EPOCH.load(Ordering::Relaxed)
        {
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
    position: usize,
    kind: usize,
    receiver: Value,
    instance: bool,
    owner: Value,
    bound: *mut Value,
) -> Value {
    unsafe {
        let defining = layout.ancestor(position >> DEPTH_SHIFT);
        let value = defining.attributes[position & INDEX_MASK].1;
        if kind == METHOD && instance {
            *bound = object::retain(receiver);
            return object::retain(value);
        }
        bind_class_attribute(value, if instance { receiver } else { std::ptr::null_mut() }, owner)
    }
}

/// The value for a resolved encoding, or None when an instance field is
/// unset and the class has no attribute of that name either.
/// An attribute in the instance's `__dict__`, owned, if it has one.
unsafe fn dict_attribute(receiver: Value, attribute: *const c_char) -> Option<Value> {
    unsafe {
        let attributes = match &(*receiver).data {
            Data::Instance { attributes, .. } => *attributes,
            _ => object::typed_attributes(receiver, false),
        };
        if attributes.is_null() {
            return None;
        }
        let value = object::lookup(attributes, name(attribute)?);
        (!value.is_null()).then(|| object::retain(value))
    }
}

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
            // The instance __dict__ precedes everything but data descriptors.
            if instance && kind != DATA_DESCRIPTOR {
                if let Some(value) = dict_attribute(receiver, attribute) {
                    return Some(value);
                }
            }
            let value = class_attribute(layout, index, kind, receiver, instance, owner, bound);
            return (!value.is_null()).then_some(value);
        }
        if instance {
            if matches!(&(*receiver).data, Data::NativeInstance { .. }) {
                let native = layout.native.as_ref().expect("native allocation layout");
                let value = (native.getter)(native.globals, receiver, index as i64);
                return (!value.is_null()).then_some(value);
            }
            if let Data::Instance { fields, .. } = &(*receiver).data {
                let value = fields[index];
                if !value.is_null() {
                    return Some(object::retain(value));
                }
            } else if let Some(value) = dict_attribute(receiver, attribute) {
                return Some(value);
            }
        }
        // An unset field, or a field name read through the class, falls back
        // to a class attribute of the same name.
        let attribute = name(attribute)?;
        let (defining, depth, index) = layout.find(attribute)?;
        let kind = attribute_kind(defining.attributes[index].1);
        let position = (depth << DEPTH_SHIFT) | index;
        let value = class_attribute(layout, position, kind, receiver, instance, owner, bound);
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
    unsafe { object::new_value(Data::Text(Text::from_rust_str(value), std::ptr::null_mut(), std::ptr::null_mut())) }
}

/// Call class attribute `index` as a method of `receiver`, as Python's
/// implicit special-method calls do: through the class, with the descriptor
/// protocol applied to anything that is not a plain function.
pub(crate) unsafe fn call_attribute(value: Value, receiver: Value, owner: Value, args: &[Value]) -> Value {
    unsafe {
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
unsafe fn call_hook(hook: Value, receiver: Value, owner: Value, attribute: *const c_char) -> Value {
    unsafe {
        let Some(attribute) = name(attribute) else {
            return std::ptr::null_mut();
        };
        let attribute = text(attribute);
        let result = call_attribute(hook, receiver, owner, &[attribute]);
        object::release(attribute);
        result
    }
}

/// Resolve an attribute before arguments are evaluated. Once registered, the
/// compiled protocol owns semantic lookup; the raw cache remains for bootstrap.
/// The callable and optional receiver each carry independent owned references.
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
        let Some(lookup) = ATTRIBUTE_LOOKUP.with(Cell::get) else {
            return resolve_with(receiver, attribute, cache, bound, true, true);
        };
        let Some(attribute) = name(attribute) else {
            return std::ptr::null_mut();
        };
        let attribute = text(attribute);
        let result = lookup(receiver, attribute);
        object::release(attribute);
        if result.is_null() {
            if !object::has_error() {
                object::error("SystemError", "attribute lookup returned NULL without an error");
            }
            return result;
        }
        if !object::is_fixnum(result) {
            if let Data::BoundMethod { receiver, method } = &(*result).data {
                *bound = object::retain(*receiver);
                let callable = object::retain(*method);
                object::release(result);
                return callable;
            }
        }
        result
    }
}

/// Raw attribute lookup. `hooks` enables primary getter dispatch and `fallbacks`
/// enables the bootstrap fallback. Explicit generic lookup disables both.
unsafe fn resolve_with(
    receiver: Value,
    attribute: *const c_char,
    cache: *mut AttrCache,
    bound: *mut Value,
    hooks: bool,
    fallbacks: bool,
) -> Value {
    unsafe {
        *bound = std::ptr::null_mut();
        if object::has_error() {
            return std::ptr::null_mut();
        }
        if let Some((layout, instance, owner)) = layout(receiver) {
            if !instance {
                let Some(attribute) = name(attribute) else {
                    return std::ptr::null_mut();
                };
                return resolve_on_class(receiver, layout, attribute);
            }
            let getter = if hooks { hook(layout, "__getattribute__") } else { std::ptr::null_mut() };
            if !getter.is_null() {
                return call_hook(getter, receiver, owner, attribute);
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
            } else if let Some(value) = dict_attribute(receiver, attribute) {
                return value;
            }
            let fallback = if fallbacks { hook(layout, "__getattr__") } else { std::ptr::null_mut() };
            if !fallback.is_null() {
                return call_hook(fallback, receiver, owner, attribute);
            }
            let Some(attribute) = name(attribute) else {
                return std::ptr::null_mut();
            };
            return missing(Some(layout), instance, attribute);
        }
        if hooks {
            let owner = class_of_value(receiver);
            if let Some(layout) = layout_of_class(owner) {
                let getter = hook(layout, "__getattribute__");
                if !getter.is_null() {
                    return call_hook(getter, receiver, owner, attribute);
                }
            }
        }
        let Some(attribute) = name(attribute) else {
            return std::ptr::null_mut();
        };
        resolve_on_value(receiver, attribute, bound)
    }
}

/// Primary lookup for the compiled protocol: invoke the getter once, without
/// invoking `__getattr__`. The boxed name and receiver are borrowed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_attribute_lookup_once(receiver: Value, attribute: Value) -> Value {
    unsafe {
        let Some(attribute) = c_name(attribute) else {
            return std::ptr::null_mut();
        };
        let mut bound = std::ptr::null_mut();
        let method = resolve_with(
            receiver, attribute.as_ptr(), std::ptr::null_mut(), &mut bound, true, false,
        );
        if method.is_null() {
            object::release(bound);
            return method;
        }
        if bound.is_null() {
            return method;
        }
        object::new_value(Data::BoundMethod { receiver: bound, method })
    }
}

/// Attribute lookup on a value that is neither a class nor an instance of a
/// compiled class: module bindings and function attributes first, then the
/// value's builtin class with the descriptor protocol.
unsafe fn resolve_on_value(receiver: Value, attribute: &str, bound: *mut Value) -> Value {
    unsafe {
        if !receiver.is_null() && !object::is_fixnum(receiver) {
            match &(*receiver).data {
                Data::Namespace(_) => {
                    let value = object::lookup(receiver, attribute);
                    if !value.is_null() {
                        return object::retain(value);
                    }
                }
                Data::Function { .. } => {
                    let value = object::function_attribute(receiver, attribute);
                    if !value.is_null() || object::has_error() {
                        return value;
                    }
                }
                _ => (),
            }
        }
        let typed = object::typed_attributes(receiver, false);
        if !typed.is_null() {
            let class = class_of_value(receiver);
            if let Some(layout) = layout_of_class(class) {
                if let Some((owner, _, index)) = layout.find(attribute) {
                    let value = owner.attributes[index].1;
                    if attribute_kind(value) == DATA_DESCRIPTOR {
                        return bind_class_attribute(value, receiver, class);
                    }
                }
            }
            let value = object::lookup(typed, attribute);
            if !value.is_null() {
                return object::retain(value);
            }
        }
        let class = class_of_value(receiver);
        let Some(layout) = layout_of_class(class) else {
            return missing(None, false, attribute);
        };
        let Some((owner, _, index)) = layout.find(attribute) else {
            let name = object::type_name(receiver).unwrap_or_else(|| "object".into());
            object::error(
                "AttributeError",
                &format!("'{name}' object has no attribute '{attribute}'"),
            );
            return std::ptr::null_mut();
        };
        let value = owner.attributes[index].1;
        if attribute_kind(value) == METHOD {
            *bound = object::retain(receiver);
            return object::retain(value);
        }
        bind_class_attribute(value, receiver, class)
    }
}

/// `C.attribute` for a class C, as `type.__getattribute__` does: a data
/// descriptor on the metaclass, then the class's own hierarchy (functions
/// unbound, descriptors given no instance), then any other metaclass
/// attribute bound to the class.
unsafe fn resolve_on_class(class: Value, layout: &ClassLayout, attribute: &str) -> Value {
    unsafe {
        let metaclass = class_of_value(class);
        let meta = layout_of_class(metaclass).and_then(|meta| meta.find(attribute));
        let meta_value = meta.map(|(owner, _, index)| owner.attributes[index].1);
        if let Some(value) = meta_value {
            if attribute_kind(value) == DATA_DESCRIPTOR {
                return bind_class_attribute(value, class, metaclass);
            }
        }
        if let Some((owner, _, index)) = layout.find(attribute) {
            return bind_class_attribute(owner.attributes[index].1, std::ptr::null_mut(), class);
        }
        if let Some(value) = meta_value {
            return bind_class_attribute(value, class, metaclass);
        }
        missing(Some(layout), false, attribute)
    }
}

/// A metaclass's own `__call__`, which replaces construction when calling
/// one of its classes; NULL for classes whose metaclass is `type`.
pub unsafe fn metaclass_call(class: Value) -> Value {
    unsafe {
        let metaclass = class_of_value(class);
        if metaclass == builtin_type(Builtin::Type) {
            return std::ptr::null_mut();
        }
        match layout_of_class(metaclass) {
            Some(meta) => hook(meta, "__call__"),
            None => std::ptr::null_mut(),
        }
    }
}

pub unsafe fn construct(class: Value, args: &[Value]) -> Value {
    unsafe {
        let call = metaclass_call(class);
        if !call.is_null() {
            return call_attribute(call, class, class_of_value(class), args);
        }
        let Data::Class(layout) = &(*class).data else {
            unreachable!();
        };
        if let Some(native) = &layout.native {
            return (native.constructor)(native.globals, class, args.len() as isize, args.as_ptr());
        }
        let creator = hook(layout, "__new__");
        let instance = if creator.is_null() {
            let Some(instance) = allocate(class, layout) else {
                return std::ptr::null_mut();
            };
            instance
        } else {
            // `__new__` is an implicit static method receiving the class.
            let mut arguments = Vec::with_capacity(args.len() + 1);
            arguments.push(class);
            arguments.extend_from_slice(args);
            let callable = if attribute_kind(creator) == METHOD {
                object::retain(creator)
            } else {
                bind_class_attribute(creator, std::ptr::null_mut(), class)
            };
            if callable.is_null() {
                return callable;
            }
            let instance = object::call(callable, &arguments);
            object::release(callable);
            // Python initializes only instances of the class being called.
            if instance.is_null() || !is_instance(instance, class) {
                return instance;
            }
            instance
        };
        let initializer = hook(layout, "__init__");
        if !initializer.is_null() {
            let result = call_attribute(initializer, instance, class, args);
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
        } else if !args.is_empty() && creator.is_null() {
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

/// A new instance of `class` with every field unset.
unsafe fn allocate(class: Value, layout: &ClassLayout) -> Option<Value> {
    unsafe {
        let mut fields = Vec::new();
        if fields.try_reserve_exact(layout.fields.len()).is_err() {
            object::error("MemoryError", "instance slot allocation failed");
            return None;
        }
        fields.resize(layout.fields.len(), std::ptr::null_mut());
        Some(object::new_value(Data::Instance {
            ty: object::retain(class),
            fields: fields.into_boxed_slice(),
            attributes: std::ptr::null_mut(),
        }))
    }
}

/// Whether `value` is an instance of `class` or of a class derived from it.
pub unsafe fn is_instance(value: Value, class: Value) -> bool {
    unsafe { is_subclass(class_of_value(value), class) }
}

/// Whether `derived` is `class` or has it among its bases.
pub unsafe fn is_subclass(derived: Value, class: Value) -> bool {
    unsafe {
        if derived.is_null() || derived == class {
            return derived == class && !derived.is_null();
        }
        let Some(layout) = class_layout(derived) else {
            return false;
        };
        layout.mro.contains(&class)
    }
}

/// Special lookup bypasses instance slots, matching the language's method protocol.
pub unsafe fn special_binding(receiver: Value, attribute: &str) -> Value {
    unsafe {
        let Some((layout, _, _)) = layout(receiver) else {
            return std::ptr::null_mut();
        };
        layout.special(attribute)
    }
}

/// An owned callable for a special method of an instance, with the receiver
/// bound, or NULL when the class lacks it.
pub unsafe fn special_bound(receiver: Value, attribute: &str) -> Value {
    unsafe {
        let Some((layout, true, owner)) = layout(receiver) else {
            return std::ptr::null_mut();
        };
        let value = layout.special(attribute);
        if value.is_null() {
            return value;
        }
        bind_class_attribute(value, receiver, owner)
    }
}

/// Explicit protocol lookup binds the complete class namespace. Unlike native
/// bootstrap hooks, it must include compiled object's default implementations.
pub unsafe fn lookup_special_bound(receiver: Value, attribute: &str, include_object_defaults: bool) -> Value {
    unsafe {
        let class = class_of_value(receiver);
        let Some(layout) = layout_of_class(class) else {
            return std::ptr::null_mut();
        };
        let value = if include_object_defaults { layout.attribute(attribute) } else { layout.special(attribute) };
        if value.is_null() { return value; }
        bind_class_attribute(value, receiver, class)
    }
}

/// A special method of a class, found on its metaclass and bound to the class.
pub unsafe fn special_bound_on_class(class: Value, attribute: &str) -> Value {
    unsafe {
        let metaclass = class_of_value(class);
        let Some(meta) = layout_of_class(metaclass) else {
            return std::ptr::null_mut();
        };
        let value = meta.special(attribute);
        if value.is_null() {
            return value;
        }
        bind_class_attribute(value, class, metaclass)
    }
}

pub unsafe fn special_method(receiver: Value, attribute: &str, args: &[Value]) -> Value {
    unsafe {
        let Some((layout, _, owner)) = layout(receiver) else {
            return std::ptr::null_mut();
        };
        let value = layout.special(attribute);
        if value.is_null() {
            return value;
        }
        call_attribute(value, receiver, owner, args)
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
    let bases = if base.is_null() { Vec::new() } else { vec![base] };
    unsafe { create_class_with(class_name, bases, std::ptr::null_mut(), field_count, field_names, namespace, native) }
}

/// The C3 linearization of a new class with the given direct bases:
/// the ancestors after the class itself, or None when the bases' orders
/// conflict or a base repeats.
fn linearize(bases: &[Value]) -> Option<Vec<Value>> {
    let mut sequences: Vec<Vec<Value>> = Vec::with_capacity(bases.len() + 1);
    for base in bases {
        let layout = unsafe { layout_of_class(*base) }?;
        let mut sequence = Vec::with_capacity(layout.mro.len() + 1);
        sequence.push(*base);
        sequence.extend_from_slice(&layout.mro);
        sequences.push(sequence);
    }
    sequences.push(bases.to_vec());
    let mut result = Vec::new();
    loop {
        sequences.retain(|sequence| !sequence.is_empty());
        if sequences.is_empty() {
            return Some(result);
        }
        let candidate = sequences.iter().find_map(|sequence| {
            let head = sequence[0];
            sequences
                .iter()
                .all(|other| !other[1..].contains(&head))
                .then_some(head)
        })?;
        result.push(candidate);
        for sequence in &mut sequences {
            if sequence[0] == candidate {
                sequence.remove(0);
            }
        }
    }
}

/// Create a class. A NULL base means `object` (once it exists), and a NULL
/// metaclass means the base's metaclass, or `type`.
unsafe fn create_class_with(
    class_name: *const c_char,
    bases: Vec<Value>,
    metaclass: Value,
    field_count: isize,
    field_names: *const *const c_char,
    namespace: Value,
    native: Option<NativeLayout>,
) -> Value {
    unsafe {
        let bases = if bases.is_empty() && native.is_none() {
            let object = builtin_type(Builtin::Object);
            if object.is_null() { Vec::new() } else { vec![object] }
        } else {
            bases
        };
        if bases.iter().enumerate().any(|(index, base)| bases[..index].contains(base)) {
            object::error("TypeError", "duplicate base class");
            return std::ptr::null_mut();
        }
        let base = bases.first().copied().unwrap_or(std::ptr::null_mut());
        let metaclass = if !metaclass.is_null() {
            metaclass
        } else if let Some(layout) = layout_of_class(base) {
            if layout.metaclass.is_null() {
                builtin_type(Builtin::Type)
            } else {
                layout.metaclass
            }
        } else {
            builtin_type(Builtin::Type)
        };
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
        for candidate in &bases {
            if object::is_fixnum(*candidate) || !matches!(&(**candidate).data, Data::Class(_)) {
                object::error("TypeError", "base must be a fixed-layout Mule class");
                return std::ptr::null_mut();
            }
            let Data::Class(layout) = &(**candidate).data else {
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
        }
        let Some(mro) = linearize(&bases) else {
            object::error(
                "TypeError",
                "Cannot create a consistent method resolution order (MRO) for bases",
            );
            return std::ptr::null_mut();
        };
        let inherited = if base.is_null() {
            None
        } else {
            let Data::Class(layout) = &(*base).data else {
                unreachable!();
            };
            Some(layout)
        };
        // Validate all names before taking ownership, making every rejection atomic.
        let mut fields = inherited.map_or_else(Vec::new, |layout| layout.fields.clone());
        let mut field_lookup: HashMap<String, usize> = inherited
            .map_or_else(HashMap::new, |layout| layout.field_lookup.clone());
        // Fields declared on later bases (in MRO order) get slots after the
        // primary base's, so every base's methods find their fields.
        for ancestor in &mro {
            let Some(layout) = layout_of_class(*ancestor) else {
                continue;
            };
            for field in &layout.fields {
                if !fields.iter().any(|known| known == field) {
                    field_lookup.insert(field.clone(), fields.len());
                    fields.push(field.clone());
                }
            }
        }
        for index in 0..field_count as usize {
            let Some(attribute) = name(*field_names.add(index)) else {
                return std::ptr::null_mut();
            };
            if !fields.iter().any(|field| field == attribute) {
                field_lookup.insert(attribute.into(), fields.len());
                fields.push(attribute.into());
            }
        }
        let mut attributes: Vec<(String, Value)> = Vec::with_capacity(bindings.len());
        let mut attribute_lookup: HashMap<String, usize> = HashMap::new();
        for (attribute, value) in bindings.iter() {
            if let Some(index) = attribute_lookup.get(attribute) {
                let old = attributes[*index].1;
                attributes[*index].1 = object::retain(value);
                object::release(old);
            } else {
                attribute_lookup.insert(attribute.into(), attributes.len());
                attributes.push((attribute.into(), object::retain(value)));
            }
        }
        // Defining equality without a hash makes instances unhashable, as
        // CPython's type creation sets `__hash__` to None.
        let defines = |wanted: &str| !bindings.get(wanted).is_null();
        if defines("__eq__") && !defines("__hash__") {
            let none = object::none_value();
            if let Some(index) = attribute_lookup.get("__hash__") {
                let old = attributes[*index].1;
                attributes[*index].1 = object::retain(none);
                object::release(old);
            } else {
                attribute_lookup.insert("__hash__".into(), attributes.len());
                attributes.push(("__hash__".into(), object::retain(none)));
            }
        }
        let slots = Some(bindings.get("__slots__")).filter(|value| !value.is_null());
        let own_dict = match slots {
            None => !base.is_null(),
            Some(slots) => slot_names_include_dict(slots),
        };
        let any_dict = mro.iter().any(|ancestor| {
            layout_of_class(*ancestor).is_some_and(|layout| layout.dict_allowed)
        });
        let dict_allowed = native.is_none() && (any_dict || own_dict);
        let id = NEXT_CLASS_ID.fetch_add(1, Ordering::Relaxed);
        assert_ne!(id, 0, "class identity space exhausted");
        let class = object::new_value(Data::Class(Box::new(ClassLayout {
            id,
            name: class_name.into(),
            base,
            bases: bases.iter().map(|base| object::retain(*base)).collect(),
            subclasses: Vec::new(),
            mro,
            metaclass: object::retain(metaclass),
            fields,
            attributes,
            field_lookup,
            attribute_lookup,
            dict_allowed,
            native,
        })));
        for base in &bases {
            let Data::Class(layout) = &mut (**base).data else { unreachable!() };
            if layout.subclasses.try_reserve(1).is_err() {
                object::error("MemoryError", "class relationship storage allocation failed");
                object::release(class);
                return std::ptr::null_mut();
            }
            layout.subclasses.push(class);
        }
        class
    }
}

/// Whether a `__slots__` value (a string or a sequence of strings) names `__dict__`.
unsafe fn slot_names_include_dict(slots: Value) -> bool {
    unsafe {
        let is_dict = |value: Value| {
            !object::is_fixnum(value)
                && matches!(&(*value).data, Data::Text(text, ..) if text.to_rust_string().as_deref() == Some("__dict__"))
        };
        if object::is_fixnum(slots) {
            return false;
        }
        match &(*slots).data {
            Data::Args(values, ..) | Data::List(values, ..) => values.iter().any(|value| is_dict(*value)),
            _ => is_dict(slots),
        }
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
        if !matches!(&(*namespace).data, Data::Namespace(_)) {
            // A metaclass's __prepare__ mapping: a missing key falls back.
            let key = text(attribute);
            let value = object::special(namespace, "__getitem__", &[key]);
            object::release(key);
            if !value.is_null() {
                return value;
            }
            if object::error_kind() != "KeyError" {
                return std::ptr::null_mut();
            }
            object::clear_error();
            return object::load_global(globals, attribute);
        }
        let value = object::lookup(namespace, attribute);
        if !value.is_null() {
            return object::retain(value);
        }
        if object::has_error() {
            return std::ptr::null_mut();
        }
        object::load_global(globals, attribute)
    }
}

/// Read a class body's name from its namespace, else use `fallback`, the
/// value of the enclosing function's variable of that name.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_class_load_or(
    namespace: Value,
    fallback: Value,
    name: *const c_char,
) -> Value {
    unsafe {
        let Some(attribute) = object::name_from_c(name) else {
            return std::ptr::null_mut();
        };
        if !matches!(&(*namespace).data, Data::Namespace(_)) {
            let key = text(attribute);
            let value = object::special(namespace, "__getitem__", &[key]);
            object::release(key);
            if !value.is_null() {
                return value;
            }
            if object::error_kind() != "KeyError" {
                return std::ptr::null_mut();
            }
            object::clear_error();
            return object::retain(fallback);
        }
        let value = object::lookup(namespace, attribute);
        object::retain(if value.is_null() { fallback } else { value })
    }
}

/// Bind a class body's name in its namespace or prepared mapping.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_class_store(namespace: Value, name: *const c_char, value: Value) -> i32 {
    unsafe {
        let Some(attribute) = object::name_from_c(name) else {
            return -1;
        };
        if matches!(&(*namespace).data, Data::Namespace(_)) {
            return if object::bind(namespace, attribute, value) { 0 } else { -1 };
        }
        let key = text(attribute);
        let result = object::special(namespace, "__setitem__", &[key, value]);
        object::release(key);
        if result.is_null() {
            if !object::has_error() {
                object::error("TypeError", "class namespace does not support assignment");
            }
            return -1;
        }
        object::release(result);
        0
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
            let key = text(attribute);
            let result = object::special(namespace, "__delitem__", &[key]);
            object::release(key);
            if result.is_null() {
                if missing_ok != 0 && object::error_kind() == "KeyError" {
                    object::clear_error();
                    return 0;
                }
                return -1;
            }
            object::release(result);
            return 0;
        };
        match bindings.take(attribute) {
            value if !value.is_null() => {
                object::release(value);
                0
            }
            _ if missing_ok != 0 => 0,
            _ => {
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
    unsafe { set_attribute_with(receiver, attribute, value, cache, true) }
}

/// Attribute assignment; without `hooks` it is `object.__setattr__`.
unsafe fn set_attribute_with(
    receiver: Value,
    attribute: *const c_char,
    value: Value,
    cache: *mut AttrCache,
    hooks: bool,
) -> i32 {
    unsafe {
        if object::has_error() {
            return -1;
        }
        if value.is_null() {
            object::error("SystemError", "NULL slot value");
            return -1;
        }
        if let Some((layout, instance, owner)) = layout(receiver) {
            if !instance {
                let Some(attribute) = name(attribute) else {
                    return -1;
                };
                return set_class_attribute(receiver, attribute, value);
            }
            let setter = if !hooks {
                std::ptr::null_mut()
            } else if matches!(&(*receiver).data, Data::Instance { .. } | Data::NativeInstance { .. }) {
                hook(layout, "__setattr__")
            } else {
                layout.method("__setattr__")
            };
            if !setter.is_null() {
                let Some(attribute) = name(attribute) else {
                    return -1;
                };
                let attribute = text(attribute);
                let result = call_attribute(setter, receiver, owner, &[attribute, value]);
                object::release(attribute);
                if result.is_null() {
                    return -1;
                }
                object::release(result);
                return 0;
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
                    if let Data::Instance { fields, .. } = &mut (*receiver).data {
                        let old = fields[index];
                        fields[index] = object::retain(value);
                        object::release(old);
                        return 0;
                    }
                    let Some(attribute) = name(attribute) else {
                        return -1;
                    };
                    let attributes = object::typed_attributes(receiver, true);
                    if attributes.is_null() {
                        missing(Some(layout), true, attribute);
                        return -1;
                    }
                    return if object::bind(attributes, attribute, value) { 0 } else { -1 };
                }
                Some(DATA_DESCRIPTOR) => {
                    let position = index.unwrap();
                    let descriptor = layout
                        .ancestor(position >> DEPTH_SHIFT)
                        .attributes[position & INDEX_MASK]
                        .1;
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
                    let Some(attribute) = name(attribute) else {
                        return -1;
                    };
                    if layout.dict_allowed {
                        if let Data::Instance { attributes, .. } = &mut (*receiver).data {
                            if attributes.is_null() {
                                *attributes = object::namespace();
                            }
                            return if object::bind(*attributes, attribute, value) { 0 } else { -1 };
                        }
                        let attributes = object::typed_attributes(receiver, true);
                        if !attributes.is_null() {
                            return if object::bind(attributes, attribute, value) { 0 } else { -1 };
                        }
                    }
                    missing(Some(layout), true, attribute);
                    return -1;
                }
            }
        }
        let owner = class_of_value(receiver);
        if let Some(layout) = layout_of_class(owner) {
            if let Some((defining, _, index)) = layout.find(name(attribute).unwrap_or("")) {
                let descriptor = defining.attributes[index].1;
                if attribute_kind(descriptor) == DATA_DESCRIPTOR {
                    let result = special_method(descriptor, "__set__", &[receiver, value]);
                    if result.is_null() {
                        return -1;
                    }
                    object::release(result);
                    return 0;
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
        }
        if !receiver.is_null()
            && !object::is_fixnum(receiver)
            && matches!(&(*receiver).data, Data::Function { .. })
        {
            return if object::set_function_attribute(receiver, attribute, value) {
                0
            } else {
                -1
            };
        }
        let typed = object::typed_attributes(receiver, true);
        if !typed.is_null() {
            if object::bind(typed, attribute, value) {
                return 0;
            }
            return -1;
        }
        missing(None, false, attribute);
        -1
    }
}

/// Bind one of a class's own attributes, as `type.__setattr__` does, and
/// invalidate every attribute cache. Typed native classes stay fixed because
/// compiled code relies on their layout and methods.
unsafe fn set_class_attribute(class: Value, attribute: &str, value: Value) -> i32 {
    unsafe {
        let Data::Class(layout) = &mut (*class).data else {
            unreachable!();
        };
        if layout.native.is_some() {
            object::error(
                "TypeError",
                &format!("cannot set '{attribute}' attribute of typed native class '{}'", layout.name),
            );
            return -1;
        }
        if let Some(index) = layout.attribute_lookup.get(attribute) {
            let old = layout.attributes[*index].1;
            layout.attributes[*index].1 = object::retain(value);
            object::release(old);
        } else {
            layout
                .attribute_lookup
                .insert(attribute.into(), layout.attributes.len());
            layout.attributes.push((attribute.into(), object::retain(value)));
        }
        ATTRIBUTE_EPOCH.fetch_add(1, Ordering::Relaxed);
        0
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
            if let Data::Text(text, ..) = &(*value).data {
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

/// `object.__repr__`: the class's qualified name and the value's address.
unsafe extern "C" fn default_repr(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let class = class_of_value(value);
        text(&format!(
            "<{} object at {:#x}>",
            object::qualified_name(class),
            value as usize
        ))
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

unsafe extern "C" fn class_subclasses(_: Value, args: Value) -> Value {
    unsafe {
        let Some([class]) = operands(args) else { return std::ptr::null_mut() };
        let Some(layout) = layout_of_class(class) else {
            return fail("TypeError", "class storage is required");
        };
        object::mule_sequence_tuple(layout.subclasses.len() as isize, layout.subclasses.as_ptr())
    }
}

unsafe extern "C" fn class_bases(_: Value, args: Value) -> Value {
    unsafe {
        let Some([class]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Some(layout) = class_layout(class) else {
            return std::ptr::null_mut();
        };
        object::mule_sequence_tuple(layout.bases.len() as isize, layout.bases.as_ptr())
    }
}

unsafe extern "C" fn class_mro(_: Value, args: Value) -> Value {
    unsafe {
        let Some([class]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Some(layout) = class_layout(class) else {
            return std::ptr::null_mut();
        };
        let mut values = Vec::with_capacity(layout.mro.len() + 1);
        values.push(class);
        values.extend_from_slice(&layout.mro);
        object::mule_sequence_tuple(values.len() as isize, values.as_ptr())
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

unsafe extern "C" fn class_lookup_or_null(_: Value, args: Value) -> Value {
    unsafe {
        let Some([class, attribute]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Some(layout) = layout_of_class(class) else {
            object::error("TypeError", "a compiled class is required");
            return std::ptr::null_mut();
        };
        let Some(attribute) = text_argument(attribute) else {
            return std::ptr::null_mut();
        };
        let value = layout.method(&attribute);
        if value.is_null() {
            object::none_value()
        } else {
            object::retain(value)
        }
    }
}

unsafe extern "C" fn class_lookup_default(_: Value, args: Value) -> Value {
    unsafe {
        let Some([class, attribute, default]) = operands(args) else { return std::ptr::null_mut(); };
        let Some(layout) = class_layout(class) else { return std::ptr::null_mut(); };
        let Some(attribute) = text_argument(attribute) else { return std::ptr::null_mut(); };
        let value = layout.method(&attribute);
        object::retain(if value.is_null() { default } else { value })
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

unsafe extern "C" fn bind_instance_attribute(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value, instance, owner]) = operands(args) else {
            return std::ptr::null_mut();
        };
        bind_class_attribute(value, instance, owner)
    }
}

unsafe extern "C" fn bound_method_receiver(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands(args) else {
            return std::ptr::null_mut();
        };
        if !object::is_fixnum(value) {
            if let Data::BoundMethod { receiver, .. } = &(*value).data {
                return object::retain(*receiver);
            }
        }
        fail("TypeError", "a bound method is required")
    }
}

unsafe extern "C" fn bound_method_function(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands(args) else { return std::ptr::null_mut() };
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::BoundMethod { method, .. } = &(*value).data {
                return object::retain(*method);
            }
        }
        fail("TypeError", "bound method storage is required")
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

unsafe fn c_name(attribute: Value) -> Option<std::ffi::CString> {
    unsafe {
        let attribute = text_argument(attribute)?;
        let Ok(attribute) = std::ffi::CString::new(attribute) else {
            object::error("ValueError", "attribute name contains a NUL character");
            return None;
        };
        Some(attribute)
    }
}

unsafe extern "C" fn get_attribute(_: Value, args: Value) -> Value {
    unsafe {
        let Some([receiver, attribute]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Some(attribute) = c_name(attribute) else {
            return std::ptr::null_mut();
        };
        mule_get_attr(receiver, attribute.as_ptr())
    }
}

unsafe extern "C" fn set_attribute(_: Value, args: Value) -> Value {
    unsafe {
        let Some([receiver, attribute, value]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Some(attribute) = c_name(attribute) else {
            return std::ptr::null_mut();
        };
        if mule_set_attr(receiver, attribute.as_ptr(), value) < 0 {
            return std::ptr::null_mut();
        }
        object::none_value()
    }
}

/// Delete an attribute: a class's own attribute, an instance field, or
/// through a data descriptor's `__delete__`.
unsafe extern "C" fn delete_attribute(_: Value, args: Value) -> Value {
    unsafe {
        let Some([receiver, attribute]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Some(attribute) = text_argument(attribute) else {
            return std::ptr::null_mut();
        };
        delete_attribute_with(receiver, &attribute, true)
    }
}

unsafe extern "C" fn generic_delete_attribute(_: Value, args: Value) -> Value {
    unsafe {
        let Some([receiver, attribute]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Some(attribute) = text_argument(attribute) else {
            return std::ptr::null_mut();
        };
        delete_attribute_with(receiver, &attribute, false)
    }
}

unsafe fn delete_attribute_with(receiver: Value, attribute: &str, hooks: bool) -> Value {
    unsafe {
        let Some((layout, instance, owner)) = layout(receiver) else {
            let owner = class_of_value(receiver);
            if let Some(layout) = layout_of_class(owner) {
                if let Some((defining, _, index)) = layout.find(attribute) {
                    let descriptor = defining.attributes[index].1;
                    if attribute_kind(descriptor) == DATA_DESCRIPTOR {
                        return special_method(descriptor, "__delete__", &[receiver]);
                    }
                }
            }
            return missing(None, false, &attribute);
        };
        if !instance {
            let Data::Class(layout) = &mut (*receiver).data else {
                unreachable!();
            };
            if layout.native.is_some() {
                return fail("TypeError", "cannot delete attributes of a typed native class");
            }
            let Some(index) = layout.attribute_lookup.remove(attribute) else {
                return missing(Some(layout), false, &attribute);
            };
            let (_, old) = layout.attributes.remove(index);
            for position in layout.attribute_lookup.values_mut() {
                if *position > index {
                    *position -= 1;
                }
            }
            object::release(old);
            ATTRIBUTE_EPOCH.fetch_add(1, Ordering::Relaxed);
            return object::none_value();
        }
        if hooks {
            let deleter = layout.method("__delattr__");
            if !deleter.is_null() {
                let attribute = text(attribute);
                let result = call_attribute(deleter, receiver, owner, &[attribute]);
                object::release(attribute);
                return result;
            }
        }
        let descriptor = layout.method(attribute);
        if matches!(&(*receiver).data, Data::Instance { .. } | Data::NativeInstance { .. })
            && !descriptor.is_null() && is_data_descriptor(descriptor) {
            let result = special_method(descriptor, "__delete__", &[receiver]);
            if result.is_null() && !object::has_error() {
                return fail("AttributeError", "attribute cannot be deleted");
            }
            return result;
        }
        if remove_instance_binding(receiver, layout, attribute) {
            return object::none_value();
        }
        missing(Some(layout), true, &attribute)
    }
}

/// `type(name, bases, namespace)` after compiled code has validated the
/// arguments: the bases tuple and the namespace's names and values in order.
unsafe extern "C" fn new_class(_: Value, args: Value) -> Value {
    unsafe {
        let Some([metaclass, class_name, bases_arg, names, values, fields]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Data::Args(fields, ..) = &(*fields).data else {
            return fail("TypeError", "class fields must be a tuple");
        };
        let mut owned_fields = Vec::with_capacity(fields.len());
        for field in fields {
            let Some(field) = text_argument(*field).and_then(|field| std::ffi::CString::new(field).ok()) else {
                return fail("TypeError", "class fields must be strings");
            };
            owned_fields.push(field);
        }
        let field_pointers: Vec<*const c_char> = owned_fields.iter().map(|field| field.as_ptr()).collect();
        let (Some(class_name), Data::Args(names, ..), Data::Args(values, ..)) =
            (text_argument(class_name), &(*names).data, &(*values).data)
        else {
            return fail("TypeError", "class names and values must be tuples");
        };
        let Ok(class_name) = std::ffi::CString::new(class_name) else {
            return fail("ValueError", "type name contains a NUL character");
        };
        let namespace = object::namespace();
        for (name, value) in names.iter().zip(values) {
            let Some(name) = text_argument(*name) else {
                object::release(namespace);
                return std::ptr::null_mut();
            };
            if !object::bind(namespace, &name, *value) {
                object::release(namespace);
                return std::ptr::null_mut();
            }
        }
        let bases: Vec<Value> = if bases_arg == object::none_value() {
            Vec::new()
        } else if !object::is_fixnum(bases_arg) {
            match &(*bases_arg).data {
                Data::Args(values, ..) => values.clone(),
                _ => vec![bases_arg],
            }
        } else {
            vec![bases_arg]
        };
        let class = create_class_with(
            class_name.as_ptr(),
            bases,
            metaclass,
            field_pointers.len() as isize,
            field_pointers.as_ptr(),
            namespace,
            None,
        );
        object::release(namespace);
        class
    }
}

/// `register_type(name, class)`: install one of the builtin classes.
unsafe extern "C" fn register_type(_: Value, args: Value) -> Value {
    unsafe {
        let Some([name, class]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Some(name) = text_argument(name) else {
            return std::ptr::null_mut();
        };
        let Some(index) = BUILTIN_NAMES.iter().position(|candidate| *candidate == name) else {
            return fail("ValueError", "unknown builtin type");
        };
        if class_layout(class).is_none() {
            return std::ptr::null_mut();
        }
        BUILTIN_TYPES.with_borrow_mut(|types| {
            object::release(std::mem::replace(&mut types[index], object::retain(class)));
        });
        // Classes created before `type` existed adopt it as their metaclass.
        object::none_value()
    }
}

/// `traceback_field(value, index)`: a field of a traceback or frame record.
unsafe extern "C" fn traceback_field(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value, index]) = operands(args) else {
            return std::ptr::null_mut();
        };
        if !object::is_fixnum(index) {
            return fail("TypeError", "traceback field index must be an int");
        }
        crate::traceback::field(value, object::fixnum(index))
    }
}

/// `type(value)`: the class of any value.
unsafe extern "C" fn type_of(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let class = class_of_value(value);
        if class.is_null() {
            return fail("SystemError", "builtin types are not initialized");
        }
        object::retain(class)
    }
}

/// `is_subclass(derived, base)` over compiled classes.
unsafe extern "C" fn is_subclass_export(_: Value, args: Value) -> Value {
    unsafe {
        let Some([derived, base]) = operands(args) else {
            return std::ptr::null_mut();
        };
        object::bool_value(is_subclass(derived, base))
    }
}

/// The class's name, bases (its base, or none) and own attributes in order.
unsafe extern "C" fn class_name(_: Value, args: Value) -> Value {
    unsafe {
        let Some([class]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Some(layout) = class_layout(class) else {
            return std::ptr::null_mut();
        };
        text(&layout.name)
    }
}

unsafe extern "C" fn set_class_name(_: Value, args: Value) -> Value {
    unsafe {
        let Some([class, name]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Some(name) = text_argument(name) else {
            return std::ptr::null_mut();
        };
        if class_layout(class).is_none() {
            return std::ptr::null_mut();
        }
        let Data::Class(layout) = &mut (*class).data else {
            unreachable!();
        };
        layout.name = name;
        object::none_value()
    }
}

unsafe extern "C" fn class_own_names(_: Value, args: Value) -> Value {
    unsafe {
        let Some([class]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Some(layout) = class_layout(class) else {
            return std::ptr::null_mut();
        };
        let names: Vec<Value> = layout.attributes.iter().map(|(name, _)| text(name)).collect();
        let result = object::mule_sequence_tuple(names.len() as isize, names.as_ptr());
        for name in names {
            object::release(name);
        }
        result
    }
}

/// An instance's attributes outside the class: set fields, then its
/// `__dict__` entries, as (name, value) pairs; None when it has neither kind.
unsafe extern "C" fn instance_items(_: Value, args: Value) -> Value {
    unsafe {
        let Some([instance]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Some((layout, true, _)) = layout(instance) else {
            return fail("TypeError", "an instance of a compiled class is required");
        };
        let Data::Instance { fields, attributes, .. } = &(*instance).data else {
            return fail("TypeError", "an instance with reference fields is required");
        };
        let mut pairs = Vec::new();
        for (index, field) in layout.fields.iter().enumerate() {
            if !fields[index].is_null() && layout.field_lookup.contains_key(field) {
                let key = text(field);
                pairs.push(object::mule_sequence_tuple(2, [key, fields[index]].as_ptr()));
                object::release(key);
            }
        }
        if !attributes.is_null() {
            if let Data::Namespace(bindings) = &(**attributes).data {
                for (name, value) in bindings.iter() {
                    let key = text(name);
                    pairs.push(object::mule_sequence_tuple(2, [key, value].as_ptr()));
                    object::release(key);
                }
            }
        }
        let result = object::mule_sequence_tuple(pairs.len() as isize, pairs.as_ptr());
        for pair in pairs {
            object::release(pair);
        }
        result
    }
}

/// Dynamic namespace entries, excluding fixed instance fields.
unsafe extern "C" fn instance_extra_items(_: Value, args: Value) -> Value {
    unsafe {
        let Some([instance]) = operands(args) else {
            return std::ptr::null_mut();
        };
        if instance.is_null() || object::is_fixnum(instance) {
            return fail("TypeError", "an instance with reference fields is required");
        }
        let Data::Instance { attributes, .. } = &(*instance).data else {
            return fail("TypeError", "an instance with reference fields is required");
        };
        let mut pairs = Vec::new();
        if !attributes.is_null() {
            if let Data::Namespace(bindings) = &(**attributes).data {
                for (name, value) in bindings.iter() {
                    let key = text(name);
                    pairs.push(object::mule_sequence_tuple(2, [key, value].as_ptr()));
                    object::release(key);
                }
            }
        }
        let result = object::mule_sequence_tuple(pairs.len() as isize, pairs.as_ptr());
        for pair in pairs {
            object::release(pair);
        }
        result
    }
}

/// A dictionary-owning storage subclass's namespace, owned. Reference-field
/// instances use the existing compiled instance view and return None instead.
unsafe extern "C" fn storage_dictionary(_: Value, args: Value) -> Value {
    unsafe {
        let Some([receiver]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Some((layout, true, _)) = layout(receiver) else {
            return missing(None, true, "__dict__");
        };
        if !layout.dict_allowed {
            return missing(Some(layout), true, "__dict__");
        }
        if matches!(&(*receiver).data, Data::Instance { .. }) {
            return object::none_value();
        }
        let attributes = object::typed_attributes(receiver, true);
        if attributes.is_null() {
            return missing(Some(layout), true, "__dict__");
        }
        object::retain(attributes)
    }
}

/// Write dictionary storage directly, without attribute hooks or descriptors.
unsafe extern "C" fn instance_set_binding(_: Value, args: Value) -> Value {
    unsafe {
        let Some([receiver, attribute, value]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Some(attribute) = text_argument(attribute) else {
            return std::ptr::null_mut();
        };
        let Some((layout, true, _)) = layout(receiver) else {
            return fail("TypeError", "an instance with dictionary storage is required");
        };
        if !layout.dict_allowed {
            return missing(Some(layout), true, "__dict__");
        }
        let attributes = if let Data::Instance { fields, attributes, .. } = &mut (*receiver).data {
            if let Some(index) = layout.field_lookup.get(&attribute) {
                let old = std::mem::replace(&mut fields[*index], object::retain(value));
                object::release(old);
                return object::none_value();
            }
            if attributes.is_null() {
                *attributes = object::namespace();
            }
            *attributes
        } else {
            object::typed_attributes(receiver, true)
        };
        if attributes.is_null() {
            return fail("TypeError", "an instance with dictionary storage is required");
        }
        if object::bind(attributes, &attribute, value) {
            object::none_value()
        } else {
            std::ptr::null_mut()
        }
    }
}

/// Unbind raw instance storage. The caller owns absence/error translation.
unsafe fn remove_instance_binding(receiver: Value, layout: &ClassLayout, attribute: &str) -> bool {
    unsafe {
        let attributes = if let Data::Instance { fields, attributes, .. } = &mut (*receiver).data {
            if let Some(index) = layout.field_lookup.get(attribute) {
                let old = std::mem::replace(&mut fields[*index], std::ptr::null_mut());
                if !old.is_null() {
                    object::release(old);
                    return true;
                }
            }
            *attributes
        } else {
            object::typed_attributes(receiver, false)
        };
        if attributes.is_null() {
            return false;
        }
        let Data::Namespace(bindings) = &mut (*attributes).data else {
            unreachable!("instance attributes use namespace storage");
        };
        let old = bindings.take(attribute);
        if old.is_null() {
            return false;
        }
        object::release(old);
        true
    }
}

unsafe extern "C" fn instance_delete_binding(_: Value, args: Value) -> Value {
    unsafe {
        let Some([receiver, attribute]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Some(attribute) = text_argument(attribute) else {
            return std::ptr::null_mut();
        };
        let Some((layout, true, _)) = layout(receiver) else {
            return fail("TypeError", "an instance with dictionary storage is required");
        };
        if !layout.dict_allowed {
            return missing(Some(layout), true, "__dict__");
        }
        object::bool_value(remove_instance_binding(receiver, layout, &attribute))
    }
}

/// A fresh class-body namespace in the runtime's direct representation.
unsafe extern "C" fn new_namespace(_: Value, args: Value) -> Value {
    unsafe {
        let Some([]) = operands::<0>(args) else {
            return std::ptr::null_mut();
        };
        object::namespace()
    }
}

/// The bindings of a runtime namespace, or None for any other mapping.
unsafe extern "C" fn namespace_items(_: Value, args: Value) -> Value {
    unsafe {
        let Some([namespace]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Data::Namespace(bindings) = &(*namespace).data else {
            return object::none_value();
        };
        let mut pairs = Vec::with_capacity(bindings.len());
        for (name, value) in bindings.iter() {
            let key = text(name);
            let pair = object::mule_sequence_tuple(2, [key, value].as_ptr());
            object::release(key);
            pairs.push(pair);
        }
        let result = object::mule_sequence_tuple(pairs.len() as isize, pairs.as_ptr());
        for pair in pairs {
            object::release(pair);
        }
        result
    }
}

/// An own namespace binding as a one-element tuple, or None for absence or
/// other storage. Tuple wrapping distinguishes a binding whose value is None.
unsafe extern "C" fn namespace_binding(_: Value, args: Value) -> Value {
    unsafe {
        let Some([receiver, attribute]) = operands(args) else {
            return std::ptr::null_mut();
        };
        if receiver.is_null() || object::is_fixnum(receiver)
            || !matches!(&(*receiver).data, Data::Namespace(_)) {
            return object::none_value();
        }
        let Some(attribute) = text_argument(attribute) else {
            return std::ptr::null_mut();
        };
        let value = object::lookup(receiver, &attribute);
        if value.is_null() {
            return if object::has_error() { std::ptr::null_mut() } else { object::none_value() };
        }
        object::mule_sequence_tuple(1, [value].as_ptr())
    }
}

unsafe extern "C" fn namespace_bind(_: Value, args: Value) -> Value {
    unsafe {
        let Some([namespace, name, value]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Some(name) = text_argument(name) else {
            return std::ptr::null_mut();
        };
        if namespace.is_null() || object::is_fixnum(namespace)
            || !matches!(&(*namespace).data, Data::Namespace(_)) {
            object::error("TypeError", "a namespace is required");
            return std::ptr::null_mut();
        }
        if object::bind(namespace, &name, value) {
            object::none_value()
        } else {
            std::ptr::null_mut()
        }
    }
}

unsafe extern "C" fn namespace_unbind(_: Value, args: Value) -> Value {
    unsafe {
        let Some([namespace, name]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Some(name) = text_argument(name) else {
            return std::ptr::null_mut();
        };
        if namespace.is_null() || object::is_fixnum(namespace) {
            object::error("TypeError", "a namespace is required");
            return std::ptr::null_mut();
        }
        let removed = match &mut (*namespace).data {
            Data::Namespace(bindings) => bindings.take(&name),
            _ => {
                object::error("TypeError", "a namespace is required");
                return std::ptr::null_mut();
            }
        };
        let found = !removed.is_null();
        object::release(removed);
        object::bool_value(found)
    }
}

/// A class's own or inherited `__new__`, or None when only `object`'s applies.
pub unsafe fn overriding_new(class: Value) -> Value {
    unsafe {
        match layout_of_class(class) {
            Some(layout) => hook(layout, "__new__"),
            None => std::ptr::null_mut(),
        }
    }
}

unsafe extern "C" fn overriding_new_export(_: Value, args: Value) -> Value {
    unsafe {
        let Some([class]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let creator = overriding_new(class);
        if creator.is_null() {
            object::none_value()
        } else {
            object::retain(creator)
        }
    }
}

/// `object.__new__`: a new instance of a compiled class with unset fields.
unsafe extern "C" fn allocate_instance(_: Value, args: Value) -> Value {
    unsafe {
        let Some([class]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Some(layout) = class_layout(class) else {
            return std::ptr::null_mut();
        };
        if layout.native.is_some() {
            return fail("TypeError", "typed native classes allocate through their constructor");
        }
        allocate(class, layout).unwrap_or(std::ptr::null_mut())
    }
}

/// `object.__setattr__` for a reference field: bypasses `__setattr__` hooks.
unsafe extern "C" fn set_instance_field(_: Value, args: Value) -> Value {
    unsafe {
        let Some([instance, attribute, value]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Some(attribute) = text_argument(attribute) else {
            return std::ptr::null_mut();
        };
        if let Some((layout, true, _)) = layout(instance) {
            if let (Some(index), Data::Instance { fields, .. }) =
                (layout.field_lookup.get(&attribute), &mut (*instance).data)
            {
                let old = fields[*index];
                fields[*index] = object::retain(value);
                object::release(old);
                return object::none_value();
            }
            return missing(Some(layout), true, &attribute);
        }
        fail("TypeError", "an instance with reference fields is required")
    }
}

/// Clear one named reference slot without hooks or descriptor dispatch.
unsafe extern "C" fn delete_instance_field(_: Value, args: Value) -> Value {
    unsafe {
        let Some([instance, attribute]) = operands(args) else { return std::ptr::null_mut(); };
        let Some(attribute) = text_argument(attribute) else { return std::ptr::null_mut(); };
        if let Some((layout, true, _)) = layout(instance) {
            if let (Some(index), Data::Instance { fields, .. }) = (layout.field_lookup.get(&attribute), &mut (*instance).data) {
                let old = std::mem::replace(&mut fields[*index], std::ptr::null_mut());
                if !old.is_null() {
                    object::release(old);
                    return object::none_value();
                }
            }
            return missing(Some(layout), true, &attribute);
        }
        fail("TypeError", "an instance with reference fields is required")
    }
}

/// `object.__getattribute__(obj, name)`.
unsafe extern "C" fn generic_get_attribute(_: Value, args: Value) -> Value {
    unsafe {
        let Some([receiver, attribute]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Some(attribute) = c_name(attribute) else {
            return std::ptr::null_mut();
        };
        let mut bound = std::ptr::null_mut();
        let method = resolve_with(receiver, attribute.as_ptr(), std::ptr::null_mut(), &mut bound, false, false);
        if bound.is_null() || method.is_null() {
            return method;
        }
        object::new_value(Data::BoundMethod { receiver: bound, method })
    }
}

/// `object.__setattr__(obj, name, value)`.
unsafe extern "C" fn generic_set_attribute(_: Value, args: Value) -> Value {
    unsafe {
        let Some([receiver, attribute, value]) = operands(args) else {
            return std::ptr::null_mut();
        };
        let Some(attribute) = c_name(attribute) else {
            return std::ptr::null_mut();
        };
        if set_attribute_with(receiver, attribute.as_ptr(), value, std::ptr::null_mut(), false) < 0 {
            return std::ptr::null_mut();
        }
        object::none_value()
    }
}

unsafe extern "C" fn is_instance_export(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value, class]) = operands(args) else {
            return std::ptr::null_mut();
        };
        object::bool_value(is_instance(value, class))
    }
}

/// Bind the class storage operations compiled descriptor and `super` code
/// uses into the compiler-owned native namespace.
/// The attributes namespace of a function, created on demand when asked.
unsafe extern "C" fn function_attributes(_: Value, args: Value) -> Value {
    unsafe {
        let Some([function]) = operands(args) else {
            return std::ptr::null_mut();
        };
        if function.is_null() || object::is_fixnum(function) {
            object::error("TypeError", "a function is required");
            return std::ptr::null_mut();
        }
        let Data::Function { attributes, .. } = &mut (*function).data else {
            object::error("TypeError", "a function is required");
            return std::ptr::null_mut();
        };
        if attributes.is_null() {
            *attributes = object::namespace();
        }
        object::retain(*attributes)
    }
}

/// `typed_value(value, class)`: record `class` on a freshly created int,
/// float or str storage value, making it an instance of that subclass.
unsafe extern "C" fn typed_value(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value, class]) = operands(args) else {
            return std::ptr::null_mut();
        };
        if object::is_fixnum(value) {
            let boxed = object::new_value(Data::BigInt(
                crate::gmp::BigInt::from_i64(object::fixnum(value)),
                object::retain(class),
                std::ptr::null_mut(),
            ));
            return boxed;
        }
        if value.is_null() {
            object::error("TypeError", "typed value storage is required");
            return std::ptr::null_mut();
        }
        match &mut (*value).data {
            Data::BigInt(_, ty, _)
            | Data::Float(_, ty, _)
            | Data::Text(_, ty, _)
            | Data::Bytes(_, ty, _)
            | Data::List(_, ty, _)
            | Data::Args(_, ty, _) => {
                object::release(*ty);
                *ty = object::retain(class);
                crate::gc::track(value);
                object::retain(value)
            }
            _ => {
                object::error("TypeError", "typed value storage is required");
                std::ptr::null_mut()
            }
        }
    }
}

macro_rules! function_annotation_slot {
    ($getter:ident, $setter:ident, $slot:ident) => {
        unsafe extern "C" fn $getter(_: Value, args: Value) -> Value {
            unsafe {
                let Some([function]) = operands(args) else { return std::ptr::null_mut() };
                if function.is_null() || object::is_fixnum(function) {
                    return fail("TypeError", "function storage is required");
                }
                let Data::Function { $slot, .. } = &(*function).data else {
                    return fail("TypeError", "function storage is required");
                };
                object::retain(if $slot.is_null() { object::none_value() } else { *$slot })
            }
        }

        unsafe extern "C" fn $setter(_: Value, args: Value) -> Value {
            unsafe {
                let Some([function, value]) = operands(args) else { return std::ptr::null_mut() };
                if function.is_null() || object::is_fixnum(function) {
                    return fail("TypeError", "function storage is required");
                }
                let Data::Function { $slot, .. } = &mut (*function).data else {
                    return fail("TypeError", "function storage is required");
                };
                object::release(std::mem::replace($slot, object::retain(value)));
                object::none_value()
            }
        }
    };
}

function_annotation_slot!(function_annotate, set_function_annotate, annotate);
function_annotation_slot!(function_annotations, set_function_annotations, annotations);
function_annotation_slot!(function_positional_defaults, set_function_positional_defaults, positional_defaults);
function_annotation_slot!(function_keyword_defaults, set_function_keyword_defaults, keyword_defaults);

unsafe extern "C" fn code_metadata(_: Value, args: Value) -> Value {
    unsafe {
        let Some([receiver]) = operands(args) else { return std::ptr::null_mut(); };
        crate::traceback::code_metadata(receiver, false)
    }
}

unsafe extern "C" fn code_cache(_: Value, args: Value) -> Value {
    unsafe {
        let Some([receiver]) = operands(args) else { return std::ptr::null_mut(); };
        crate::traceback::code_metadata(receiver, true)
    }
}

unsafe extern "C" fn set_code_cache(_: Value, args: Value) -> Value {
    unsafe {
        let Some([receiver, code]) = operands(args) else { return std::ptr::null_mut(); };
        crate::traceback::cache_code(receiver, code)
    }
}

unsafe extern "C" fn function_captured_defaults(_: Value, args: Value) -> Value {
    unsafe {
        let Some([function]) = operands(args) else { return std::ptr::null_mut(); };
        if function.is_null() || object::is_fixnum(function) { return fail("TypeError", "function storage is required"); }
        let Data::Function { defaults, .. } = &(*function).data else { return fail("TypeError", "function storage is required"); };
        object::retain(if defaults.is_null() { object::none_value() } else { *defaults })
    }
}

pub unsafe fn populate(globals: Value) -> bool {
    unsafe {
        if object::has_error() {
            return false;
        }
        let exports: [(&str, object::Native); 60] = [
            ("delete_instance_field", delete_instance_field),
            ("class_lookup_default", class_lookup_default),
            ("code_metadata", code_metadata),
            ("code_cache", code_cache),
            ("set_code_cache", set_code_cache),
            ("function_captured_defaults", function_captured_defaults),
            ("function_positional_defaults", function_positional_defaults),
            ("set_function_positional_defaults", set_function_positional_defaults),
            ("function_keyword_defaults", function_keyword_defaults),
            ("set_function_keyword_defaults", set_function_keyword_defaults),
            ("class_subclasses", class_subclasses),
            ("bound_method_function", bound_method_function),
            ("function_annotate", function_annotate),
            ("set_function_annotate", set_function_annotate),
            ("function_annotations", function_annotations),
            ("set_function_annotations", set_function_annotations),
            ("register_not_implemented", register_not_implemented),
            ("instance_items", instance_items),
            ("instance_extra_items", instance_extra_items),
            ("storage_dictionary", storage_dictionary),
            ("instance_set_binding", instance_set_binding),
            ("instance_delete_binding", instance_delete_binding),
            ("new_namespace", new_namespace),
            ("namespace_items", namespace_items),
            ("namespace_binding", namespace_binding),
            ("namespace_bind", namespace_bind),
            ("namespace_unbind", namespace_unbind),
            ("overriding_new", overriding_new_export),
            ("generic_get_attribute", generic_get_attribute),
            ("generic_set_attribute", generic_set_attribute),
            ("generic_delete_attribute", generic_delete_attribute),
            ("register_type", register_type),
            ("traceback_field", traceback_field),
            ("type_of", type_of),
            ("is_subclass", is_subclass_export),
            ("class_name", class_name),
            ("set_class_name", set_class_name),
            ("class_own_names", class_own_names),
            ("allocate_instance", allocate_instance),
            ("set_instance_field", set_instance_field),
            ("is_instance", is_instance_export),
            ("get_attribute", get_attribute),
            ("set_attribute", set_attribute),
            ("delete_attribute", delete_attribute),
            ("new_class", new_class),
            ("is_class", is_class),
            ("class_of", class_of),
            ("default_repr", default_repr),
            ("class_base", class_base),
            ("class_bases", class_bases),
            ("class_mro", class_mro),
            ("class_lookup", class_lookup),
            ("class_lookup_or_null", class_lookup_or_null),
            ("bind_attribute", bind_attribute),
            ("bind_instance_attribute", bind_instance_attribute),
            ("bound_method", bound_method),
            ("bound_method_receiver", bound_method_receiver),
            ("instance_field", instance_field),
            ("typed_value", typed_value),
            ("function_attributes", function_attributes),
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
