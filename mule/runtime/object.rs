//! Mule values and the native-code ABI. Heap ownership is explicit at this boundary;
//! generated code retains each live value and releases it on normal and error exits.
use crate::{builtins, gmp::BigInt, numeric, text::Text};
use crate::value_buffer::ValueBuffer;
use std::cell::{Cell, RefCell};
use std::ffi::{CStr, c_char, c_void};
use std::sync::atomic::{AtomicUsize, Ordering};

pub type Value = *mut Object;
pub type Native = unsafe extern "C" fn(Value, Value) -> Value;
#[repr(C)]
pub struct Object {
    pub references: std::sync::atomic::AtomicUsize,
    pub tracked: bool,
    pub gc_slot: u32,
    pub data: Data,
}
/// A global's storage: one per namespace and name, at an address that never
/// changes, so compiled code reads `value` directly. A unit's cells are its
/// static data, registered with the namespace when the unit links it, as SBCL
/// code reaches immobile symbols' value cells at fixed addresses; a namespace
/// boxes cells for names first bound at run time.
#[repr(C)]
pub struct GlobalCell {
    /// The namespace's own value, else a copy of the builtin of the same
    /// name, else the undefined object; never NULL.
    value: Value,
    /// Nonzero while the namespace itself binds the name.
    own: usize,
    /// The builtins' cell for the same name: what exact-semantics code reads
    /// and what `del` restores. NULL in the builtins' own cells.
    builtin: *mut GlobalCell,
    /// The namespace the cell belongs to. NULL for a cell used only to read
    /// builtins, and once the namespace is freed.
    namespace: Value,
}

impl GlobalCell {
    fn unbound(builtin: *mut GlobalCell, namespace: Value) -> Self {
        let mut cell = Self { value: undefined_value(), own: 0, builtin, namespace };
        cell.value = unsafe { cell.inherited() };
        cell
    }

    fn bound(&self) -> bool {
        self.own != 0
    }

    /// The value the name has while the namespace does not bind it: the
    /// builtin's, owned, or the undefined object.
    unsafe fn inherited(&self) -> Value {
        unsafe {
            if !self.builtin.is_null() && (*self.builtin).bound() {
                retain((*self.builtin).value)
            } else {
                undefined_value()
            }
        }
    }
}

struct Entry {
    name: String,
    cell: *mut GlobalCell,
    /// The namespace allocated the cell and frees it; a unit's static cell
    /// is only registered here.
    boxed: bool,
}

static NEXT_NAMESPACE_VERSION: AtomicUsize = AtomicUsize::new(1);

/// A namespace's names and their cells, in binding order: a name moves to
/// the end when it becomes bound, as a key added to a dict does. Only names
/// the namespace itself binds are visible to queries.
pub struct Namespace {
    entries: Vec<Entry>,
    version: usize,
}

impl Default for Namespace {
    fn default() -> Self {
        Self {
            entries: Vec::new(),
            version: NEXT_NAMESPACE_VERSION.fetch_add(1, Ordering::Relaxed),
        }
    }
}

impl Drop for Namespace {
    fn drop(&mut self) {
        for entry in &self.entries {
            if entry.boxed {
                drop(unsafe { Box::from_raw(entry.cell) });
            }
        }
    }
}

impl Namespace {
    #[inline]
    pub fn version(&self) -> usize {
        self.version
    }

    #[inline]
    pub fn get_at(&self, index: usize) -> Value {
        self.entries
            .get(index)
            .map(|entry| unsafe { &*entry.cell })
            .filter(|cell| cell.bound())
            .map_or(std::ptr::null_mut(), |cell| cell.value)
    }

    pub fn lookup_index(&self, name: &str) -> Option<(usize, Value)> {
        let index = self.position(name)?;
        let cell = unsafe { &*self.entries[index].cell };
        cell.bound().then_some((index, cell.value))
    }

    fn position(&self, name: &str) -> Option<usize> {
        self.entries.iter().position(|entry| entry.name == name)
    }

    fn cell_of(&self, name: &str) -> Option<&GlobalCell> {
        self.position(name).map(|index| unsafe { &*self.entries[index].cell })
    }

    /// The value the namespace binds to `name`, borrowed, or NULL.
    pub fn get(&self, name: &str) -> Value {
        self.cell_of(name)
            .filter(|cell| cell.bound())
            .map_or(std::ptr::null_mut(), |cell| cell.value)
    }

    /// The cell of `name`, boxed and unbound when the namespace has none.
    /// `this` is the namespace object owning these bindings.
    fn cell(&mut self, name: &str, builtin: *mut GlobalCell, this: Value) -> *mut GlobalCell {
        match self.position(name) {
            Some(index) => self.entries[index].cell,
            None => {
                let cell = Box::into_raw(Box::new(GlobalCell::unbound(builtin, this)));
                self.entries.push(Entry { name: name.into(), cell, boxed: true });
                cell
            }
        }
    }

    /// Bind `name` to `value`, which the cell takes over, and return what the
    /// cell held, owned: the previous binding, an inherited copy, or the
    /// undefined object. `this` is the namespace object owning these bindings.
    pub fn replace(&mut self, name: &str, value: Value, this: Value) -> Value {
        let index = self.position(name).unwrap_or_else(|| {
            let cell = Box::into_raw(Box::new(GlobalCell::unbound(std::ptr::null_mut(), this)));
            self.entries.push(Entry { name: name.into(), cell, boxed: true });
            self.entries.len() - 1
        });
        let cell = unsafe { &mut *self.entries[index].cell };
        let previous = std::mem::replace(&mut cell.value, value);
        if !cell.bound() {
            cell.own = 1;
            if index + 1 != self.entries.len() {
                let entry = self.entries.remove(index);
                self.entries.push(entry);
                self.version = NEXT_NAMESPACE_VERSION.fetch_add(1, Ordering::Relaxed);
            }
        }
        previous
    }

    /// Unbind `name`, returning its value, owned, or NULL when the namespace
    /// does not bind it. The cell goes back to inheriting the builtin.
    pub fn take(&mut self, name: &str) -> Value {
        let Some(index) = self.position(name) else {
            return std::ptr::null_mut();
        };
        let cell = unsafe { &mut *self.entries[index].cell };
        if !cell.bound() {
            return std::ptr::null_mut();
        }
        cell.own = 0;
        let inherited = unsafe { cell.inherited() };
        std::mem::replace(&mut cell.value, inherited)
    }

    /// Register a unit's static `cell` for `name`. A value the runtime bound
    /// before linking moves into it.
    fn link(&mut self, name: &str, cell: *mut GlobalCell, builtin: *mut GlobalCell, this: Value) -> Result<(), String> {
        unsafe {
            match self.position(name) {
                Some(index) => {
                    let entry = &mut self.entries[index];
                    if !entry.boxed {
                        if entry.cell == cell { return Ok(()); }
                        return Err(format!("global '{name}' is linked by two units"));
                    }
                    let previous = Box::from_raw(entry.cell);
                    *cell = GlobalCell { value: previous.value, own: previous.own, builtin, namespace: this };
                    entry.cell = cell;
                    entry.boxed = false;
                }
                None => {
                    *cell = GlobalCell::unbound(builtin, this);
                    self.entries.push(Entry { name: name.into(), cell, boxed: false });
                }
            }
            Ok(())
        }
    }

    /// The bound names and their borrowed values, in binding order.
    pub fn iter(&self) -> impl Iterator<Item = (&str, Value)> {
        self.entries.iter().filter_map(|entry| {
            let cell = unsafe { &*entry.cell };
            cell.bound().then_some((entry.name.as_str(), cell.value))
        })
    }

    pub fn len(&self) -> usize {
        self.iter().count()
    }

    pub fn is_empty(&self) -> bool {
        self.iter().next().is_none()
    }

    /// Detach every cell, returning the values it held, owned. Static cells
    /// outlive the namespace, so they are left unbound and unowned.
    fn clear(&mut self) -> Vec<Value> {
        self.version = NEXT_NAMESPACE_VERSION.fetch_add(1, Ordering::Relaxed);
        self.entries
            .iter()
            .map(|entry| {
                let cell = unsafe { &mut *entry.cell };
                cell.own = 0;
                cell.namespace = std::ptr::null_mut();
                std::mem::replace(&mut cell.value, undefined_value())
            })
            .collect()
    }
}

/// `repr(C)` fixes the variant tag and payload offsets so compiled code can
/// test an object's variant and read instance fields and class layouts
/// directly, as SBCL's generated code reads instance headers and slots.
/// The offsets compiled code uses are the `*_OFFSET` and `*_TAG` constants
/// after this enum, mirrored in `mule/compiler/vops.py`; the runtime's
/// tests check them against real objects.
#[repr(C)]
pub enum Data {
    None,
    Bool(bool),
    /// The class of the value, or NULL for the builtin class itself.
    /// Subclass values keep their native storage, a class reference, and
    /// an owned attribute namespace created on first assignment.
    BigInt(BigInt, Value, Value),
    Float(f64, Value, Value),
    Text(Text, Value, Value),
    Bytes(Vec<u8>, Value, Value),
    ForeignPointerVector {
        base: *const *const c_char,
        length: usize,
    },
    ForeignCString {
        address: *const c_char,
    },
    Function {
        entry: Native,
        globals: Value,
        name: String,
        defaults: Value,
        keyword_entry: Option<Native>,
        /// Captured cells and values for a nested function, or NULL.
        closure: Value,
        /// A static native boolean per closure entry: true for a shared cell,
        /// false for a value captured directly. NULL without a closure.
        closure_cells: *const bool,
        /// The function's `__dict__` namespace, created on first assignment.
        attributes: Value,
        /// Deferred annotation callable and lazy cache, owned independently.
        annotate: Value,
        annotations: Value,
        code_info: *const crate::traceback::CodeInfo,
        positional_defaults: Value,
        keyword_defaults: Value,
    },
    Args(ValueBuffer, Value, Value),
    ArgumentView {
        values: *const Value,
        count: usize,
        defaults: Value,
        closure: Value,
        owned_mask: Cell<u64>,
    },
    ReceiverArgs {
        receiver: Value,
        values: *const Value,
        count: usize,
        defaults: Value,
        closure: Value,
        owned_mask: Cell<u64>,
    },
    KeywordArgs {
        receiver: Value,
        values: *const Value,
        count: usize,
        names: *const *const c_char,
        keyword_values: *const Value,
        keyword_count: usize,
        defaults: Value,
        closure: Value,
    },
    List(ValueBuffer, Value, Value),
    Namespace(Namespace),
    Class(Box<crate::classes::ClassLayout>),
    Instance {
        ty: Value,
        fields: Box<[Value]>,
        /// Attributes outside the fixed fields (the instance `__dict__`), as
        /// a namespace created on first assignment, or NULL.
        attributes: Value,
    },
    NativeInstance {
        ty: Value,
        payload: Box<[i64]>,
    },
    BoundMethod {
        receiver: Value,
        method: Value,
    },
    /// A weak reference's link to its referent: `referent` is borrowed and
    /// becomes NULL when the referent is freed; `owner` is the borrowed
    /// compiled `ref` object that owns this handle and is passed to the
    /// owned `callback`.
    WeakHandle {
        referent: Value,
        callback: Value,
        owner: Value,
    },
    /// An exported buffer's physical lifetime guard, owning its backing object.
    BufferLease { owner: Value },
    /// Fast sequence iterator over an exact list or tuple.
    SeqIter {
        sequence: Value,
        index: usize,
    },
    DescriptorLease(Option<std::fs::File>),
    /// A generator's suspended execution on its own native stack.
    Coroutine(Box<crate::coroutine::Coroutine>),
    /// What an unbound global's cell holds. Code checking for unbound names
    /// compares against it; using it unchecked raises `NameError`.
    Undefined,
    /// One entry of an exception's traceback (see `traceback.rs`): the
    /// frame (lazily materialized on access), its code and globals, its line,
    /// and the entry nearer the raise, or NULL.
    Traceback {
        frame: Cell<Value>,
        code: *const crate::traceback::CodeInfo,
        globals: Value,
        line: i64,
        /// Constant compiler-planned highlight, or NULL without source ranges.
        marker: *const c_char,
        next: Value,
        /// The call that recorded the entry, compared, never dereferenced.
        activation: usize,
    },
    /// A function call an exception passed through.
    Frame {
        code: *const crate::traceback::CodeInfo,
        line: i64,
        globals: Value,
        /// The calling frame, or NULL until the exception reaches it.
        back: Value,
    },
}

/// Byte offsets compiled code reads, from the object address. `Data`'s
/// `repr(C)` tag is a `u32` at `DATA_OFFSET`; payload fields follow it.
pub const REFERENCES_OFFSET: usize = 0;
pub const TRACKED_OFFSET: usize = 8;
pub const GC_SLOT_OFFSET: usize = 12;
pub const DATA_OFFSET: usize = 16;
pub const BIGINT_TAG: u32 = 2;
pub const BIGINT_VALUE_OFFSET: usize = 24;
pub const BIGINT_CLASS_OFFSET: usize = 40;
pub const BIGINT_ATTRIBUTES_OFFSET: usize = 48;
pub const FLOAT_TAG: u32 = 3;
pub const FLOAT_VALUE_OFFSET: usize = 24;
pub const FLOAT_CLASS_OFFSET: usize = 32;
pub const FLOAT_ATTRIBUTES_OFFSET: usize = 40;
pub const INSTANCE_TAG: u32 = 16;
pub const NATIVE_INSTANCE_TAG: u32 = 17;
pub const NATIVE_INSTANCE_CLASS_OFFSET: usize = 24;
pub const NATIVE_INSTANCE_PAYLOAD_OFFSET: usize = 32;
pub const NATIVE_INSTANCE_LENGTH_OFFSET: usize = 40;
pub const CLASS_TAG: u32 = 15;
/// `Data::Instance.ty`, the class object.
pub const INSTANCE_CLASS_OFFSET: usize = 24;
/// The data pointer of `Data::Instance.fields`.
pub const INSTANCE_FIELDS_OFFSET: usize = 32;
/// `Data::Instance.attributes`, NULL without an instance `__dict__`.
pub const INSTANCE_ATTRIBUTES_OFFSET: usize = 48;
/// The `Box<ClassLayout>` pointer of `Data::Class`.
pub const CLASS_LAYOUT_OFFSET: usize = 24;

impl Data {
    /// Physical ownership edges only: borrowed globals, class MRO links,
    /// argument buffers and weak targets are deliberately excluded.
    pub fn owned_values(&self) -> Vec<Value> {
        let mut values = Vec::new();
        self.visit_owned_values(|value| values.push(value));
        values
    }

    pub fn visit_owned_values(&self, mut visit: impl FnMut(Value)) {
        match self {
            Self::Args(values, ty, attributes) | Self::List(values, ty, attributes) => {
                visit(*ty); visit(*attributes);
                for value in values.iter().rev() { visit(*value); }
            }
            Self::BigInt(_, ty, attributes) | Self::Float(_, ty, attributes)
            | Self::Text(_, ty, attributes) | Self::Bytes(_, ty, attributes) => { visit(*ty); visit(*attributes); }
            Self::Namespace(namespace) => {
                for entry in &namespace.entries { visit(unsafe { (*entry.cell).value }); }
            }
            Self::Class(layout) => {
                for base in &layout.bases { visit(*base); }
                visit(layout.metaclass);
                for (_, value) in &layout.attributes { visit(*value); }
            }
            Self::Instance { ty, fields, attributes } => {
                visit(*ty); visit(*attributes);
                for field in fields { visit(*field); }
            }
            Self::NativeInstance { ty, .. } => visit(*ty),
            Self::BoundMethod { receiver, method } => { visit(*receiver); visit(*method); }
            Self::WeakHandle { callback, .. } => visit(*callback),
            Self::BufferLease { owner } => visit(*owner),
            Self::SeqIter { sequence, .. } => visit(*sequence),
            Self::Traceback { frame, globals, next, .. } => { visit(frame.get()); visit(*globals); visit(*next); }
            Self::Frame { globals, back, .. } => { visit(*globals); visit(*back); }
            Self::Function { defaults, closure, attributes, annotate, annotations, positional_defaults, keyword_defaults, .. } => {
                for value in [*defaults, *closure, *attributes, *annotate, *annotations, *positional_defaults, *keyword_defaults] { visit(value); }
            }
            Self::None | Self::Bool(_) | Self::DescriptorLease(_) | Self::ForeignPointerVector { .. } | Self::ForeignCString { .. }
            | Self::ArgumentView { .. } | Self::ReceiverArgs { .. } | Self::KeywordArgs { .. }
            | Self::Coroutine(_) | Self::Undefined => (),
        }
    }

    #[inline]
    pub fn may_cycle(value: Value) -> bool {
        !value.is_null()
            && !is_fixnum(value)
            && unsafe { (*value).tracked || matches!(&(*value).data, Self::List(..)) }
    }

    pub fn cycle_candidate(&self) -> bool {
        match self {
            Self::BigInt(_, ty, attributes) | Self::Float(_, ty, attributes)
            | Self::Text(_, ty, attributes) | Self::Bytes(_, ty, attributes) => !ty.is_null() || !attributes.is_null(),
            Self::Args(values, ty, attributes) => {
                !ty.is_null()
                    || !attributes.is_null()
                    || values.iter().any(|&v| Self::may_cycle(v))
            }
            Self::BoundMethod { receiver, .. } => {
                !receiver.is_null()
                    && !is_fixnum(*receiver)
                    && unsafe { (**receiver).tracked }
            }
            Self::List(..) | Self::Namespace(_) | Self::Class(_) | Self::Instance { .. }
            | Self::NativeInstance { .. } | Self::WeakHandle { .. } | Self::BufferLease { .. }
            | Self::Traceback { .. } | Self::Frame { .. } | Self::Function { .. } => true,
            Self::None | Self::Bool(_) | Self::DescriptorLease(_) | Self::ForeignPointerVector { .. } | Self::ForeignCString { .. }
            | Self::ArgumentView { .. } | Self::ReceiverArgs { .. } | Self::KeywordArgs { .. }
            | Self::SeqIter { .. } | Self::Coroutine(_) | Self::Undefined => false,
        }
    }
}

#[inline]
pub unsafe fn track_if_needed(container: Value, element: Value) {
    unsafe {
        if !(*container).tracked && Data::may_cycle(element) {
            (*container).tracked = true;
            crate::gc::track(container);
        }
    }
}
#[derive(Clone, Copy)]
struct FrameState {
    depth: u32,
    has_native_error: bool,
    has_pending_exception: bool,
}

thread_local! {
    static ERROR: RefCell<Option<(String, String)>> = const { RefCell::new(None) };
    static FRAME_STATE: Cell<FrameState> = const {
        Cell::new(FrameState {
            depth: 0,
            has_native_error: false,
            has_pending_exception: false,
        })
    };
}
#[inline(always)]
pub fn set_has_pending_exception(pending: bool) {
    FRAME_STATE.with(|state| {
        let mut current = state.get();
        current.has_pending_exception = pending;
        state.set(current);
    });
}
pub fn error(kind: &str, message: &str) {
    if crate::sequences::has_pending() {
        return;
    }
    ERROR.with_borrow_mut(|current| {
        if current.is_none() {
            *current = Some((kind.into(), message.into()));
            FRAME_STATE.with(|state| {
                let mut st = state.get();
                st.has_native_error = true;
                state.set(st);
            });
            crate::traceback::clear();
        }
    });
}
#[inline(always)]
pub fn has_error() -> bool {
    FRAME_STATE.with(|state| {
        let st = state.get();
        st.has_native_error || st.has_pending_exception
    })
}
pub fn clear_error() {
    ERROR.with_borrow_mut(|current| *current = None);
    FRAME_STATE.with(|state| {
        let mut st = state.get();
        st.has_native_error = false;
        state.set(st);
    });
    crate::sequences::clear_pending();
    crate::traceback::clear();
}
/// The current error, removed so code can run and later restore it.
pub struct SavedError {
    native: Option<(String, String)>,
    pending: Value,
    traceback: Value,
}
pub fn save_error() -> SavedError {
    let native = ERROR.with_borrow_mut(Option::take);
    FRAME_STATE.with(|state| {
        let mut st = state.get();
        st.has_native_error = false;
        state.set(st);
    });
    SavedError {
        native,
        pending: crate::sequences::take_pending(),
        traceback: crate::traceback::take(),
    }
}
pub fn restore_error(saved: SavedError) {
    let has_native = saved.native.is_some();
    ERROR.with_borrow_mut(|current| *current = saved.native);
    FRAME_STATE.with(|state| {
        let mut st = state.get();
        st.has_native_error = has_native;
        state.set(st);
    });
    crate::sequences::restore_pending(saved.pending);
    crate::traceback::restore(saved.traceback);
}
pub fn take_native_error() -> Option<(String, String)> {
    let taken = ERROR.with_borrow_mut(Option::take);
    FRAME_STATE.with(|state| {
        let mut st = state.get();
        st.has_native_error = false;
        state.set(st);
    });
    taken
}
/// Clear a pending error of class `kind` (or a subclass), whether the
/// runtime raised it or compiled code did, reporting whether one was
/// cleared. `getattr` with a default and `hasattr` use it: they swallow any
/// `AttributeError`, including one a property or `__getattr__` raised.
pub fn clear_native_error_of(kind: &str) -> bool {
    if crate::sequences::has_pending() {
        return unsafe { crate::sequences::clear_pending_instance_of(kind) };
    }
    clear_runtime_error_of(kind)
}
/// Clear an error of class `kind` only when the runtime itself raised it,
/// reporting whether it did. An exception raised by compiled code stays
/// pending: a lookup that reports "not found" must not hide it, or the
/// caller's fallback runs the code that raised it a second time.
pub fn clear_runtime_error_of(kind: &str) -> bool {
    if crate::sequences::has_pending() {
        return false;
    }
    let matched = ERROR.with_borrow(|current| current.as_ref().is_some_and(|value| value.0 == kind));
    if matched {
        clear_error();
    }
    matched
}
pub fn error_kind() -> String {
    if crate::sequences::has_pending() {
        return crate::sequences::pending_kind();
    }
    ERROR.with_borrow(|current| {
        current
            .as_ref()
            .map_or_else(String::new, |value| value.0.clone())
    })
}
pub fn error_message() -> String {
    if crate::sequences::has_pending() {
        return crate::sequences::pending_message();
    }
    ERROR.with_borrow(|current| {
        current
            .as_ref()
            .map_or_else(String::new, |value| value.1.clone())
    })
}
pub fn is_fixnum(value: Value) -> bool {
    value as usize & 1 != 0
}
pub fn small(value: i64) -> Value {
    (((value as u64) << 1) | 1) as Value
}
/// Only tagged values may be decoded; heap values must never be dereferenced as tags.
pub unsafe fn fixnum(value: Value) -> i64 {
    (value as isize >> 1) as i64
}
pub unsafe fn new_value(data: Data) -> Value {
    if let Data::Text(ref text, ty, attrs) = data {
        if ty.is_null() && attrs.is_null() {
            if text.is_empty() {
                return empty_text();
            }
            if text.len() == 1 {
                if let Some(code) = text.get(0) {
                    if code <= 0xff {
                        return latin1_character(code as u8);
                    }
                }
            }
        }
    }
    let tracked = data.cycle_candidate();
    let value = unsafe {
        alloc_object(Object {
            references: std::sync::atomic::AtomicUsize::new(1),
            tracked,
            gc_slot: 0,
            data,
        })
    };
    if tracked { crate::gc::track(value); }
    value
}

const OBJECT_POOL_CAP: usize = 512;
const TUPLE2_POOL_CAP: usize = 1024;

struct ObjectPool {
    slots: [*mut Object; OBJECT_POOL_CAP],
    len: usize,
    tuple2_slots: [*mut Object; TUPLE2_POOL_CAP],
    tuple2_len: usize,
}

impl ObjectPool {
    const fn new() -> Self {
        Self {
            slots: [std::ptr::null_mut(); OBJECT_POOL_CAP],
            len: 0,
            tuple2_slots: [std::ptr::null_mut(); TUPLE2_POOL_CAP],
            tuple2_len: 0,
        }
    }

    #[cold]
    #[inline(never)]
    unsafe fn flush(&mut self) {
        unsafe {
            while self.len > 0 {
                self.len -= 1;
                let ptr = *self.slots.get_unchecked(self.len);
                std::alloc::dealloc(ptr.cast::<u8>(), std::alloc::Layout::new::<Object>());
            }
            while self.tuple2_len > 0 {
                self.tuple2_len -= 1;
                let ptr = *self.tuple2_slots.get_unchecked(self.tuple2_len);
                std::ptr::drop_in_place(&mut (*ptr).data);
                std::alloc::dealloc(ptr.cast::<u8>(), std::alloc::Layout::new::<Object>());
            }
        }
    }
}

std::thread_local! {
    static OBJECT_POOL: std::cell::UnsafeCell<ObjectPool> = const {
        std::cell::UnsafeCell::new(ObjectPool::new())
    };
}

#[cold]
#[inline(never)]
pub unsafe fn flush_object_pool() {
    unsafe {
        let cell = OBJECT_POOL.with(|c| c.get());
        (*cell).flush();
    }
}

/// Acquire aligned storage without creating a Rust Object value.
/// The caller must initialize all active fields before any Object access.
/// Allocation failure aborts, as it does for the existing Box-based allocator.
///
/// # Safety
/// The caller must initialize the storage or return it through the raw storage
/// deallocator. No Object reference may observe partially initialized fields.
#[inline(always)]
pub unsafe fn allocate_object_storage() -> Value {
    unsafe {
        let cell = OBJECT_POOL.with(|c| c.get());
        let pool = &mut *cell;
        if pool.len > 0 {
            pool.len -= 1;
            return *pool.slots.get_unchecked(pool.len);
        }
        let layout = std::alloc::Layout::new::<Object>();
        let ptr = std::alloc::alloc(layout).cast::<Object>();
        if ptr.is_null() {
            std::alloc::handle_alloc_error(layout);
        }
        ptr
    }
}

#[inline(always)]
unsafe fn alloc_object(obj: Object) -> Value {
    unsafe {
        let ptr = allocate_object_storage();
        ptr.write(obj);
        ptr
    }
}

#[inline(always)]
unsafe fn alloc_tuple2(v0: Value, v1: Value, tracked: bool) -> Value {
    unsafe {
        let cell = OBJECT_POOL.with(|c| c.get());
        let pool = &mut *cell;
        if pool.tuple2_len > 0 {
            pool.tuple2_len -= 1;
            let ptr = *pool.tuple2_slots.get_unchecked(pool.tuple2_len);
            (*ptr).references.store(1, std::sync::atomic::Ordering::Relaxed);
            (*ptr).tracked = tracked;
            (*ptr).gc_slot = 0;
            if let Data::Args(ref mut values, ..) = (*ptr).data {
                let buf = values.as_mut_ptr();
                *buf = v0;
                *buf.add(1) = v1;
                values.set_len(2);
                if tracked {
                    crate::gc::track(ptr);
                }
                return ptr;
            }
        }
        let mut values = Vec::new();
        if values.try_reserve_exact(2).is_err() {
            release(v0);
            release(v1);
            error("MemoryError", "sequence allocation failed");
            pool.flush();
            return std::ptr::null_mut();
        }
        values.push(v0);
        values.push(v1);
        let value = alloc_object(Object {
            references: std::sync::atomic::AtomicUsize::new(1),
            tracked,
            gc_slot: 0,
            data: Data::Args(values.into(), std::ptr::null_mut(), std::ptr::null_mut()),
        });
        if tracked {
            crate::gc::track(value);
        }
        value
    }
}

#[inline(always)]
unsafe fn dealloc_object(value: Value) {
    unsafe {
        let cell = OBJECT_POOL.with(|c| c.get());
        let pool = &mut *cell;
        if !has_error() && pool.len < OBJECT_POOL_CAP {
            *pool.slots.get_unchecked_mut(pool.len) = value;
            pool.len += 1;
            return;
        }
        std::alloc::dealloc(value.cast::<u8>(), std::alloc::Layout::new::<Object>());
    }
}

/// End a raw storage allocation without inspecting or dropping its payload.
///
/// # Safety
/// The caller must own this allocation exclusively and complete every required
/// child, weak-reference, finalization, and GC action before calling this function.
/// Uninitialized storage and trivially droppable scalar data need no payload drop.
#[inline(always)]
pub unsafe fn deallocate_object_storage(value: Value) {
    unsafe { dealloc_object(value) }
}

#[inline(always)]
unsafe fn drop_and_dealloc_object(value: Value) {
    unsafe {
        if !has_error() {
            if let Data::Args(ref mut values, ty, attrs) = (*value).data {
                if ty.is_null() && attrs.is_null() && values.capacity() == 2 {
                    let cell = OBJECT_POOL.with(|c| c.get());
                    let pool = &mut *cell;
                    if pool.tuple2_len < TUPLE2_POOL_CAP {
                        values.set_len(0);
                        *pool.tuple2_slots.get_unchecked_mut(pool.tuple2_len) = value;
                        pool.tuple2_len += 1;
                        return;
                    }
                }
            }
        }
        std::ptr::drop_in_place(&mut (*value).data);
        dealloc_object(value);
    }
}

impl Object {
    pub fn reference_count(&self) -> usize {
        self.references.load(std::sync::atomic::Ordering::Relaxed)
    }

    fn remove_reference(&self) -> bool {
        use std::sync::atomic::{Ordering, fence};
        let previous = self.references.fetch_sub(1, Ordering::Release);
        assert!(previous != 0, "release of an unowned heap value");
        if previous != 1 { return false; }
        fence(Ordering::Acquire);
        true
    }
}

#[inline(always)]
pub unsafe fn retain(value: Value) -> Value {
    unsafe {
        if !value.is_null() && !is_fixnum(value) && (*value).reference_count() != usize::MAX {
            let previous = (*value).references.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            assert!(
                previous != 0 && previous < isize::MAX as usize,
                "invalid or overflowing heap reference count"
            );
        }
        value
    }
}
#[inline(always)]
pub unsafe fn release(value: Value) {
    unsafe {
        if value.is_null() || is_fixnum(value) {
            return;
        }
        let count = (*value).reference_count();
        if count == usize::MAX {
            return;
        }
        if count == 1 {
            std::sync::atomic::fence(std::sync::atomic::Ordering::Acquire);
            destroy(value);
            return;
        }
        if !(*value).remove_reference() { return; }
        destroy(value);
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_destroy(value: Value) {
    unsafe {
        std::sync::atomic::fence(std::sync::atomic::Ordering::Acquire);
        destroy(value);
    }
}

#[cold]
#[inline(never)]
unsafe fn destroy(value: Value) {
    unsafe {
        if crate::weakref::has_references(value) {
            // Callbacks see the referent already cleared; they may resurrect
            // nothing, because no reference to it remains reachable.
            (*value).references.store(1, std::sync::atomic::Ordering::Relaxed);
            crate::weakref::clear(value);
            if !(*value).remove_reference() { return; }
        }
        if (*value).tracked {
            crate::gc::untrack(value);
        }
        match &(*value).data {
            Data::Text(text, ty, attrs) if ty.is_null() && attrs.is_null() => {
                if text.is_inline() {
                    dealloc_object(value);
                } else {
                    drop_and_dealloc_object(value);
                }
                return;
            }
            Data::Float(_, ty, attrs) if ty.is_null() && attrs.is_null() => {
                dealloc_object(value);
                return;
            }
            Data::SeqIter { sequence, .. } => {
                let seq = *sequence;
                dealloc_object(value);
                release(seq);
                return;
            }
            Data::Bytes(_, ty, attrs) | Data::BigInt(_, ty, attrs)
                if ty.is_null() && attrs.is_null() =>
            {
                drop_and_dealloc_object(value);
                return;
            }
            Data::List(values, ty, attrs) | Data::Args(values, ty, attrs)
                if ty.is_null() && attrs.is_null() =>
            {
                (*value).references.store(0, std::sync::atomic::Ordering::Relaxed);
                // Child weak-reference callbacks observe Python's reverse teardown order.
                for &child in values.iter().rev() {
                    release(child);
                }
                drop_and_dealloc_object(value);
                return;
            }
            Data::Instance {
                ty,
                fields,
                attributes,
            } if attributes.is_null() => {
                (*value).references.store(0, std::sync::atomic::Ordering::Relaxed);
                release(*ty);
                for &field in fields.iter() {
                    release(field);
                }
                drop_and_dealloc_object(value);
                return;
            }
            _ => (),
        }
        (*value).references.store(0, std::sync::atomic::Ordering::Relaxed);
        let data = std::ptr::read(&(*value).data);
        dealloc_object(value);
        release_data(value, data);
    }
}

#[inline]
pub unsafe fn new_untracked_value(data: Data) -> Value {
    unsafe {
        alloc_object(Object {
            references: std::sync::atomic::AtomicUsize::new(1),
            tracked: false,
            gc_slot: 0,
            data,
        })
    }
}

/// Drop detached physical storage using the same ownership enumeration as GC.
pub unsafe fn release_data(value: Value, mut data: Data) {
    unsafe {
        match &mut data {
            Data::Namespace(namespace) => {
                for child in namespace.clear() { release(child); }
                return;
            }
            Data::Class(layout) => {
                for base in &layout.bases {
                    if let Data::Class(parent) = &mut (**base).data { parent.remove_subclass(value); }
                }
                // Compiled attribute sites compare layout addresses; a later
                // class may reuse this one's.
                crate::classes::invalidate_attribute_caches();
            }
            Data::WeakHandle { referent, .. } => {
                crate::weakref::unregister(*referent, value);
            }
            Data::BufferLease { owner } => crate::buffers::release(*owner),
            _ => (),
        }
        data.visit_owned_values(|child| release(child));
    }
}
/// This private wrapper is used only for frozen None/Bool/Undefined objects. These variants
/// own no pointers or mutable storage, and their immortal reference count is
/// never modified by retain/release. No other Object variants may be stored here.
#[repr(transparent)]
struct ImmortalObject(Object);
unsafe impl Send for ImmortalObject {}
unsafe impl Sync for ImmortalObject {}

#[unsafe(export_name = "mule.undefined")]
static UNDEFINED: ImmortalObject = ImmortalObject(Object {
    references: std::sync::atomic::AtomicUsize::new(usize::MAX),
    tracked: false,
    gc_slot: 0,
    data: Data::Undefined,
});
pub fn undefined_value() -> Value {
    std::ptr::addr_of!(UNDEFINED.0).cast_mut()
}
pub fn is_undefined(value: Value) -> bool {
    std::ptr::eq(value, undefined_value())
}
#[unsafe(export_name = "mule.none")]
static NONE: ImmortalObject = ImmortalObject(Object {
    references: std::sync::atomic::AtomicUsize::new(usize::MAX),
    tracked: false,
    gc_slot: 0,
    data: Data::None,
});
#[unsafe(export_name = "mule.false")]
static FALSE: ImmortalObject = ImmortalObject(Object {
    references: std::sync::atomic::AtomicUsize::new(usize::MAX),
    tracked: false,
    gc_slot: 0,
    data: Data::Bool(false),
});
#[unsafe(export_name = "mule.true")]
static TRUE: ImmortalObject = ImmortalObject(Object {
    references: std::sync::atomic::AtomicUsize::new(usize::MAX),
    tracked: false,
    gc_slot: 0,
    data: Data::Bool(true),
});
pub unsafe fn none_value() -> Value {
    std::ptr::addr_of!(NONE.0).cast_mut()
}
pub unsafe fn bool_value(value: bool) -> Value {
    if value {
        std::ptr::addr_of!(TRUE.0).cast_mut()
    } else {
        std::ptr::addr_of!(FALSE.0).cast_mut()
    }
}
static EMPTY_TEXT: std::sync::OnceLock<ImmortalObject> = std::sync::OnceLock::new();
static LATIN1_CHARACTERS: std::sync::OnceLock<Box<[ImmortalObject; 256]>> =
    std::sync::OnceLock::new();

pub fn empty_text() -> Value {
    let entry = EMPTY_TEXT.get_or_init(|| {
        let text = Text::empty();
        let _ = text.hash();
        ImmortalObject(Object {
            references: std::sync::atomic::AtomicUsize::new(usize::MAX),
            tracked: false,
            gc_slot: 0,
            data: Data::Text(text, std::ptr::null_mut(), std::ptr::null_mut()),
        })
    });
    std::ptr::from_ref(&entry.0).cast_mut()
}

pub fn latin1_character(byte: u8) -> Value {
    let table = LATIN1_CHARACTERS.get_or_init(|| {
        Box::new(std::array::from_fn(|code| {
            let text = Text::from_codepoints(std::iter::once(code as u32))
                .expect("single-character Latin-1 allocation");
            let _ = text.hash();
            ImmortalObject(Object {
                references: std::sync::atomic::AtomicUsize::new(usize::MAX),
                tracked: false,
                gc_slot: 0,
                data: Data::Text(text, std::ptr::null_mut(), std::ptr::null_mut()),
            })
        }))
    });
    std::ptr::from_ref(&table[byte as usize].0).cast_mut()
}
pub unsafe fn namespace() -> Value {
    unsafe { new_value(Data::Namespace(Namespace::default())) }
}
pub unsafe fn list() -> Value {
    unsafe { new_value(Data::List(ValueBuffer::new(), std::ptr::null_mut(), std::ptr::null_mut())) }
}
pub unsafe fn list_append(list: Value, value: Value) -> bool {
    unsafe {
        if list.is_null() || is_fixnum(list) || !matches!(&(*list).data, Data::List(..)) {
            error("TypeError", "list append requires a list");
            return false;
        }
        match &mut (*list).data {
            Data::List(values, ..) => {
                values.push(retain(value));
                track_if_needed(list, value);
                true
            }
            _ => {
                error("TypeError", "list append requires a list");
                false
            }
        }
    }
}
/// Lookup borrows a binding; callers retain it before callbacks or mutation.
pub unsafe fn lookup(namespace: Value, name: &str) -> Value {
    unsafe {
        if !namespace.is_null() && !is_fixnum(namespace) {
            if let Data::Namespace(bindings) = &(*namespace).data {
                return bindings.get(name);
            }
        }
        error("TypeError", "expected a Mule namespace");
        std::ptr::null_mut()
    }
}
pub unsafe fn bind(namespace: Value, name: &str, value: Value) -> bool {
    unsafe {
        if !namespace.is_null()
            && !is_fixnum(namespace)
            && !value.is_null()
            && matches!(&(*namespace).data, Data::Namespace(_))
        {
            if let Data::Namespace(bindings) = &mut (*namespace).data {
                let previous = bindings.replace(name, retain(value), namespace);
                release(previous);
                return true;
            }
        }
        error("TypeError", "invalid namespace binding");
        false
    }
}
pub unsafe fn function(globals: Value, entry: Native, name: &str) -> Value {
    unsafe {
        new_value(Data::Function {
            entry,
            globals,
            name: name.into(),
            defaults: std::ptr::null_mut(),
            keyword_entry: None,
            closure: std::ptr::null_mut(),
            closure_cells: std::ptr::null(),
            attributes: std::ptr::null_mut(),
            annotate: std::ptr::null_mut(),
            annotations: std::ptr::null_mut(),
            code_info: std::ptr::null(),
            positional_defaults: std::ptr::null_mut(),
            keyword_defaults: std::ptr::null_mut(),
        })
    }
}

/// The attribute namespace of a builtin-storage subclass with an instance
/// dictionary, or NULL otherwise. Created on demand when `create` is set.
pub unsafe fn typed_attributes(value: Value, create: bool) -> Value {
    unsafe {
        if value.is_null() || is_fixnum(value) {
            return std::ptr::null_mut();
        }
        let (class, attributes) = match &mut (*value).data {
            Data::BigInt(_, class, attributes)
            | Data::Float(_, class, attributes)
            | Data::Text(_, class, attributes)
            | Data::Bytes(_, class, attributes)
            | Data::Args(_, class, attributes)
            | Data::List(_, class, attributes) => (class, attributes),
            _ => return std::ptr::null_mut(),
        };
        if class.is_null() || is_fixnum(*class) {
            return std::ptr::null_mut();
        }
        let Data::Class(layout) = &(**class).data else {
            return std::ptr::null_mut();
        };
        if !layout.allows_instance_dictionary() {
            return std::ptr::null_mut();
        }
        if attributes.is_null() && create {
            *attributes = namespace();
            if !(*value).tracked {
                (*value).tracked = true;
                crate::gc::track(value);
            }
        }
        *attributes
    }
}

/// A function's own attribute: an assigned one, or one of the attributes
/// every function has. NULL without an error when it has neither.
pub unsafe fn function_attribute(function: Value, attribute: &str) -> Value {
    unsafe {
        let Data::Function {
            globals,
            name,
            attributes,
            closure,
            closure_cells,
            ..
        } = &(*function).data
        else {
            return std::ptr::null_mut();
        };
        if !attributes.is_null() {
            let value = lookup(*attributes, attribute);
            if !value.is_null() {
                return retain(value);
            }
        }
        match attribute {
            "__name__" | "__qualname__" => new_value(Data::Text(crate::text::Text::from_rust_str(name), std::ptr::null_mut(), std::ptr::null_mut())),
            "__module__" => {
                if globals.is_null() {
                    return retain(none_value());
                }
                let module = lookup(*globals, "__name__");
                retain(if module.is_null() { none_value() } else { module })
            }
            "__doc__" => none_value(),
            "__closure__" => closure_cells_of(*closure, *closure_cells),
            _ => std::ptr::null_mut(),
        }
    }
}

/// Assign a function attribute in its `__dict__`.
pub unsafe fn set_function_attribute(function: Value, attribute: &str, value: Value) -> bool {
    unsafe {
        let Data::Function { attributes, name, .. } = &mut (*function).data else {
            return false;
        };
        if attribute == "__name__" || attribute == "__qualname__" {
            if let Data::Text(text, ..) = &(*value).data {
                if attribute == "__name__" {
                    if let Some(text) = text.to_rust_string() {
                        *name = text;
                    }
                    return true;
                }
            }
        }
        if attributes.is_null() {
            *attributes = namespace();
        }
        bind(*attributes, attribute, value)
    }
}
pub unsafe fn argument(args: Value, index: usize) -> Option<Value> {
    unsafe {
        if !args.is_null() && !is_fixnum(args) {
            if let Data::Args(values, ..) = &(*args).data {
                return values.get(index).copied();
            }
            match &(*args).data {
                Data::ArgumentView { values, count, .. } if index < *count => {
                    return Some(*values.add(index));
                }
                Data::ReceiverArgs { receiver, .. } if index == 0 => return Some(*receiver),
                Data::ReceiverArgs { values, count, .. } if index <= *count => {
                    return Some(*values.add(index - 1));
                }
                Data::KeywordArgs {
                    receiver,
                    values,
                    count,
                    ..
                } => {
                    let offset = usize::from(!receiver.is_null());
                    if offset == 1 && index == 0 {
                        return Some(*receiver);
                    }
                    if index >= offset && index - offset < *count {
                        return Some(*values.add(index - offset));
                    }
                }
                _ => (),
            }
        }
        error("SystemError", "native arguments must be an argument view");
        None
    }
}
pub unsafe fn argument_count(args: Value) -> Option<usize> {
    unsafe {
        if !args.is_null() && !is_fixnum(args) {
            if let Data::Args(values, ..) = &(*args).data {
                return Some(values.len());
            }
            match &(*args).data {
                Data::ArgumentView { count, .. } => return Some(*count),
                Data::KeywordArgs {
                    receiver, count, ..
                } => return Some(*count + usize::from(!receiver.is_null())),
                Data::ReceiverArgs { count, .. } => return Some(*count + 1),
                _ => (),
            }
        }
        error("SystemError", "native arguments must be an argument view");
        None
    }
}
pub unsafe fn call(callable: Value, args: &[Value]) -> Value {
    unsafe {
        if has_error() {
            return std::ptr::null_mut();
        }
        if callable.is_null() || is_fixnum(callable) {
            error("TypeError", "object is not callable");
            return std::ptr::null_mut();
        }
        match &(*callable).data {
            Data::Class(_) => return crate::classes::construct(callable, args),
            Data::BoundMethod { receiver, method } => {
                return call_with_receiver(*method, *receiver, args);
            }
            Data::Instance { .. } | Data::NativeInstance { .. } => {
                let result = special(callable, "__call__", args);
                if result.is_null() && !has_error() {
                    error("TypeError", "object is not callable");
                }
                return result;
            }
            Data::Function { .. } => (),
            Data::Undefined => {
                error("NameError", "name is not defined");
                return std::ptr::null_mut();
            }
            _ => {
                error("TypeError", "object is not callable");
                return std::ptr::null_mut();
            }
        }
        let (entry, globals, defaults, closure) = match &(*callable).data {
            Data::Function {
                entry,
                globals,
                defaults,
                closure,
                ..
            } => (*entry, *globals, *defaults, *closure),
            _ => unreachable!(),
        };
        if mule_enter_frame() < 0 {
            return std::ptr::null_mut();
        }
        // This stack object borrows the argument slice without allocating a vector.
        // Native callbacks may retain individual arguments, never the argument view.
        let mut view = Object {
            references: std::sync::atomic::AtomicUsize::new(usize::MAX),
            tracked: false,
            gc_slot: 0,
            data: Data::ArgumentView {
                values: args.as_ptr(),
                count: args.len(),
                defaults,
                closure,
                owned_mask: Cell::new(0),
            },
        };
        let result = entry(globals, &mut view);
        mule_leave_frame();
        if result.is_null() && !has_error() {
            error(
                "SystemError",
                "native function returned NULL without an error",
            );
        }
        result
    }
}
/// Invoke `callable` consuming the caller's owned references in `args`.
pub unsafe fn call_owned(callable: Value, args: &[Value]) -> Value {
    unsafe {
        if has_error() {
            for &arg in args {
                release(arg);
            }
            return std::ptr::null_mut();
        }
        if !callable.is_null() && !is_fixnum(callable) && args.len() < 64 {
            if let Data::Function {
                entry,
                globals,
                defaults,
                closure,
                ..
            } = &(*callable).data
            {
                if mule_enter_frame() < 0 {
                    for &arg in args {
                        release(arg);
                    }
                    return std::ptr::null_mut();
                }
                let mask = if args.is_empty() {
                    0u64
                } else {
                    (1u64 << args.len()) - 1
                };
                let mut view = Object {
                    references: std::sync::atomic::AtomicUsize::new(usize::MAX),
                    tracked: false,
                    gc_slot: 0,
                    data: Data::ArgumentView {
                        values: args.as_ptr(),
                        count: args.len(),
                        defaults: *defaults,
                        closure: *closure,
                        owned_mask: Cell::new(mask),
                    },
                };
                let result = (*entry)(*globals, &mut view);
                mule_leave_frame();
                if let Data::ArgumentView { owned_mask, .. } = &view.data {
                    let mut rem = owned_mask.get();
                    while rem != 0 {
                        let idx = rem.trailing_zeros() as usize;
                        rem &= rem - 1;
                        release(*args.get_unchecked(idx));
                    }
                }
                if result.is_null() && !has_error() {
                    error(
                        "SystemError",
                        "native function returned NULL without an error",
                    );
                }
                return result;
            }
            if let Data::BoundMethod { receiver, method } = &(*callable).data {
                return call_with_receiver_owned(*method, retain(*receiver), args);
            }
        }
        let result = call(callable, args);
        for &arg in args {
            release(arg);
        }
        result
    }
}
/// Invoke a native method with a borrowed receiver-prefixed argument view.
/// The common call path allocates neither an argument vector nor a bound method.
pub unsafe fn call_with_receiver(method: Value, receiver: Value, args: &[Value]) -> Value {
    unsafe {
        if has_error() {
            return std::ptr::null_mut();
        }
        if !method.is_null() && !is_fixnum(method) {
            if let Data::Function {
                entry,
                globals,
                defaults,
                closure,
                ..
            } = &(*method).data
            {
                if mule_enter_frame() < 0 {
                    return std::ptr::null_mut();
                }
                let mut view = Object {
                    references: std::sync::atomic::AtomicUsize::new(usize::MAX),
                    tracked: false,
                    gc_slot: 0,
                    data: Data::ReceiverArgs {
                        receiver,
                        values: args.as_ptr(),
                        count: args.len(),
                        defaults: *defaults,
                        closure: *closure,
                        owned_mask: Cell::new(0),
                    },
                };
                let result = entry(*globals, &mut view);
                mule_leave_frame();
                if result.is_null() && !has_error() {
                    error(
                        "SystemError",
                        "native method returned NULL without an error",
                    );
                }
                return result;
            }
            // A bound method's underlying callable need not be a plain
            // function: ``classmethod(SomeClass)`` binds the class itself,
            // and calling the method calls that callable with the receiver
            // as its first argument (constructing the class in that case).
            if matches!((*method).data, Data::Class(_) | Data::Instance { .. } | Data::NativeInstance { .. }) {
                let mut full = Vec::with_capacity(args.len() + 1);
                full.push(receiver);
                full.extend_from_slice(args);
                return call(method, &full);
            }
        }
        error("TypeError", "method is not a native function");
        std::ptr::null_mut()
    }
}
/// Invoke `method` consuming the caller's owned `receiver` and `args`.
pub unsafe fn call_with_receiver_owned(method: Value, receiver: Value, args: &[Value]) -> Value {
    unsafe {
        if has_error() {
            release(receiver);
            for &arg in args {
                release(arg);
            }
            return std::ptr::null_mut();
        }
        if !method.is_null() && !is_fixnum(method) && args.len() < 63 {
            if let Data::Function {
                entry,
                globals,
                defaults,
                closure,
                ..
            } = &(*method).data
            {
                if mule_enter_frame() < 0 {
                    release(receiver);
                    for &arg in args {
                        release(arg);
                    }
                    return std::ptr::null_mut();
                }
                let mask = (1u64 << (args.len() + 1)) - 1;
                let mut view = Object {
                    references: std::sync::atomic::AtomicUsize::new(usize::MAX),
                    tracked: false,
                    gc_slot: 0,
                    data: Data::ReceiverArgs {
                        receiver,
                        values: args.as_ptr(),
                        count: args.len(),
                        defaults: *defaults,
                        closure: *closure,
                        owned_mask: Cell::new(mask),
                    },
                };
                let result = (*entry)(*globals, &mut view);
                mule_leave_frame();
                if let Data::ReceiverArgs { owned_mask, .. } = &view.data {
                    let mut rem = owned_mask.get();
                    if (rem & 1) != 0 {
                        release(receiver);
                        rem &= !1;
                    }
                    while rem != 0 {
                        let idx = rem.trailing_zeros() as usize;
                        rem &= rem - 1;
                        release(*args.get_unchecked(idx - 1));
                    }
                }
                if result.is_null() && !has_error() {
                    error(
                        "SystemError",
                        "native method returned NULL without an error",
                    );
                }
                return result;
            }
        }
        let result = call_with_receiver(method, receiver, args);
        release(receiver);
        for &arg in args {
            release(arg);
        }
        result
    }
}
pub unsafe fn special(value: Value, name: &str, args: &[Value]) -> Value {
    unsafe {
        let method = crate::sequences::binding(value, name);
        if !method.is_null() {
            return crate::classes::call_attribute(
                method, value, crate::classes::class_of_value(value), args,
            );
        }
        if value.is_null() || is_fixnum(value) {
            return std::ptr::null_mut();
        }
        let ty = match &(*value).data {
            Data::Instance { ty, .. } | Data::NativeInstance { ty, .. } => *ty,
            Data::Class(_) => {
                // A class's special methods come from its metaclass.
                let bound = crate::classes::special_bound_on_class(value, name);
                if bound.is_null() {
                    return bound;
                }
                let result = call(bound, args);
                release(bound);
                return result;
            }
            _ => return std::ptr::null_mut(),
        };
        match &(*ty).data {
            Data::Class(_) => crate::classes::special_method(value, name, args),
            _ => unreachable!(),
        }
    }
}

/// A binary or comparison special method's result, or NULL without an error
/// when the method is absent or returns NotImplemented.
unsafe fn implemented(value: Value, name: &str, args: &[Value]) -> Value {
    unsafe {
        let result = special(value, name, args);
        if !result.is_null() && result == crate::classes::not_implemented() {
            release(result);
            return std::ptr::null_mut();
        }
        result
    }
}
pub unsafe fn truth(value: Value) -> Option<bool> {
    unsafe {
        if has_error() {
            return None;
        }
        if is_fixnum(value) {
            return Some(fixnum(value) != 0);
        }
        if value.is_null() {
            error("SystemError", "NULL truth operand");
            return None;
        }
        Some(match &(*value).data {
            Data::None => false,
            Data::Bool(value) => *value,
            Data::BigInt(integer, ty, ..) if ty.is_null() => integer.sign() != 0,
            Data::Float(value, ty, ..) if ty.is_null() => *value != 0.0,
            Data::Text(value, ty, ..) if ty.is_null() => !value.is_empty(),
            Data::Bytes(value, ty, ..) if ty.is_null() => !value.is_empty(),
            Data::Args(value, ty, ..) | Data::List(value, ty, ..) if ty.is_null() => !value.is_empty(),
            Data::Namespace(bindings) => !bindings.is_empty(),
            Data::Instance { .. }
            | Data::NativeInstance { .. }
            | Data::Class(_)
            | Data::BigInt(..)
            | Data::Float(..)
            | Data::Text(..)
            | Data::Bytes(..)
            | Data::Args(..)
            | Data::List(..) => {
                let result = special(value, "__bool__", &[]);
                if result.is_null() {
                    if has_error() {
                        return None;
                    }
                    let length = special(value, "__len__", &[]);
                    if length.is_null() {
                        return if has_error() { None } else { Some(true) };
                    }
                    return length_result(length).map(|length| length != 0);
                }
                let boolean = if !is_fixnum(result) {
                    match &(*result).data {
                        Data::Bool(value) => Some(*value),
                        _ => None,
                    }
                } else {
                    None
                };
                release(result);
                if boolean.is_none() {
                    error("TypeError", "__bool__ should return bool");
                }
                return boolean;
            }
            _ => true,
        })
    }
}
/// Validate and consume the owned result of the length protocol.
pub unsafe fn length_result(result: Value) -> Option<usize> {
    unsafe {
        if is_fixnum(result) {
            let length = fixnum(result);
            if length < 0 {
                error("ValueError", "__len__ must return a nonnegative integer");
                return None;
            }
            return Some(length as usize);
        }
        if !result.is_null() {
            if let Data::Bool(value) = &(*result).data {
                let length = usize::from(*value);
                release(result);
                return Some(length);
            }
        }
        let integer = numeric::integer_export(result);
        release(result);
        let integer = integer?;
        if integer.sign() < 0 {
            error("ValueError", "__len__ must return a nonnegative integer");
            return None;
        }
        let length = integer
            .to_u64()
            .and_then(|length| usize::try_from(length).ok());
        match length {
            Some(length) if length <= isize::MAX as usize => Some(length),
            _ => {
                error("OverflowError", "__len__ result is too large");
                None
            }
        }
    }
}
/// Python's binary-operator special methods by compiled operator code:
/// the forward, reflected, and in-place names, and the operator's symbol.
const BINARY_METHODS: [(&str, &str, &str, &str); 13] = [
    ("__add__", "__radd__", "__iadd__", "+"),
    ("__sub__", "__rsub__", "__isub__", "-"),
    ("__mul__", "__rmul__", "__imul__", "*"),
    ("__truediv__", "__rtruediv__", "__itruediv__", "/"),
    ("__floordiv__", "__rfloordiv__", "__ifloordiv__", "//"),
    ("__mod__", "__rmod__", "__imod__", "%"),
    ("__matmul__", "__rmatmul__", "__imatmul__", "@"),
    ("__pow__", "__rpow__", "__ipow__", "**"),
    ("__lshift__", "__rlshift__", "__ilshift__", "<<"),
    ("__rshift__", "__rrshift__", "__irshift__", ">>"),
    ("__and__", "__rand__", "__iand__", "&"),
    ("__xor__", "__rxor__", "__ixor__", "^"),
    ("__or__", "__ror__", "__ior__", "|"),
];
unsafe fn numeric_operand(value: Value) -> bool {
    unsafe {
        numeric::is_integer(value)
            || (!value.is_null() && !is_fixnum(value) && matches!(&(*value).data, Data::Float(..)))
    }
}
pub unsafe fn binary(op: i32, left: Value, right: Value, inplace: bool) -> Value {
    unsafe {
        if has_error() {
            return std::ptr::null_mut();
        }
        if !left.is_null() && !right.is_null() && !is_fixnum(left) && !is_fixnum(right) {
            match (&(*left).data, &(*right).data) {
                (Data::Text(a, ty_a, ..), Data::Text(b, ty_b, ..))
                    if op == 0 && ty_a.is_null() && ty_b.is_null() =>
                {
                    if a.is_empty() {
                        return retain(right);
                    }
                    if b.is_empty() {
                        return retain(left);
                    }
                    return match Text::join([a, b].into_iter()) {
                        Ok(text) => new_value(Data::Text(
                            text,
                            std::ptr::null_mut(),
                            std::ptr::null_mut(),
                        )),
                        Err(_) => {
                            error("MemoryError", "text storage allocation failed");
                            std::ptr::null_mut()
                        }
                    };
                }
                (Data::Bytes(a, ..), Data::Bytes(b, ..)) if op == 0 => {
                    let mut result = a.clone();
                    result.extend_from_slice(b);
                    return new_value(Data::Bytes(result, std::ptr::null_mut(), std::ptr::null_mut()));
                }
                (Data::List(a, ..), Data::List(b, ..)) if op == 0 => {
                    // Snapshot borrowed values before mutating either aliased list.
                    let left_values = a.clone();
                    let right_values = b.clone();
                    let result = if inplace { retain(left) } else { list() };
                    if !inplace {
                        for value in left_values {
                            list_append(result, value);
                        }
                    }
                    for value in right_values {
                        list_append(result, value);
                    }
                    return result;
                }
                _ => (),
            }
        }
        if op == 2 {
            let repeated = if !left.is_null()
                && !is_fixnum(left)
                && matches!(&(*left).data, Data::Bytes(..))
                && numeric::is_integer(right)
            {
                Some((left, right))
            } else if !right.is_null()
                && !is_fixnum(right)
                && matches!(&(*right).data, Data::Bytes(..))
                && numeric::is_integer(left)
            {
                Some((right, left))
            } else {
                None
            };
            if let Some((bytes, count)) = repeated {
                return repeat_bytes(bytes, count);
            }
        }
        if numeric_operand(left) && numeric_operand(right) {
            return numeric::binary(op, left, right);
        }
        dispatch_binary(op, left, right, inplace)
    }
}
unsafe fn repeat_bytes(bytes: Value, count: Value) -> Value {
    unsafe {
        let Some(count) = numeric::integer_export(count) else {
            return std::ptr::null_mut();
        };
        let Data::Bytes(data, ..) = &(*bytes).data else {
            unreachable!()
        };
        let count = if count.sign() <= 0 || data.is_empty() {
            0
        } else {
            let Some(count) = count
                .to_u64()
                .and_then(|value| usize::try_from(value).ok())
                .filter(|count| data.len().checked_mul(*count).is_some())
            else {
                error("OverflowError", "repeated bytes are too large");
                return std::ptr::null_mut();
            };
            count
        };
        let mut result = Vec::new();
        if result.try_reserve_exact(data.len() * count).is_err() {
            error("MemoryError", "repeated bytes allocation failed");
            return std::ptr::null_mut();
        }
        for _ in 0..count {
            result.extend_from_slice(data);
        }
        new_value(Data::Bytes(result, std::ptr::null_mut(), std::ptr::null_mut()))
    }
}
/// Try the in-place, forward, then reflected special method, as Python's
/// binary-operator protocol does for operands without a native operation.
unsafe fn dispatch_binary(op: i32, left: Value, right: Value, inplace: bool) -> Value {
    unsafe {
        let Some(&(method, reflected, inplace_method, symbol)) =
            usize::try_from(op).ok().and_then(|op| BINARY_METHODS.get(op))
        else {
            error("SystemError", "unknown binary operation");
            return std::ptr::null_mut();
        };
        if inplace {
            let result = implemented(left, inplace_method, &[right]);
            if !result.is_null() || has_error() {
                return result;
            }
        }
        let result = implemented(left, method, &[right]);
        if !result.is_null() || has_error() {
            return result;
        }
        let result = implemented(right, reflected, &[left]);
        if !result.is_null() || has_error() {
            return result;
        }
        let (Some(left), Some(right)) = (type_name(left), type_name(right)) else {
            return std::ptr::null_mut();
        };
        error(
            "TypeError",
            &format!("unsupported operand type(s) for {symbol}: '{left}' and '{right}'"),
        );
        std::ptr::null_mut()
    }
}
unsafe fn contains(container: Value, item: Value) -> Option<bool> {
    unsafe {
        if !container.is_null() && !is_fixnum(container) {
            match &(*container).data {
                Data::Text(..) | Data::Instance { .. } | Data::NativeInstance { .. } => {
                    let result = special(container, "__contains__", &[item]);
                    if result.is_null() {
                        if !has_error() {
                            error("TypeError", "object does not support containment");
                        }
                        return None;
                    }
                    let found = truth(result);
                    release(result);
                    return found;
                }
                Data::Bytes(haystack, ..) => {
                    if numeric::is_integer(item) {
                        let integer = numeric::integer_export(item)?;
                        let Some(byte) =
                            integer.to_u64().and_then(|value| u8::try_from(value).ok())
                        else {
                            error("ValueError", "byte must be in range(0, 256)");
                            return None;
                        };
                        return Some(haystack.contains(&byte));
                    }
                    if !item.is_null() && !is_fixnum(item) {
                        if let Data::Bytes(needle, ..) = &(*item).data {
                            return Some(
                                needle.is_empty()
                                    || haystack
                                        .windows(needle.len())
                                        .any(|window| window == needle),
                            );
                        }
                    }
                    error("TypeError", "incompatible string containment operand");
                    return None;
                }
                Data::Args(values, ..) | Data::List(values, ..) => {
                    // Retain the snapshot across user equality/truth callbacks.
                    let values: Vec<_> = values.iter().map(|value| retain(*value)).collect();
                    let mut result = Some(false);
                    for value in &values {
                        if *value == item {
                            result = Some(true);
                            break;
                        }
                        let equal = compare(0, item, *value);
                        if equal.is_null() {
                            result = None;
                            break;
                        }
                        let matched = truth(equal);
                        release(equal);
                        if matched != Some(false) {
                            result = matched;
                            break;
                        }
                    }
                    for value in values {
                        release(value);
                    }
                    return result;
                }
                _ => (),
            }
        }
        error("TypeError", "object does not support containment");
        None
    }
}

#[cold]
#[inline(never)]
unsafe fn compare_sequence_storage_slow(
    op: i32,
    left: Value,
    right: Value,
    index: usize,
) -> Value {
    unsafe {
        if has_error() {
            return std::ptr::null_mut();
        }
        retain(left);
        retain(right);
        let result = compare_sequence_storage_slow_inner(op, left, right, index);
        release(left);
        release(right);
        result
    }
}

unsafe fn compare_sequence_storage_slow_inner(
    op: i32,
    left: Value,
    right: Value,
    mut index: usize,
) -> Value {
    unsafe {
        let elements = |v: Value| -> &[Value] {
            match &(*v).data {
                Data::Args(items, ..) | Data::List(items, ..) => items.as_slice(),
                _ => &[],
            }
        };
        loop {
            let a = elements(left);
            let b = elements(right);
            if index >= a.len() || index >= b.len() {
                let (la, lb) = (a.len(), b.len());
                return bool_value(match op {
                    0 => la == lb,
                    1 => la != lb,
                    2 => la < lb,
                    3 => la <= lb,
                    4 => la > lb,
                    _ => la >= lb,
                });
            }
            let x = a[index];
            let y = b[index];
            if x == y {
                index += 1;
                continue;
            }
            if is_fixnum(x) && is_fixnum(y) {
                let (fx, fy) = (fixnum(x), fixnum(y));
                return bool_value(match op {
                    0 => false,
                    1 => true,
                    2 => fx < fy,
                    3 => fx <= fy,
                    4 => fx > fy,
                    _ => fx >= fy,
                });
            }
            retain(x);
            retain(y);
            let eq = compare(0, x, y);
            if eq.is_null() {
                release(x);
                release(y);
                return std::ptr::null_mut();
            }
            let same = truth(eq);
            release(eq);
            let Some(same) = same else {
                release(x);
                release(y);
                return std::ptr::null_mut();
            };
            if same {
                release(x);
                release(y);
                index += 1;
                continue;
            }
            if op < 2 {
                release(x);
                release(y);
                return bool_value(op == 1);
            }
            let ord = compare(op, x, y);
            release(x);
            release(y);
            return ord;
        }
    }
}

#[inline(always)]
unsafe fn compare_sequence_storage(
    op: i32,
    left: Value,
    right: Value,
    init_a: &[Value],
    init_b: &[Value],
) -> Value {
    unsafe {
        if std::ptr::eq(left, right) {
            return bool_value(matches!(op, 0 | 3 | 5));
        }
        if op < 2 && init_a.len() != init_b.len() {
            return bool_value(op == 1);
        }
        let mut index = 0usize;
        let min_len = init_a.len().min(init_b.len());
        while index < min_len {
            let x = *init_a.get_unchecked(index);
            let y = *init_b.get_unchecked(index);
            if x == y {
                index += 1;
                continue;
            }
            if ((x as usize) & (y as usize) & 1) != 0 {
                let (sx, sy) = (x as isize, y as isize);
                return bool_value(match op {
                    0 => false,
                    1 => true,
                    2 | 3 => sx < sy,
                    _ => sx > sy,
                });
            }
            return compare_sequence_storage_slow(op, left, right, index);
        }
        let (la, lb) = (init_a.len(), init_b.len());
        bool_value(match op {
            0 => la == lb,
            1 => la != lb,
            2 => la < lb,
            3 => la <= lb,
            4 => la > lb,
            _ => la >= lb,
        })
    }
}

pub unsafe fn compare(op: i32, left: Value, right: Value) -> Value {
    unsafe {
        if !left.is_null() && !right.is_null() && !is_fixnum(left) && !is_fixnum(right) {
            match (&(*left).data, &(*right).data) {
                (Data::Args(a, ty_a, ..), Data::Args(b, ty_b, ..))
                | (Data::List(a, ty_a, ..), Data::List(b, ty_b, ..))
                    if ty_a.is_null() && ty_b.is_null() && (0..=5).contains(&op) =>
                {
                    return compare_sequence_storage(op, left, right, a.as_slice(), b.as_slice());
                }
                (Data::Text(a, ty_a, ..), Data::Text(b, ty_b, ..))
                    if ty_a.is_null() && ty_b.is_null() && (0..=5).contains(&op) =>
                {
                    let matched = match op {
                        0 => std::ptr::eq(left, right) || a == b,
                        1 => !std::ptr::eq(left, right) && a != b,
                        _ => {
                            if let Some(table) = LATIN1_CHARACTERS.get() {
                                let base = table.as_ptr() as usize;
                                let span = std::mem::size_of::<[ImmortalObject; 256]>();
                                let l = (left as usize).wrapping_sub(base);
                                let r = (right as usize).wrapping_sub(base);
                                if l < span && r < span {
                                    return bool_value(match op {
                                        2 => l < r,
                                        3 => l <= r,
                                        4 => l > r,
                                        _ => l >= r,
                                    });
                                }
                            }
                            match op {
                                2 => a < b,
                                3 => a <= b,
                                4 => a > b,
                                _ => a >= b,
                            }
                        }
                    };
                    return bool_value(matched);
                }
                (Data::Bytes(a, ty_a, ..), Data::Bytes(b, ty_b, ..))
                    if ty_a.is_null() && ty_b.is_null() && (0..=5).contains(&op) =>
                {
                    let matched = match op {
                        0 => std::ptr::eq(left, right) || a == b,
                        1 => !std::ptr::eq(left, right) && a != b,
                        2 => a < b,
                        3 => a <= b,
                        4 => a > b,
                        _ => a >= b,
                    };
                    return bool_value(matched);
                }
                _ => (),
            }
        }
        compare_slow(op, left, right)
    }
}

#[cold]
#[inline(never)]
unsafe fn compare_slow(op: i32, left: Value, right: Value) -> Value {
    unsafe {
        if has_error() {
            return std::ptr::null_mut();
        }
        if op == 6 || op == 7 {
            return bool_value((left == right) == (op == 6));
        }
        retain(left);
        retain(right);
        let result = compare_slow_inner(op, left, right);
        release(left);
        release(right);
        result
    }
}

unsafe fn compare_slow_inner(op: i32, left: Value, right: Value) -> Value {
    unsafe {
        if op == 8 || op == 9 {
            return contains(right, left)
                .map_or(std::ptr::null_mut(), |found| bool_value(found == (op == 8)));
        }
        let methods = ["__eq__", "__ne__", "__lt__", "__le__", "__gt__", "__ge__"];
        let Some(method) = usize::try_from(op).ok().and_then(|op| methods.get(op)) else {
            error("SystemError", "unknown comparison operation");
            return std::ptr::null_mut();
        };
        let result = implemented(left, method, &[right]);
        if !result.is_null() || has_error() {
            return result;
        }
        // The reflected comparison: == and != swap, < and > exchange.
        let reflected = ["__eq__", "__ne__", "__gt__", "__ge__", "__lt__", "__le__"][op as usize];
        let result = implemented(right, reflected, &[left]);
        if !result.is_null() || has_error() {
            return result;
        }
        let numeric_value = |value: Value| {
            numeric::is_integer(value)
                || (!value.is_null()
                    && !is_fixnum(value)
                    && matches!(&(*value).data, Data::Float(..)))
        };
        let ordering = if numeric_value(left) && numeric_value(right) {
            numeric::compare(left, right)
        } else if !left.is_null() && !right.is_null() && !is_fixnum(left) && !is_fixnum(right) {
            match (&(*left).data, &(*right).data) {
                (Data::Text(a, ..), Data::Text(b, ..)) => Some(a.cmp(b)),
                (Data::Bytes(a, ..), Data::Bytes(b, ..)) => Some(a.cmp(b)),
                _ => None,
            }
            .map(|ordering| match ordering {
                std::cmp::Ordering::Less => -1,
                std::cmp::Ordering::Equal => 0,
                std::cmp::Ordering::Greater => 1,
            })
        } else {
            None
        };
        if let Some(ordering) = ordering {
            return bool_value(if ordering == 2 {
                op == 1
            } else {
                match op {
                    0 => ordering == 0,
                    1 => ordering != 0,
                    2 => ordering < 0,
                    3 => ordering <= 0,
                    4 => ordering > 0,
                    _ => ordering >= 0,
                }
            });
        }
        if has_error() {
            return std::ptr::null_mut();
        }
        if op < 2 {
            return bool_value((left == right) == (op == 0));
        }
        let (Some(left), Some(right)) = (type_name(left), type_name(right)) else {
            return std::ptr::null_mut();
        };
        let symbol = ["==", "!=", "<", "<=", ">", ">="][op as usize];
        error(
            "TypeError",
            &format!("'{symbol}' not supported between instances of '{left}' and '{right}'"),
        );
        std::ptr::null_mut()
    }
}

fn quoted(codes: impl Iterator<Item = u32> + Clone, bytes: bool, ascii_only: bool) -> Option<Vec<u8>> {
    let has = |wanted: u8| codes.clone().any(|code| code == u32::from(wanted));
    let quote = if has(b'\'') && !has(b'"') {
        b'"'
    } else {
        b'\''
    };
    let mut result = Vec::new();
    if bytes {
        result.push(b'b');
    }
    result.push(quote);
    for code in codes {
        if code == u32::from(quote) || code == u32::from(b'\\') {
            result.push(b'\\');
            result.push(code as u8);
        } else if code == 10 {
            result.extend_from_slice(b"\\n");
        } else if code == 13 {
            result.extend_from_slice(b"\\r");
        } else if code == 9 {
            result.extend_from_slice(b"\\t");
        } else if code < 32
            || (127..=160).contains(&code)
            || ((bytes || ascii_only) && code >= 128)
            || (0xd800..=0xdfff).contains(&code)
            || matches!(code, 0x2028 | 0x2029 | 0xfeff)
            || code & 0xffff >= 0xfffe
        {
            result.extend_from_slice(ascii_escape(code).as_bytes());
        } else {
            let mut buffer = [0; 4];
            result.extend_from_slice(char::from_u32(code)?.encode_utf8(&mut buffer).as_bytes());
        }
    }
    result.push(quote);
    Some(result)
}
fn ascii_escape(code: u32) -> String {
    if code <= 0xff {
        format!("\\x{code:02x}")
    } else if code <= 0xffff {
        format!("\\u{code:04x}")
    } else {
        format!("\\U{code:08x}")
    }
}
fn float_repr(value: f64) -> String {
    if value.is_nan() {
        return "nan".into();
    }
    let value = format!("{value:?}");
    if let Some((mantissa, exponent)) = value.split_once('e') {
        if let Ok(exponent) = exponent.parse::<i32>() {
            return format!("{mantissa}e{exponent:+03}");
        }
    }
    value
}
pub unsafe fn formatted_value(
    value: Value,
    representation: bool,
    ascii_only: bool,
) -> Option<Vec<u8>> {
    unsafe {
        if is_fixnum(value) {
            return Some(fixnum(value).to_string().into_bytes());
        }
        if value.is_null() {
            error("SystemError", "NULL formatting operand");
            return None;
        }
        Some(match &(*value).data {
            Data::None => b"None".to_vec(),
            Data::Bool(value) => {
                if *value {
                    b"True".to_vec()
                } else {
                    b"False".to_vec()
                }
            }
            Data::BigInt(value, ..) => value.to_decimal().into_bytes(),
            Data::Float(value, ..) => float_repr(*value).into_bytes(),
            Data::Text(data, ..) if !representation => match data.to_utf8() {
                Ok(bytes) => bytes,
                Err(_) => {
                    error("UnicodeEncodeError", "cannot encode surrogate to UTF-8");
                    return None;
                }
            },
            Data::Text(data, ..) => {
                // The compiled str protocol owns repr; the quoting below serves
                // only runtimes linked without it, such as the runtime probe.
                let method = crate::sequences::binding(value, "__repr__");
                if method.is_null() {
                    quoted(data.codepoints(), false, ascii_only)?
                } else {
                    let result = call_with_receiver(method, value, &[]);
                    if result.is_null() {
                        return None;
                    }
                    let Data::Text(text, ..) = &(*result).data else {
                        release(result);
                        error("TypeError", "__repr__ returned non-string");
                        return None;
                    };
                    let mut bytes = Vec::new();
                    for code in text.codepoints() {
                        if ascii_only && code >= 0x80 {
                            bytes.extend_from_slice(ascii_escape(code).as_bytes());
                        } else {
                            crate::text::append_wtf8(&mut bytes, code);
                        }
                    }
                    release(result);
                    bytes
                }
            }
            Data::Bytes(data, ..) => quoted(data.iter().map(|value| u32::from(*value)), true, ascii_only)?,
            Data::Args(values, ..) | Data::List(values, ..) => {
                let list = matches!(&(*value).data, Data::List(..));
                let mut result = vec![if list { b'[' } else { b'(' }];
                for (index, value) in values.iter().enumerate() {
                    if index > 0 {
                        result.extend_from_slice(b", ");
                    }
                    result.extend(user_text(*value, true, ascii_only)?);
                }
                if !list && values.len() == 1 {
                    result.push(b',');
                }
                result.push(if list { b']' } else { b')' });
                result
            }
            Data::Namespace(bindings) => {
                let mut result = vec![b'{'];
                for (index, (name, value)) in bindings.iter().enumerate() {
                    if index > 0 {
                        result.extend_from_slice(b", ");
                    }
                    result.extend(quoted(name.chars().map(u32::from), false, ascii_only)?);
                    result.extend_from_slice(b": ");
                    result.extend(user_text(value, true, ascii_only)?);
                }
                result.push(b'}');
                result
            }
            Data::Function { name, .. } => format!("<native function {name}>").into_bytes(),
            Data::ForeignPointerVector { .. } => b"<foreign pointer vector>".to_vec(),
            Data::ForeignCString { .. } => b"<foreign C string pointer>".to_vec(),
            Data::Class(_) => {
                if !crate::classes::special_bound_on_class(value, "__repr__").is_null() {
                    return user_text(value, true, ascii_only);
                }
                format!("<class '{}'>", qualified_name(value)).into_bytes()
            }
            Data::BoundMethod { .. } => b"<bound native method>".to_vec(),
            Data::WeakHandle { .. } => b"<weak handle>".to_vec(),
            Data::BufferLease { .. } => b"<buffer lifetime guard>".to_vec(),
            Data::SeqIter { .. } => b"<sequence iterator>".to_vec(),
            Data::DescriptorLease(_) => b"<descriptor lifetime guard>".to_vec(),
            Data::Coroutine(_) => b"<coroutine>".to_vec(),
            Data::Undefined => b"<undefined>".to_vec(),
            Data::Traceback { .. } => b"<traceback object>".to_vec(),
            Data::Frame { code, line, .. } => format!(
                "<frame, file '{}', line {line}, code {}>",
                (**code).filename(),
                (**code).name()
            )
            .into_bytes(),
            Data::Instance { ty, .. } | Data::NativeInstance { ty, .. } => {
                let user = special_binding_present(value, representation);
                if user {
                    return user_text(value, representation, ascii_only);
                }
                format!("<{} object at {:#x}>", qualified_name(*ty), value as usize).into_bytes()
            }
            Data::ArgumentView { .. } | Data::ReceiverArgs { .. } | Data::KeywordArgs { .. } => {
                error("SystemError", "argument view cannot escape a native call");
                return None;
            }
        })
    }
}

/// Whether an instance's class defines `__repr__` (or, for `str()`, `__str__`
/// or `__repr__`).
unsafe fn special_binding_present(value: Value, representation: bool) -> bool {
    unsafe {
        (!representation && !crate::classes::special_binding(value, "__str__").is_null())
            || !crate::classes::special_binding(value, "__repr__").is_null()
    }
}

/// A class's `module.qualname`, without the module for builtins.
pub(crate) unsafe fn qualified_name(class: Value) -> String {
    unsafe {
        let text = |name: &std::ffi::CStr| {
            let value = crate::classes::mule_get_attr(class, name.as_ptr());
            if value.is_null() {
                clear_error();
                return None;
            }
            let result = match &(*value).data {
                Data::Text(text, ..) => text.to_rust_string(),
                _ => None,
            };
            release(value);
            result
        };
        let name = text(c"__qualname__").or_else(|| text(c"__name__")).unwrap_or_default();
        match text(c"__module__") {
            Some(module) if module != "builtins" => format!("{module}.{name}"),
            _ => name,
        }
    }
}

/// The text of `repr(value)` (or `str(value)`) through the value's own
/// special methods when it has them, else the runtime's formatting.
unsafe fn user_text(value: Value, representation: bool, ascii_only: bool) -> Option<Vec<u8>> {
    unsafe {
        let custom = !value.is_null()
            && !is_fixnum(value)
            && match &(*value).data {
                Data::Instance { .. } | Data::NativeInstance { .. } => {
                    special_binding_present(value, representation)
                }
                Data::Class(_) => {
                    !crate::classes::special_bound_on_class(value, "__repr__").is_null()
                }
                _ => false,
            };
        if !custom {
            return formatted_value(value, representation, ascii_only);
        }
        let mut result = if representation {
            std::ptr::null_mut()
        } else {
            special(value, "__str__", &[])
        };
        if result.is_null() && !has_error() {
            result = special(value, "__repr__", &[]);
        }
        if result.is_null() {
            return None;
        }
        let bytes = match &(*result).data {
            Data::Text(text, ..) => {
                let mut bytes = Vec::new();
                for code in text.codepoints() {
                    if ascii_only && code >= 0x80 {
                        bytes.extend_from_slice(ascii_escape(code).as_bytes());
                    } else {
                        crate::text::append_wtf8(&mut bytes, code);
                    }
                }
                Some(bytes)
            }
            _ => {
                error("TypeError", "__repr__ returned non-string");
                None
            }
        };
        release(result);
        bytes
    }
}

pub(crate) unsafe fn name_from_c<'a>(name: *const c_char) -> Option<&'a str> {
    unsafe {
        if name.is_null() {
            error("SystemError", "NULL name");
            return None;
        }
        match CStr::from_ptr(name).to_str() {
            Ok(name) => Some(name),
            Err(_) => {
                error("UnicodeDecodeError", "invalid name");
                None
            }
        }
    }
}
// FFI callers supply valid buffers and live owned/borrowed values. Null results
// signal the sticky runtime error; each successful returned value is owned.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_none() -> Value {
    unsafe { none_value() }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_bool(value: i32) -> Value {
    unsafe { bool_value(value != 0) }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_release(value: Value) {
    unsafe { release(value) };
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_literal_text(data: *const c_char, length: isize) -> Value {
    unsafe { literal_bytes(data, length, true) }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_literal_bytes(data: *const c_char, length: isize) -> Value {
    unsafe { literal_bytes(data, length, false) }
}
unsafe fn literal_bytes(data: *const c_char, length: isize, text: bool) -> Value {
    unsafe {
        if has_error() {
            return std::ptr::null_mut();
        }
        if length < 0 || (data.is_null() && length != 0) {
            error("OverflowError", "invalid string buffer");
            return std::ptr::null_mut();
        }
        if text {
            if length == 0 {
                return empty_text();
            }
            let slice = std::slice::from_raw_parts(data.cast::<u8>(), length as usize);
            if slice.len() == 1 && slice[0] < 0x80 {
                return latin1_character(slice[0]);
            }
            return match Text::from_wtf8(slice) {
                Ok(decoded) => {
                    if decoded.len() == 1 {
                        if let Some(code) = decoded.get(0) {
                            if code <= 0xff {
                                return latin1_character(code as u8);
                            }
                        }
                    }
                    new_value(Data::Text(decoded, std::ptr::null_mut(), std::ptr::null_mut()))
                }
                Err(message) => {
                    error("UnicodeDecodeError", message);
                    std::ptr::null_mut()
                }
            };
        }
        let bytes = if length == 0 {
            Vec::new()
        } else {
            std::slice::from_raw_parts(data.cast::<u8>(), length as usize).to_vec()
        };
        new_value(Data::Bytes(bytes, std::ptr::null_mut(), std::ptr::null_mut()))
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_retain(value: Value) -> Value {
    unsafe { retain(value) }
}
#[unsafe(no_mangle)]
pub extern "C" fn mule_enter_frame() -> i32 {
    let status = FRAME_STATE.with(|state| {
        let mut st = state.get();
        if st.has_native_error || st.has_pending_exception {
            return -1;
        }
        if st.depth >= 1000 {
            return -2;
        }
        st.depth += 1;
        state.set(st);
        0
    });
    if status == 0 {
        0
    } else {
        if status == -2 {
            error("RecursionError", "maximum native recursion depth exceeded");
        }
        -1
    }
}
/// The current native frame depth, which a coroutine switch saves and restores.
pub fn frame_depth() -> usize {
    FRAME_STATE.with(|state| state.get().depth as usize)
}
pub fn set_frame_depth(value: usize) {
    FRAME_STATE.with(|state| {
        let mut st = state.get();
        st.depth = value as u32;
        state.set(st);
    });
}
#[unsafe(no_mangle)]
pub extern "C" fn mule_leave_frame() {
    FRAME_STATE.with(|state| {
        let mut st = state.get();
        debug_assert!(st.depth > 0, "native frame exits must match entries");
        st.depth -= 1;
        state.set(st);
    });
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_function_matches(
    callable: Value,
    expected: *mut c_void,
    globals: Value,
) -> i32 {
    unsafe {
        if callable.is_null() || is_fixnum(callable) {
            return 0;
        }
        match &(*callable).data {
            Data::Function {
                entry,
                globals: captured,
                ..
            } => i32::from(*entry as *const () as *mut c_void == expected && *captured == globals),
            _ => 0,
        }
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_method_entry(
    callable: Value,
    globals_out: *mut Value,
) -> *mut c_void {
    unsafe {
        if callable.is_null() || is_fixnum(callable) {
            return std::ptr::null_mut();
        }
        match &(*callable).data {
            Data::Function {
                entry,
                globals: captured,
                ..
            } if !captured.is_null() => {
                *globals_out = *captured;
                *entry as *const () as *mut c_void
            }
            _ => std::ptr::null_mut(),
        }
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_namespace() -> Value {
    unsafe { namespace() }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_namespace_bind(namespace: Value, name: *const c_char, value: Value) -> i32 {
    unsafe {
        let Some(name) = name_from_c(name) else { return -1; };
        if bind(namespace, name, value) { 0 } else { -1 }
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_namespace_bound(namespace: Value, name: *const c_char) -> i32 {
    unsafe {
        let Some(name) = name_from_c(name) else {
            return -1;
        };
        let value = lookup(namespace, name);
        if has_error() {
            -1
        } else {
            i32::from(!value.is_null())
        }
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_assertion_failure(message: Value) -> i32 {
    unsafe {
        let helper = lookup(crate::sequences::mule_protocol_globals(), "_assertion_failure");
        if !helper.is_null() {
            let instance = if message.is_null() {
                call(helper, &[])
            } else {
                call(helper, &[message])
            };
            if !instance.is_null() {
                crate::sequences::mule_raise_pending(instance);
                release(instance);
            }
            return -1;
        }
        if message.is_null() {
            error("AssertionError", "");
        } else if let Some(message) = formatted_value(message, false, false) {
            error("AssertionError", &String::from_utf8_lossy(&message));
        }
        -1
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_list() -> Value {
    unsafe { list() }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_list_append(list: Value, value: Value) -> i32 {
    unsafe { if list_append(list, value) { 0 } else { -1 } }
}

/// Copy and retain an evaluated buffer. Language evaluation and container
/// algorithms are compiled Mule code; this only constructs owned storage.
unsafe fn sequence_buffer(count: isize, values: *const Value) -> Option<Vec<Value>> {
    unsafe {
        if count < 0 || (count != 0 && values.is_null()) {
            error("SystemError", "invalid sequence buffer");
            return None;
        }
        let mut result = Vec::new();
        if result.try_reserve_exact(count as usize).is_err() {
            error("MemoryError", "sequence allocation failed");
            flush_object_pool();
            return None;
        }
        for index in 0..count as usize {
            result.push(retain(*values.add(index)));
        }
        Some(result)
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_sequence_list(count: isize, values: *const Value) -> Value {
    unsafe {
        sequence_buffer(count, values)
            .map_or(std::ptr::null_mut(), |values| new_value(Data::List(values.into(), std::ptr::null_mut(), std::ptr::null_mut())))
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_sequence_tuple(count: isize, values: *const Value) -> Value {
    unsafe {
        if count == 2 && !values.is_null() {
            let v0 = retain(*values);
            let v1 = retain(*values.add(1));
            let tracked = Data::may_cycle(v0) || Data::may_cycle(v1);
            return alloc_tuple2(v0, v1, tracked);
        }
        sequence_buffer(count, values)
            .map_or(std::ptr::null_mut(), |values| new_value(Data::Args(values.into(), std::ptr::null_mut(), std::ptr::null_mut())))
    }
}

/// Transfer an evaluated compiler buffer into tuple storage without retaining.
///
/// For nonnegative `count`, each entry must contain one independently owned,
/// live value reference (including separately owned occurrences of aliases).
/// A valid buffer's references are consumed on success and recoverable reserve
/// failure. Invalid negative counts or NULL nonempty buffers transfer nothing.
/// The buffer memory itself remains borrowed. Object allocation still uses the
/// runtime's infallible `Box::new`; pending errors are never cleared here.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_sequence_tuple_owned(
    count: isize,
    values: *const Value,
) -> Value {
    unsafe {
        if count == 2 && !values.is_null() {
            let v0 = *values;
            let v1 = *values.add(1);
            if !v0.is_null() && !v1.is_null() {
                let tracked = Data::may_cycle(v0) || Data::may_cycle(v1);
                return alloc_tuple2(v0, v1, tracked);
            }
        }
        if count < 0 || (count != 0 && values.is_null()) {
            error("SystemError", "invalid sequence buffer");
            return std::ptr::null_mut();
        }
        let count = count as usize;
        let slice = if count == 0 {
            &[]
        } else {
            std::slice::from_raw_parts(values, count)
        };
        if slice.iter().any(|value| value.is_null()) {
            for &value in slice {
                release(value);
            }
            if !has_error() {
                error("SystemError", "NULL sequence element");
            }
            return std::ptr::null_mut();
        }
        let mut result = Vec::new();
        if result.try_reserve_exact(count).is_err() {
            for &value in slice {
                release(value);
            }
            error("MemoryError", "sequence allocation failed");
            flush_object_pool();
            return std::ptr::null_mut();
        }
        if count != 0 {
            result.extend_from_slice(slice);
        }
        new_value(Data::Args(result.into(), std::ptr::null_mut(), std::ptr::null_mut()))
    }
}
/// Return an owned callable from the receiver's class method table, or None
/// when absent. Instance fields are intentionally not consulted.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_lookup_special(value: Value, name: *const c_char) -> Value {
    unsafe { lookup_special(value, name, false) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_lookup_special_including_defaults(value: Value, name: *const c_char) -> Value {
    unsafe { lookup_special(value, name, true) }
}

unsafe fn lookup_special(value: Value, name: *const c_char, include_object_defaults: bool) -> Value {
    unsafe {
        let Some(name) = name_from_c(name) else {
            return std::ptr::null_mut();
        };
        if value.is_null() || (is_fixnum(value) && name == "__index__") {
            return none_value();
        }
        let bound = crate::classes::lookup_special_bound(value, name, include_object_defaults);
        if bound.is_null() && !has_error() {
            none_value()
        } else {
            bound
        }
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_render_text(value: Value, representation: i32) -> Value {
    unsafe {
        formatted_value(value, representation != 0, false)
            .map_or(std::ptr::null_mut(), |bytes| text_value(&bytes))
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_text_result(value: Value) -> Value {
    unsafe {
        if !value.is_null() && !is_fixnum(value) {
            if let Data::Text(..) = &(*value).data {
                return retain(value);
            }
        }
        error("TypeError", "formatting protocol must return str");
        std::ptr::null_mut()
    }
}
/// Own a new `str` holding formatted UTF-8 output.
pub unsafe fn text_value(bytes: &[u8]) -> Value {
    unsafe {
        if bytes.is_empty() {
            return empty_text();
        }
        if bytes.len() == 1 && bytes[0] < 0x80 {
            return latin1_character(bytes[0]);
        }
        match Text::from_wtf8(bytes) {
            Ok(text) => new_value(Data::Text(text, std::ptr::null_mut(), std::ptr::null_mut())),
            Err(message) => {
                error("UnicodeDecodeError", message);
                std::ptr::null_mut()
            }
        }
    }
}
#[cold]
#[inline(never)]
fn text_storage_error() -> Value {
    error("TypeError", "text storage is required");
    std::ptr::null_mut()
}

#[cold]
#[inline(never)]
fn text_index_error() -> Value {
    error("IndexError", "text storage index out of range");
    std::ptr::null_mut()
}

#[cold]
#[inline(never)]
fn string_index_error() -> Value {
    error("IndexError", "string index out of range");
    std::ptr::null_mut()
}

/// Physical Unicode storage access for compiled Mule text algorithms.
#[inline]
pub unsafe fn text_storage<'a>(value: Value) -> Option<&'a Text> {
    unsafe {
        if !value.is_null() && !is_fixnum(value) {
            if let Data::Text(text, ..) = &(*value).data {
                return Some(text);
            }
        }
        text_storage_error();
        None
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_text_length(value: Value) -> Value {
    unsafe {
        if value.is_null() || is_fixnum(value) {
            return text_storage_error();
        }
        let Data::Text(text, ..) = &(*value).data else {
            return text_storage_error();
        };
        small(text.len() as i64)
    }
}
unsafe fn text_position(value: Value, index: Value) -> Option<(&'static Text, usize)> {
    unsafe {
        let text = text_storage(value)?;
        let index = if is_fixnum(index) {
            usize::try_from(fixnum(index)).ok()
        } else {
            numeric::integer_export(index)?
                .to_u64()
                .and_then(|value| usize::try_from(value).ok())
        };
        let Some(index) = index.filter(|index| *index < text.len()) else {
            error("IndexError", "text storage index out of range");
            return None;
        };
        Some((text, index))
    }
}
pub unsafe fn text_character_at(text: &Text, index: usize) -> Value {
    unsafe {
        if let Some(code) = text.get(index) {
            if code <= 0xff {
                return latin1_character(code as u8);
            }
        }
        match text.slice(index, index + 1) {
            Ok(character) => new_value(Data::Text(character, std::ptr::null_mut(), std::ptr::null_mut())),
            Err(_) => {
                error("MemoryError", "text storage allocation failed");
                std::ptr::null_mut()
            }
        }
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_text_character(value: Value, index: Value) -> Value {
    unsafe {
        let Some((text, index)) = text_position(value, index) else {
            return std::ptr::null_mut();
        };
        text_character_at(text, index)
    }
}
/// The Python type name of a value, for error messages.
pub unsafe fn type_name(value: Value) -> Option<String> {
    unsafe {
        if value.is_null() {
            error("SystemError", "NULL type name operand");
            return None;
        }
        if is_fixnum(value) {
            return Some("int".into());
        }
        Some(
            match &(*value).data {
                Data::None => "NoneType",
                Data::Bool(_) => "bool",
                Data::BigInt(..) => "int",
                Data::Float(..) => "float",
                Data::Text(..) => "str",
                Data::Bytes(..) => "bytes",
                Data::List(..) => "list",
                Data::Args(..) => "tuple",
                Data::Namespace(_) => "module",
                Data::Function { .. } => "function",
                Data::BoundMethod { .. } => "method",
                Data::WeakHandle { .. } => "weak handle",
                Data::BufferLease { .. } => "buffer lifetime guard",
                Data::SeqIter { .. } => "iterator",
                Data::DescriptorLease(_) => "descriptor lifetime guard",
                Data::Coroutine(_) => "coroutine",
                Data::Undefined => "undefined",
                Data::Traceback { .. } => "traceback",
                Data::Frame { .. } => "frame",
                Data::Class(_) => "type",
                Data::Instance { ty, .. } | Data::NativeInstance { ty, .. } => {
                    match &(**ty).data {
                        Data::Class(layout) => layout.name.as_str(),
                        _ => unreachable!(),
                    }
                }
                Data::ForeignPointerVector { .. } | Data::ForeignCString { .. } => {
                    "foreign pointer"
                }
                Data::ArgumentView { .. } | Data::ReceiverArgs { .. } | Data::KeywordArgs { .. } => {
                    error("SystemError", "argument view cannot escape a native call");
                    return None;
                }
            }
            .into(),
        )
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_type_name(value: Value) -> Value {
    unsafe {
        type_name(value).map_or(std::ptr::null_mut(), |name| {
            new_value(Data::Text(Text::from_rust_str(&name), std::ptr::null_mut(), std::ptr::null_mut()))
        })
    }
}
#[inline]
pub unsafe fn latin1_slice_value(bytes: &[u8]) -> Value {
    unsafe {
        match bytes.len() {
            0 => empty_text(),
            1 => latin1_character(bytes[0]),
            _ => match Text::from_latin1_slice(bytes) {
                Ok(slice) => new_untracked_value(Data::Text(
                    slice,
                    std::ptr::null_mut(),
                    std::ptr::null_mut(),
                )),
                Err(_) => {
                    error("MemoryError", "text storage allocation failed");
                    std::ptr::null_mut()
                }
            },
        }
    }
}
#[inline]
pub unsafe fn text_slice_range(text: &Text, start: usize, stop: usize) -> Value {
    unsafe {
        if start >= stop {
            return empty_text();
        }
        if let Some(bytes) = text.as_latin1() {
            let end = stop.min(bytes.len());
            let begin = start.min(end);
            return latin1_slice_value(&bytes[begin..end]);
        }
        if stop - start == 1 {
            if let Some(code) = text.get(start) {
                if code <= 0xff {
                    return latin1_character(code as u8);
                }
            }
        }
        match text.slice(start, stop) {
            Ok(slice) => new_untracked_value(Data::Text(
                slice,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
            )),
            Err(_) => {
                error("MemoryError", "text storage allocation failed");
                std::ptr::null_mut()
            }
        }
    }
}

/// Copy `text[start:stop]`; the compiled caller has clamped both bounds.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_text_slice(value: Value, start: Value, stop: Value) -> Value {
    unsafe {
        let Some(text) = text_storage(value) else {
            return std::ptr::null_mut();
        };
        let bound = |bound: Value| {
            if is_fixnum(bound) {
                usize::try_from(fixnum(bound)).ok()
            } else {
                None
            }
        };
        let (Some(start), Some(stop)) = (bound(start), bound(stop)) else {
            error("IndexError", "text slice bounds must be nonnegative small integers");
            return std::ptr::null_mut();
        };
        if start == 0 && stop >= text.len() {
            if let Data::Text(_, ty, ..) = &(*value).data {
                if ty.is_null() {
                    return retain(value);
                }
            }
        }
        text_slice_range(text, start, stop)
    }
}
/// Concatenate a list or tuple of `str` pieces into one new string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_text_join(pieces: Value) -> Value {
    unsafe {
        let values = if !pieces.is_null() && !is_fixnum(pieces) {
            match &(*pieces).data {
                Data::List(values, ..) | Data::Args(values, ..) => Some(values),
                _ => None,
            }
        } else {
            None
        };
        let Some(values) = values else {
            error("TypeError", "text join requires list or tuple storage");
            return std::ptr::null_mut();
        };
        let mut texts = Vec::with_capacity(values.len());
        for value in values {
            if is_fixnum(*value) || value.is_null() {
                error("TypeError", "text join requires str pieces");
                return std::ptr::null_mut();
            }
            let Data::Text(text, ..) = &(**value).data else {
                error("TypeError", "text join requires str pieces");
                return std::ptr::null_mut();
            };
            texts.push(text);
        }
        match Text::join(texts.into_iter()) {
            Ok(text) => new_value(Data::Text(text, std::ptr::null_mut(), std::ptr::null_mut())),
            Err(_) => {
                error("MemoryError", "text storage allocation failed");
                std::ptr::null_mut()
            }
        }
    }
}

/// Build a `str` from a list or tuple of codepoint integers.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_text_from_codepoints(value: Value) -> Value {
    unsafe {
        let values = if !value.is_null() && !is_fixnum(value) {
            match &(*value).data {
                Data::List(values, ..) | Data::Args(values, ..) => Some(values.as_slice()),
                _ => None,
            }
        } else {
            None
        };
        let Some(values) = values else {
            error("TypeError", "text_from_codepoints requires list or tuple storage");
            return std::ptr::null_mut();
        };
        if values.is_empty() {
            return empty_text();
        }
        let mut widest = 0u32;
        let mut all_fixnum = true;
        for &item in values {
            if is_fixnum(item) {
                let code = fixnum(item);
                if !(0..=0x10ffff).contains(&code) {
                    error("ValueError", "text codepoint must be in range(0x110000)");
                    return std::ptr::null_mut();
                }
                widest |= code as u32;
            } else {
                all_fixnum = false;
                break;
            }
        }
        if !all_fixnum {
            let mut codes = Vec::new();
            if codes.try_reserve_exact(values.len()).is_err() {
                error("MemoryError", "text storage allocation failed");
                return std::ptr::null_mut();
            }
            for &item in values {
                let code = if is_fixnum(item) {
                    u64::try_from(fixnum(item)).ok()
                } else {
                    let Some(integer) = numeric::integer_export(item) else {
                        return std::ptr::null_mut();
                    };
                    integer.to_u64()
                };
                let Some(code) = code.filter(|code| *code <= 0x10ffff) else {
                    error("ValueError", "text codepoint must be in range(0x110000)");
                    return std::ptr::null_mut();
                };
                codes.push(code as u32);
            }
            return match Text::from_codepoints(codes.into_iter()) {
                Ok(text) => new_value(Data::Text(text, std::ptr::null_mut(), std::ptr::null_mut())),
                Err(_) => {
                    error("MemoryError", "text storage allocation failed");
                    std::ptr::null_mut()
                }
            };
        }
        if values.len() == 1 && widest <= 0xff {
            return latin1_character(widest as u8);
        }
        let text = if widest <= 0xff {
            let mut units = Vec::new();
            if units.try_reserve_exact(values.len()).is_err() {
                error("MemoryError", "text storage allocation failed");
                return std::ptr::null_mut();
            }
            units.extend(values.iter().map(|&v| fixnum(v) as u8));
            Text::from_latin1(units)
        } else if widest <= 0xffff {
            let mut units = Vec::new();
            if units.try_reserve_exact(values.len()).is_err() {
                error("MemoryError", "text storage allocation failed");
                return std::ptr::null_mut();
            }
            units.extend(values.iter().map(|&v| fixnum(v) as u16));
            Text::from_ucs2(units)
        } else {
            let mut units = Vec::new();
            if units.try_reserve_exact(values.len()).is_err() {
                error("MemoryError", "text storage allocation failed");
                return std::ptr::null_mut();
            }
            units.extend(values.iter().map(|&v| fixnum(v) as u32));
            Text::from_ucs4(units)
        };
        new_value(Data::Text(text, std::ptr::null_mut(), std::ptr::null_mut()))
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_text_codepoint(value: Value, index: Value) -> Value {
    unsafe {
        text_position(value, index).map_or(std::ptr::null_mut(), |(text, index)| {
            small(i64::from(text.get(index).expect("index was checked")))
        })
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_text_codepoint_at(value: Value, index: i64) -> Value {
    unsafe {
        if value.is_null() || is_fixnum(value) {
            return text_storage_error();
        }
        let Data::Text(text, ..) = &(*value).data else {
            return text_storage_error();
        };
        if index >= 0 {
            let idx = index as usize;
            if let Some(bytes) = text.as_latin1() {
                if idx < bytes.len() {
                    return small(i64::from(*bytes.get_unchecked(idx)));
                }
            } else if let Some(code) = text.get(idx) {
                return small(i64::from(code));
            }
        }
        text_index_error()
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_text_codepoint_getitem(value: Value, index: Value) -> Value {
    unsafe {
        if value.is_null() || is_fixnum(value) {
            return text_storage_error();
        }
        let Data::Text(text, ..) = &(*value).data else {
            return text_storage_error();
        };
        let len = text.len();
        let raw = if is_fixnum(index) {
            fixnum(index)
        } else if !index.is_null() {
            match &(*index).data {
                Data::Bool(flag) => i64::from(*flag),
                Data::BigInt(number, ..) => {
                    let Some(raw) = number.to_i64() else {
                        return string_index_error();
                    };
                    raw
                }
                _ => return text_storage_error(),
            }
        } else {
            return text_storage_error();
        };
        let idx = if (raw as usize) < len {
            raw as usize
        } else if raw < 0 {
            let adjusted = raw.wrapping_add(len as i64);
            if adjusted < 0 {
                return string_index_error();
            }
            adjusted as usize
        } else {
            return string_index_error();
        };
        if let Some(bytes) = text.as_latin1() {
            return small(i64::from(*bytes.get_unchecked(idx)));
        }
        if let Some(code) = text.get(idx) {
            return small(i64::from(code));
        }
        string_index_error()
    }
}
/// The value of global `name` in `globals`, falling back to the builtins,
/// owned; NULL with `NameError` when neither binds it.
pub unsafe fn load_global(globals: Value, name: &str) -> Value {
    unsafe {
        let value = lookup(globals, name);
        if !value.is_null() {
            return retain(value);
        }
        if has_error() {
            return std::ptr::null_mut();
        }
        let value = builtins::lookup_builtin(name);
        if !value.is_null() {
            return retain(value);
        }
        error("NameError", &format!("name '{name}' is not defined"));
        std::ptr::null_mut()
    }
}

/// Link a unit's static cells for `count` names into `namespace`, or, when
/// `namespace` is NULL, prepare cells that only ever read builtins. Each cell
/// starts with the namespace's own value, else a copy of the builtin, else
/// the undefined object. With `builtin_expected`, every name must already be
/// an installed builtin, as the compiler assumed when it omitted their checks.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_link_globals(
    namespace: Value,
    count: i64,
    names: *const *const c_char,
    cells: *const *mut GlobalCell,
    builtin_expected: i32,
) -> i32 {
    unsafe {
        if !namespace.is_null()
            && (is_fixnum(namespace) || !matches!(&(*namespace).data, Data::Namespace(_)))
        {
            error("TypeError", "expected a Mule namespace");
            return -1;
        }
        let builtins = builtins::namespace();
        for index in 0..count as usize {
            let Some(name) = name_from_c(*names.add(index)) else {
                return -1;
            };
            let cell = *cells.add(index);
            // The builtins module's own code links the builtins themselves;
            // take each namespace borrow in turn.
            let builtin = if std::ptr::eq(namespace, builtins) {
                std::ptr::null_mut()
            } else {
                let Data::Namespace(bindings) = &mut (*builtins).data else {
                    error("SystemError", "builtins namespace storage is required");
                    return -1;
                };
                bindings.cell(name, std::ptr::null_mut(), builtins)
            };
            if builtin_expected != 0 && (builtin.is_null() || !(*builtin).bound()) {
                error("SystemError", &format!("builtin '{name}' is not installed"));
                return -1;
            }
            if namespace.is_null() {
                *cell = GlobalCell::unbound(builtin, std::ptr::null_mut());
                continue;
            }
            let Data::Namespace(bindings) = &mut (*namespace).data else { unreachable!() };
            if let Err(message) = bindings.link(name, cell, builtin, namespace) {
                error("SystemError", &message);
                return -1;
            }
        }
        0
    }
}

/// The slow path of a global read, which generated code inlines: the
/// namespace's own value, else the builtin (refreshing the cell's copy, for
/// cells linked before that builtin was installed), else `NameError`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_load_global(cell: *mut GlobalCell, name: *const c_char) -> Value {
    unsafe {
        let cell = &mut *cell;
        if cell.bound() {
            return retain(cell.value);
        }
        if !cell.builtin.is_null() && (*cell.builtin).bound() {
            let value = (*cell.builtin).value;
            let previous = std::mem::replace(&mut cell.value, retain(value));
            release(previous);
            return retain(value);
        }
        let name = name_from_c(name).unwrap_or("<invalid name>");
        error("NameError", &format!("name '{name}' is not defined"));
        std::ptr::null_mut()
    }
}
/// Bind global `name` to `value` (borrowed).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_store_global(cell: *mut GlobalCell, name: *const c_char, value: Value) -> i32 {
    unsafe {
        let cell = &mut *cell;
        if cell.bound() {
            let previous = std::mem::replace(&mut cell.value, retain(value));
            release(previous);
            return 0;
        }
        let Some(name) = name_from_c(name) else {
            return -1;
        };
        if cell.namespace.is_null() {
            error("SystemError", &format!("global '{name}' has no namespace"));
            return -1;
        }
        // A new binding takes its place in the namespace's order; it lands
        // in this same cell.
        if bind(cell.namespace, name, value) { 0 } else { -1 }
    }
}
/// Unbind global `name` (`del`), ignoring a name that is not bound.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_delete_global(cell: *mut GlobalCell, _name: *const c_char) -> i32 {
    unsafe {
        let cell = &mut *cell;
        if cell.bound() {
            cell.own = 0;
            let inherited = cell.inherited();
            release(std::mem::replace(&mut cell.value, inherited));
        }
        0
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_load_local(value: Value, name: *const c_char) -> Value {
    unsafe {
        if !value.is_null() {
            return retain(value);
        }
        let Some(name) = name_from_c(name) else {
            return std::ptr::null_mut();
        };
        error(
            "UnboundLocalError",
            &format!(
                "cannot access local variable '{name}' where it is not associated with a value"
            ),
        );
        std::ptr::null_mut()
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_binary(op: i32, left: Value, right: Value, inplace: i32) -> Value {
    unsafe { binary(op, left, right, inplace != 0) }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_binary_reuse(
    op: i32,
    left: Value,
    right: Value,
    inplace: i32,
    slot: *mut Value,
) -> Value {
    unsafe {
        if has_error() {
            return std::ptr::null_mut();
        }
        if let Some(result) = numeric::try_binary_reuse(op, left, right, slot) {
            return result;
        }
        binary(op, left, right, inplace != 0)
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_unary(op: i32, value: Value) -> Value {
    unsafe {
        if op == 3 {
            truth(value).map_or(std::ptr::null_mut(), |truth| bool_value(!truth))
        } else {
            numeric::unary(op, value)
        }
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_compare(op: i32, left: Value, right: Value) -> Value {
    unsafe { compare(op, left, right) }
}
#[cold]
#[inline(never)]
unsafe fn coerce_compare_bool(result: Value) -> Value {
    unsafe {
        let converted = truth(result).map_or(std::ptr::null_mut(), |flag| bool_value(flag));
        release(result);
        converted
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_compare_bool(op: i32, left: Value, right: Value) -> Value {
    unsafe {
        if !left.is_null() && !right.is_null() && !is_fixnum(left) && !is_fixnum(right) {
            match (&(*left).data, &(*right).data) {
                (Data::Args(a, ty_a, ..), Data::Args(b, ty_b, ..))
                | (Data::List(a, ty_a, ..), Data::List(b, ty_b, ..))
                    if ty_a.is_null() && ty_b.is_null() && (0..=5).contains(&op) =>
                {
                    return compare_sequence_storage(op, left, right, a.as_slice(), b.as_slice());
                }
                _ => (),
            }
        }
        let result = compare(op, left, right);
        if result.is_null()
            || std::ptr::eq(result, bool_value(true))
            || std::ptr::eq(result, bool_value(false))
        {
            return result;
        }
        coerce_compare_bool(result)
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_truth(value: Value) -> i32 {
    unsafe { truth(value).map_or(-1, i32::from) }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_call(callable: Value, count: isize, args: *const Value) -> Value {
    unsafe {
        if count < 0 || (count != 0 && args.is_null()) {
            error("SystemError", "invalid argument buffer");
            return std::ptr::null_mut();
        }
        let args = if count == 0 {
            &[]
        } else {
            std::slice::from_raw_parts(args, count as usize)
        };
        call(callable, args)
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_call_owned(
    callable: Value,
    count: isize,
    args: *const Value,
) -> Value {
    unsafe {
        if count < 0 || (count != 0 && args.is_null()) {
            error("SystemError", "invalid argument buffer");
            return std::ptr::null_mut();
        }
        let args = if count == 0 {
            &[]
        } else {
            std::slice::from_raw_parts(args, count as usize)
        };
        call_owned(callable, args)
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_function(
    globals: Value,
    native: *mut c_void,
    name: *const c_char,
) -> Value {
    unsafe {
        let Some(name) = name_from_c(name) else {
            return std::ptr::null_mut();
        };
        if native.is_null() {
            error("SystemError", "NULL native entry");
            return std::ptr::null_mut();
        }
        function(
            globals,
            std::mem::transmute::<*mut c_void, Native>(native),
            name,
        )
    }
}
/// A function's `__closure__`: a tuple holding a cell for each captured
/// variable, or None. A value captured directly is never reassigned, so a
/// new cell holding it reads the same as the variable's own would.
unsafe fn closure_cells_of(closure: Value, cell_flags: *const bool) -> Value {
    unsafe {
        if closure.is_null() {
            return none_value();
        }
        let Data::Args(entries, ..) = &(*closure).data else {
            error("SystemError", "function closure is not a tuple");
            return std::ptr::null_mut();
        };
        let globals = crate::sequences::mule_protocol_globals();
        let (cell_new, cell_set) = (lookup(globals, "cell_new"), lookup(globals, "cell_set"));
        if cell_new.is_null() || cell_set.is_null() {
            error("SystemError", "closure cells are unavailable before the cell protocol");
            return std::ptr::null_mut();
        }
        let mut cells = Vec::with_capacity(entries.len());
        for (index, entry) in entries.iter().enumerate() {
            let cell = if *cell_flags.add(index) {
                retain(*entry)
            } else {
                let cell = call(cell_new, &[]);
                if !cell.is_null() {
                    let stored = call(cell_set, &[cell, *entry]);
                    if stored.is_null() {
                        release(cell);
                    } else {
                        release(stored);
                    }
                }
                if has_error() { std::ptr::null_mut() } else { cell }
            };
            if cell.is_null() {
                for cell in cells {
                    release(cell);
                }
                return std::ptr::null_mut();
            }
            cells.push(cell);
        }
        new_value(Data::Args(cells.into(), std::ptr::null_mut(), std::ptr::null_mut()))
    }
}

/// Give a new nested function its captured cells and values, returning the
/// function (NULL when creating it failed). `cells` describes each closure
/// entry, as `Data::Function::closure_cells`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_function_closure(
    value: Value,
    closure: Value,
    cells: *const bool,
) -> Value {
    unsafe {
        if !value.is_null() {
            let Data::Function {
                closure: slot,
                closure_cells,
                ..
            } = &mut (*value).data
            else {
                unreachable!()
            };
            *slot = retain(closure);
            *closure_cells = cells;
        }
        value
    }
}
/// Borrow immutable entry storage from a compiler-created closure tuple.
/// The executing function keeps the tuple alive for every slot load.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_closure_slots(closure: Value) -> *const Value {
    unsafe {
        let Data::Args(entries, ..) = &(*closure).data else {
            std::hint::unreachable_unchecked();
        };
        entries.as_ptr()
    }
}

/// Borrow the closure the argument view carries for the running function.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_closure(args: Value) -> Value {
    unsafe {
        if args.is_null() || is_fixnum(args) {
            return std::ptr::null_mut();
        }
        match &(*args).data {
            Data::ArgumentView { closure, .. }
            | Data::ReceiverArgs { closure, .. }
            | Data::KeywordArgs { closure, .. } => *closure,
            _ => std::ptr::null_mut(),
        }
    }
}
/// Borrow a callable's closure after the caller has guarded its entry.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_function_closure_of(callable: Value) -> Value {
    unsafe {
        match &(*callable).data {
            Data::Function { closure, .. } => *closure,
            _ => std::ptr::null_mut(),
        }
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_check_args(args: Value, count: i32) -> i32 {
    unsafe {
        let Some(actual) = argument_count(args) else {
            return -1;
        };
        if crate::arguments::keyword_count(args) != 0 {
            error("TypeError", "unexpected keyword argument");
            return -1;
        }
        if count < 0 || actual != count as usize {
            error(
                "TypeError",
                &format!("expected {count} positional arguments, got {actual}"),
            );
            -1
        } else {
            0
        }
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_arg(args: Value, index: i32) -> Value {
    unsafe {
        if index < 0 {
            error("IndexError", "native argument index out of range");
            return std::ptr::null_mut();
        }
        let idx = index as usize;
        if !args.is_null() && !is_fixnum(args) && idx < 64 {
            let bit = 1u64 << idx;
            match &(*args).data {
                Data::ArgumentView {
                    values,
                    count,
                    owned_mask,
                    ..
                } if idx < *count => {
                    let value = *values.add(idx);
                    let mask = owned_mask.get();
                    if (mask & bit) != 0 {
                        owned_mask.set(mask & !bit);
                        return value;
                    }
                    return retain(value);
                }
                Data::ReceiverArgs {
                    receiver,
                    values,
                    count,
                    owned_mask,
                    ..
                } if idx <= *count => {
                    let value = if idx == 0 {
                        *receiver
                    } else {
                        *values.add(idx - 1)
                    };
                    let mask = owned_mask.get();
                    if (mask & bit) != 0 {
                        owned_mask.set(mask & !bit);
                        return value;
                    }
                    return retain(value);
                }
                _ => (),
            }
        }
        if let Some(value) = argument(args, idx) {
            retain(value)
        } else {
            error("IndexError", "native argument index out of range");
            std::ptr::null_mut()
        }
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_run(native: *mut c_void) -> i32 {
    unsafe { builtins::run(native) }
}


#[cfg(test)]
mod lifetime_tests {
    use super::*;

    unsafe fn word<T: Copy>(value: Value, offset: usize) -> T {
        unsafe { *((value as *const u8).add(offset) as *const T) }
    }

    #[test]
    fn compiled_float_offsets_match_header_and_payload_storage() {
        assert_eq!(std::mem::offset_of!(Object, references), REFERENCES_OFFSET);
        assert_eq!(std::mem::offset_of!(Object, tracked), TRACKED_OFFSET);
        assert_eq!(std::mem::offset_of!(Object, gc_slot), GC_SLOT_OFFSET);
        assert_eq!(std::mem::offset_of!(Object, data), DATA_OFFSET);
        assert_eq!(std::mem::align_of::<Object>(), 8);
        assert!(std::mem::size_of::<Object>() >= FLOAT_ATTRIBUTES_OFFSET + std::mem::size_of::<Value>());
        unsafe {
            let value = new_value(Data::Float(-0.0, std::ptr::null_mut(), std::ptr::null_mut()));
            assert_eq!(word::<usize>(value, REFERENCES_OFFSET), 1);
            assert!(!word::<bool>(value, TRACKED_OFFSET));
            assert_eq!(word::<u32>(value, GC_SLOT_OFFSET), 0);
            assert_eq!(word::<u32>(value, DATA_OFFSET), FLOAT_TAG);
            assert_eq!(word::<f64>(value, FLOAT_VALUE_OFFSET).to_bits(), (-0.0_f64).to_bits());
            assert!(word::<Value>(value, FLOAT_CLASS_OFFSET).is_null());
            assert!(word::<Value>(value, FLOAT_ATTRIBUTES_OFFSET).is_null());
            let Data::Float(payload, class, attributes) = &(*value).data else {
                panic!("missing float storage");
            };
            let base = value as usize;
            assert_eq!(payload as *const f64 as usize - base, FLOAT_VALUE_OFFSET);
            assert_eq!(class as *const Value as usize - base, FLOAT_CLASS_OFFSET);
            assert_eq!(attributes as *const Value as usize - base, FLOAT_ATTRIBUTES_OFFSET);
            release(value);
        }
    }

    #[test]
    fn compiled_object_offsets_match_instance_and_class_storage() {
        unsafe {
            let class = new_value(Data::Class(Box::new(crate::classes::ClassLayout::for_test())));
            let fields = vec![none_value(); 2].into_boxed_slice();
            let field_data = fields.as_ptr();
            let instance = new_value(Data::Instance {
                ty: retain(class),
                fields,
                attributes: std::ptr::null_mut(),
            });
            assert_eq!(word::<u32>(instance, DATA_OFFSET), INSTANCE_TAG);
            assert_eq!(word::<Value>(instance, INSTANCE_CLASS_OFFSET), class);
            assert_eq!(word::<*const Value>(instance, INSTANCE_FIELDS_OFFSET), field_data);
            assert!(word::<Value>(instance, INSTANCE_ATTRIBUTES_OFFSET).is_null());
            assert_eq!(word::<u32>(class, DATA_OFFSET), CLASS_TAG);
            let Data::Class(layout) = &(*class).data else { unreachable!() };
            assert_eq!(
                word::<*const crate::classes::ClassLayout>(class, CLASS_LAYOUT_OFFSET),
                &**layout as *const _
            );
            release(instance);
            release(class);
        }
    }

    #[test]
    fn compiled_bigint_offsets_match_the_existing_object_storage() {
        unsafe {
            let value = new_value(Data::BigInt(
                BigInt::from_i64(i64::MIN), std::ptr::null_mut(), std::ptr::null_mut(),
            ));
            assert_eq!(word::<u32>(value, DATA_OFFSET), BIGINT_TAG);
            assert!(word::<Value>(value, BIGINT_CLASS_OFFSET).is_null());
            assert!(word::<Value>(value, BIGINT_ATTRIBUTES_OFFSET).is_null());
            let Data::BigInt(integer, class, attributes) = &(*value).data else {
                panic!("missing integer storage");
            };
            let base = value as usize;
            assert_eq!(integer as *const BigInt as usize - base, BIGINT_VALUE_OFFSET);
            assert_eq!(class as *const Value as usize - base, BIGINT_CLASS_OFFSET);
            assert_eq!(attributes as *const Value as usize - base, BIGINT_ATTRIBUTES_OFFSET);
            assert_eq!(integer.to_i64(), Some(i64::MIN));
            release(value);
        }
    }

    #[test]
    fn compiled_native_instance_offsets_preserve_the_owner_and_payload() {
        unsafe {
            let class = new_value(Data::Class(Box::new(crate::classes::ClassLayout::for_test())));
            let payload = vec![17_i64, -23].into_boxed_slice();
            let field_data = payload.as_ptr();
            let instance = new_value(Data::NativeInstance { ty: retain(class), payload });
            assert_eq!(word::<u32>(instance, DATA_OFFSET), NATIVE_INSTANCE_TAG);
            assert_eq!(word::<Value>(instance, NATIVE_INSTANCE_CLASS_OFFSET), class);
            assert_eq!(word::<*const i64>(instance, NATIVE_INSTANCE_PAYLOAD_OFFSET), field_data);
            assert_eq!(word::<usize>(instance, NATIVE_INSTANCE_LENGTH_OFFSET), 2);
            let Data::NativeInstance { ty, payload } = &(*instance).data else {
                panic!("missing native instance storage");
            };
            let base = instance as usize;
            assert_eq!(ty as *const Value as usize - base, NATIVE_INSTANCE_CLASS_OFFSET);
            assert_eq!(payload as *const Box<[i64]> as usize - base, NATIVE_INSTANCE_PAYLOAD_OFFSET);
            let borrowed = crate::native_storage::mule_native_payload(instance);
            borrowed.add(1).write(42);
            assert_eq!(crate::native_storage::mule_native_payload(instance), borrowed);
            assert_eq!(*borrowed.add(1), 42);
            assert_eq!(word::<usize>(instance, NATIVE_INSTANCE_LENGTH_OFFSET), 2);
            release(instance);
            release(class);
        }
    }

    #[test]
    fn concurrent_owned_reference_transfers_preserve_parent_lifetime() {
        unsafe {
            let value = new_value(Data::Text(Text::from_rust_str("rooted"), std::ptr::null_mut(), std::ptr::null_mut()));
            let address = value as usize;
            // Parent ownership protects the address throughout every child.
            // No language callbacks or mutable heap storage are shared.
            std::thread::scope(|scope| {
                for _ in 0..8 {
                    scope.spawn(move || {
                        for _ in 0..50_000 {
                            let owned = retain(address as Value);
                            release(owned);
                        }
                    });
                }
            });
            assert_eq!((*value).reference_count(), 1);
            let Data::Text(text, ..) = &(*value).data else { panic!("lost rooted storage"); };
            assert_eq!(text.to_rust_string().as_deref(), Some("rooted"));
            release(value);
        }
    }

    #[test]
    fn tagged_and_immortal_values_keep_their_ownership_rules() {
        unsafe {
            let immediate = small(42);
            assert_eq!(retain(immediate), immediate);
            release(immediate);
            let immortal = none_value();
            assert_eq!(retain(immortal), immortal);
            release(immortal);
            assert_eq!((*immortal).reference_count(), usize::MAX);
        }
    }

    #[test]
    #[should_panic(expected = "release of an unowned heap value")]
    fn zero_count_release_is_a_lifetime_violation() {
        let value = Object {
            references: std::sync::atomic::AtomicUsize::new(0),
            tracked: false,
            gc_slot: 0,
            data: Data::None,
        };
        value.remove_reference();
    }
}
