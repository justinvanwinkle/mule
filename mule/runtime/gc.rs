//! Nonmoving manual cycle reclamation over explicit owned heap references.

use crate::object::{self, Data, Value};
use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::ptr::null_mut;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Mutex, OnceLock};

pub const GC_SLOT_BIT: u32 = 0x8000_0000;
const SHARD_COUNT: usize = 64;
const INDEX_BITS: u32 = 24;
const INDEX_MASK: u32 = (1 << INDEX_BITS) - 1;

#[repr(align(64))]
struct Shard(Mutex<Vec<usize>>);

static SHARDS: [Shard; SHARD_COUNT] = [const { Shard(Mutex::new(Vec::new())) }; SHARD_COUNT];
static NEXT_SHARD: AtomicUsize = AtomicUsize::new(0);
static HAS_DEAD: AtomicBool = AtomicBool::new(false);

fn dead_set() -> &'static Mutex<HashSet<usize>> {
    static DEAD: OnceLock<Mutex<HashSet<usize>>> = OnceLock::new();
    DEAD.get_or_init(|| Mutex::new(HashSet::new()))
}

thread_local! {
    static COLLECTING: Cell<bool> = const { Cell::new(false) };
    static LOCAL_SHARD: Cell<usize> = const { Cell::new(usize::MAX) };
}

#[inline(always)]
fn current_shard() -> usize {
    let shard = LOCAL_SHARD.with(Cell::get);
    if shard < SHARD_COUNT {
        return shard;
    }
    init_current_shard()
}

#[cold]
#[inline(never)]
fn init_current_shard() -> usize {
    let shard = NEXT_SHARD.fetch_add(1, Ordering::Relaxed) % SHARD_COUNT;
    LOCAL_SHARD.with(|slot| slot.set(shard));
    shard
}

#[inline]
pub fn track(value: Value) {
    unsafe {
        (*value).tracked = true;
        let current = (*value).gc_slot;
        if (current & GC_SLOT_BIT) != 0 {
            return;
        }
        if current != 0 {
            object::unintern_if_text(value);
        }
        let shard_id = current_shard();
        let mut tracked = SHARDS[shard_id].0.lock().expect("heap registry poisoned");
        let index = tracked.len();
        assert!(
            index < INDEX_MASK as usize,
            "tracked heap capacity exceeded"
        );
        let slot = GC_SLOT_BIT | ((shard_id as u32) << INDEX_BITS) | ((index as u32) + 1);
        (*value).gc_slot = slot;
        tracked.push(value as usize);
    }
}

#[inline]
pub fn untrack(value: Value) {
    unsafe {
        let current = (*value).gc_slot;
        if (current & GC_SLOT_BIT) == 0 {
            return;
        }
        (*value).gc_slot = 0;
        let shard_id = ((current & !GC_SLOT_BIT) >> INDEX_BITS) as usize;
        let index = ((current & INDEX_MASK) - 1) as usize;
        let mut tracked = SHARDS[shard_id].0.lock().expect("heap registry poisoned");
        if index >= tracked.len() || tracked[index] != value as usize {
            return;
        }
        let last = tracked.pop().unwrap();
        if index < tracked.len() {
            tracked[index] = last;
            (*(last as Value)).gc_slot = current;
        }
    }
}

pub fn is_tracked(value: Value) -> bool {
    if value.is_null() || object::is_fixnum(value) {
        return false;
    }
    let target = value as usize;
    for shard in &SHARDS {
        let tracked = shard.0.lock().expect("heap registry poisoned");
        if tracked.contains(&target) {
            return unsafe { (*value).tracked && (*value).reference_count() > 0 };
        }
    }
    false
}

#[inline]
pub fn unreachable(value: Value) -> bool {
    if !HAS_DEAD.load(Ordering::Relaxed) {
        return false;
    }
    dead_set()
        .lock()
        .expect("heap registry poisoned")
        .contains(&(value as usize))
}

fn snapshot() -> Vec<Value> {
    let mut values = Vec::new();
    for shard in &SHARDS {
        let tracked = shard.0.lock().expect("heap registry poisoned");
        for &addr in tracked.iter() {
            let value = addr as Value;
            unsafe {
                if (*value).tracked && (*value).reference_count() > 0 {
                    values.push(value);
                }
            }
        }
    }
    values
}

unsafe fn operands<const N: usize>(args: Value) -> Option<[Value; N]> {
    unsafe {
        if object::has_error() { return None; }
        if object::argument_count(args)? != N || crate::arguments::keyword_count(args) != 0 {
            object::error("TypeError", "invalid collection primitive argument count"); return None;
        }
        let mut values = [null_mut(); N];
        for (index, value) in values.iter_mut().enumerate() { *value = object::argument(args, index)?; }
        Some(values)
    }
}

unsafe extern "C" fn collect(_: Value, args: Value) -> Value {
    unsafe {
        if operands::<0>(args).is_none() { return null_mut(); }
        if COLLECTING.with(|state| state.replace(true)) { return object::small(0); }
        let result = crate::threads::with_world_stopped(|| collect_heap());
        COLLECTING.with(|state| state.set(false));
        result
    }
}

unsafe fn collect_heap() -> Value {
    unsafe {
        let values = snapshot();
        let indices: HashMap<usize, usize> = values.iter().enumerate().map(|(index, value)| (*value as usize, index)).collect();
        let mut counts: Vec<usize> = values.iter().map(|value| (**value).reference_count()).collect();
        let edges: Vec<Vec<usize>> = values.iter().map(|value| (**value).data.owned_values().iter().filter_map(|child| indices.get(&(*child as usize)).copied()).collect()).collect();
        for (owner, children) in edges.iter().enumerate() {
            for child in children {
                // Each enumerated edge owns one count, including repeated aliases.
                assert!(counts[*child] > 0,
                    "owned heap edges exceed reference count: target={:p} type={:?} refs={} owner={:p} type={:?}",
                    values[*child], object::type_name(values[*child]), (*values[*child]).reference_count(),
                    values[owner], object::type_name(values[owner]));
                counts[*child] -= 1;
            }
        }
        let mut live = vec![false; values.len()];
        let mut pending: Vec<usize> = counts.iter().enumerate().filter_map(|(index, count)| (*count > 0).then_some(index)).collect();
        while let Some(index) = pending.pop() {
            if live[index] { continue; }
            live[index] = true;
            pending.extend(&edges[index]);
        }
        let dead: Vec<Value> = values.iter().enumerate().filter_map(|(index, value)| (!live[index]).then_some(*value)).collect();
        if !dead.is_empty() {
            dead_set().lock().expect("heap registry poisoned").extend(dead.iter().map(|value| *value as usize));
            HAS_DEAD.store(true, Ordering::Release);
        }
        for value in &dead { object::retain(*value); }
        // Clear weak targets before releasing any cycle edge. Callbacks owned
        // by unreachable reference objects must not run during their collection.
        for value in &dead { crate::weakref::clear(*value); }
        let mut storage = Vec::with_capacity(dead.len());
        for value in &dead {
            (**value).tracked = false;
            untrack(*value);
            storage.push((*value, std::mem::replace(&mut (**value).data, Data::None)));
        }
        for (value, data) in storage { object::release_data(value, data); }
        for value in &dead { object::release(*value); }
        if !dead.is_empty() {
            dead_set().lock().expect("heap registry poisoned").clear();
            HAS_DEAD.store(false, Ordering::Release);
        }
        object::small(dead.len() as i64)
    }
}

unsafe extern "C" fn objects(_: Value, args: Value) -> Value {
    unsafe {
        if operands::<0>(args).is_none() { return null_mut(); }
        crate::threads::with_world_stopped(|| {
            let values = snapshot();
            object::mule_sequence_list(values.len() as isize, values.as_ptr())
        })
    }
}

unsafe extern "C" fn referents(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands(args) else { return null_mut(); };
        let values = if value.is_null() || object::is_fixnum(value) { Vec::new() } else { (*value).data.owned_values() };
        let values: Vec<Value> = values.into_iter().filter(|value| !value.is_null()).collect();
        object::mule_sequence_list(values.len() as isize, values.as_ptr())
    }
}

unsafe extern "C" fn tracked(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands(args) else { return null_mut(); };
        object::bool_value(!value.is_null() && !object::is_fixnum(value) && (*value).tracked)
    }
}

pub unsafe fn populate(globals: Value) -> bool {
    unsafe {
        let exports: [(&str, object::Native); 4] = [("gc_collect", collect), ("gc_objects", objects), ("gc_referents", referents), ("gc_tracked", tracked)];
        for (name, entry) in exports {
            let function = object::function(null_mut(), entry, name);
            let bound = object::bind(globals, name, function);
            object::release(function);
            if !bound { return false; }
        }
        true
    }
}
