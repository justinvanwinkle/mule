//! Nonmoving manual cycle reclamation over explicit owned heap references.

use crate::object::{self, Data, Value};
use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::ptr::null_mut;
use std::sync::{Mutex, OnceLock};

#[derive(Default)]
struct HeapMembership {
    tracked: Vec<usize>,
    dead: HashSet<usize>,
}

fn membership() -> &'static Mutex<HeapMembership> {
    static MEMBERSHIP: OnceLock<Mutex<HeapMembership>> = OnceLock::new();
    MEMBERSHIP.get_or_init(|| Mutex::new(HeapMembership::default()))
}

thread_local! {
    static COLLECTING: Cell<bool> = const { Cell::new(false) };
}

pub fn track(value: Value) {
    let mut guard = membership().lock().expect("heap registry poisoned");
    unsafe {
        (*value).tracked = true;
        let current = (*value).gc_slot;
        if current != 0 {
            let index = (current - 1) as usize;
            if index < guard.tracked.len() && guard.tracked[index] == value as usize {
                return;
            }
        }
        let index = guard.tracked.len();
        let slot = u32::try_from(index + 1).expect("tracked heap capacity exceeded");
        (*value).gc_slot = slot;
        guard.tracked.push(value as usize);
    }
}
pub fn untrack(value: Value) {
    let mut guard = membership().lock().expect("heap registry poisoned");
    unsafe {
        let current = (*value).gc_slot;
        (*value).gc_slot = 0;
        if current == 0 {
            return;
        }
        let index = (current - 1) as usize;
        if index >= guard.tracked.len() || guard.tracked[index] != value as usize {
            return;
        }
        let last = guard.tracked.pop().unwrap();
        if index < guard.tracked.len() {
            guard.tracked[index] = last;
            (*(last as Value)).gc_slot = current;
        }
    }
}
pub fn is_tracked(value: Value) -> bool {
    membership().lock().expect("heap registry poisoned").tracked.contains(&(value as usize))
}
pub fn unreachable(value: Value) -> bool {
    membership().lock().expect("heap registry poisoned").dead.contains(&(value as usize))
}

fn snapshot() -> Vec<Value> {
    membership().lock().expect("heap registry poisoned").tracked.iter().map(|value| *value as Value).collect()
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
        membership().lock().expect("heap registry poisoned").dead.extend(dead.iter().map(|value| *value as usize));
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
        membership().lock().expect("heap registry poisoned").dead.clear();
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
