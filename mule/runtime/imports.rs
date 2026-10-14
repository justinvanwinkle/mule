//! Addresses and immutable facts of compiled initialization entries in the image.
//! Finder and loader policy is implemented in compiled Python.

use crate::object::{self, Data, Value};
use crate::text::Text;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::{c_char, c_void};
use std::ptr::null_mut;

type Entry = unsafe extern "C" fn() -> Value;
type Execute = unsafe extern "C" fn(Value) -> Value;

#[derive(Clone)]
struct Artifact {
    entry: Entry,
    create: Entry,
    execute: Execute,
    filename: Option<String>,
    paths: Vec<String>,
}

thread_local! {
    static ENTRIES: RefCell<HashMap<String, Artifact>> = RefCell::new(HashMap::new());
    static SEARCH_PATH: RefCell<Vec<String>> = RefCell::new(Vec::new());
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_set_image_search_path(paths: *const *const c_char, count: i64) {
    unsafe {
        SEARCH_PATH.with_borrow_mut(|stored| {
            *stored = (0..count as usize).filter_map(|index| object::name_from_c(*paths.add(index)).map(str::to_owned)).collect();
        });
    }
}

unsafe extern "C" fn image_search_path(_: Value, args: Value) -> Value {
    unsafe {
        if object::argument_count(args) != Some(0) || crate::arguments::keyword_count(args) != 0 {
            object::error("TypeError", "image_search_path takes no arguments");
            return null_mut();
        }
        SEARCH_PATH.with_borrow(|paths| {
            let values: Vec<Value> = paths.iter().map(|path| object::new_value(Data::Text(Text::from_rust_str(path), null_mut(), null_mut()))).collect();
            let result = object::mule_sequence_tuple(values.len() as isize, values.as_ptr());
            for value in values { object::release(value); }
            result
        })
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_register_compiled_module(
    name: *const c_char, entry: *mut c_void, create: *mut c_void,
    execute: *mut c_void, filename: *const c_char, paths: *const *const c_char, count: i64,
) {
    unsafe {
        let Some(name) = object::name_from_c(name) else { return; };
        if entry.is_null() { return; }
        let artifact = Artifact {
            entry: std::mem::transmute::<*mut c_void, Entry>(entry),
            create: std::mem::transmute::<*mut c_void, Entry>(create),
            execute: std::mem::transmute::<*mut c_void, Execute>(execute),
            filename: if filename.is_null() { None } else { object::name_from_c(filename).map(str::to_owned) },
            paths: (0..count as usize).filter_map(|index| object::name_from_c(*paths.add(index)).map(str::to_owned)).collect(),
        };
        ENTRIES.with_borrow_mut(|entries| { entries.insert(name.into(), artifact); });
    }
}

unsafe fn artifact(args: Value, count: usize) -> Option<Option<Artifact>> {
    unsafe {
        if object::has_error() { return None; }
        if object::argument_count(args)? != count || crate::arguments::keyword_count(args) != 0 {
            object::error("TypeError", "invalid compiled module operation arguments");
            return None;
        }
        let name = object::argument(args, 0)?;
        let name = if name.is_null() || object::is_fixnum(name) {
            None
        } else if let Data::Text(text, ..) = &(*name).data {
            text.to_rust_string()
        } else { None };
        let Some(name) = name else {
            object::error("TypeError", "compiled module name must be text");
            return None;
        };
        Some(ENTRIES.with_borrow(|entries| entries.get(&name).cloned()))
    }
}

unsafe extern "C" fn compiled_module_info(_: Value, args: Value) -> Value {
    unsafe {
        let Some(artifact) = artifact(args, 1) else { return null_mut(); };
        let Some(artifact) = artifact else { return object::none_value(); };
        let filename = artifact.filename.map_or_else(|| object::none_value(), |name| {
            object::new_value(Data::Text(Text::from_rust_str(&name), null_mut(), null_mut()))
        });
        let paths: Vec<Value> = artifact.paths.iter().map(|path| object::new_value(Data::Text(Text::from_rust_str(path), null_mut(), null_mut()))).collect();
        let locations = object::mule_sequence_tuple(paths.len() as isize, paths.as_ptr());
        for path in paths { object::release(path); }
        let values = [filename, locations];
        let result = object::mule_sequence_tuple(2, values.as_ptr());
        object::release(filename);
        object::release(locations);
        result
    }
}

unsafe extern "C" fn load_compiled_module(_: Value, args: Value) -> Value {
    unsafe {
        let Some(artifact) = artifact(args, 1) else { return null_mut(); };
        let Some(artifact) = artifact else {
            object::error("ModuleNotFoundError", "no linked compiled module with this name");
            return null_mut();
        };
        (artifact.entry)()
    }
}

unsafe extern "C" fn create_compiled_module(_: Value, args: Value) -> Value {
    unsafe {
        let Some(artifact) = artifact(args, 1) else { return null_mut(); };
        let Some(artifact) = artifact else {
            object::error("ModuleNotFoundError", "no linked compiled module with this name");
            return null_mut();
        };
        (artifact.create)()
    }
}

unsafe extern "C" fn exec_compiled_module(_: Value, args: Value) -> Value {
    unsafe {
        let Some(artifact) = artifact(args, 2) else { return null_mut(); };
        let Some(artifact) = artifact else {
            object::error("ModuleNotFoundError", "no linked compiled module with this name");
            return null_mut();
        };
        let Some(module) = object::argument(args, 1) else { return null_mut(); };
        (artifact.execute)(module)
    }
}

pub unsafe fn populate(globals: Value) -> bool {
    unsafe {
        for (name, entry) in [
            ("compiled_module_info", compiled_module_info as object::Native),
            ("image_search_path", image_search_path as object::Native),
            ("load_compiled_module", load_compiled_module as object::Native),
            ("create_compiled_module", create_compiled_module as object::Native),
            ("exec_compiled_module", exec_compiled_module as object::Native),
        ] {
            let function = object::function(null_mut(), entry, name);
            let bound = object::bind(globals, name, function);
            object::release(function);
            if !bound { return false; }
        }
        true
    }
}
