//! Addresses and immutable facts of compiled initialization entries in the image.
//! Finder and loader policy is implemented in compiled Python.

use crate::object::{self, Data, Value};
use crate::text::Text;
use std::collections::{HashMap, HashSet};
use std::ffi::{CString, c_char, c_int, c_void};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::ptr::null_mut;
use std::sync::{Mutex, OnceLock};

unsafe extern "C" {
    fn dlopen(filename: *const c_char, flags: c_int) -> *mut c_void;
}

const RTLD_NOW: c_int = 2;

type Entry = unsafe extern "C" fn() -> Value;
type Execute = unsafe extern "C" fn(Value) -> Value;

#[derive(Clone)]
struct Artifact {
    name: String,
    entry: Option<Entry>,
    create: Option<Entry>,
    execute: Option<Execute>,
    filename: Option<String>,
    paths: Vec<String>,
}

#[derive(Default)]
struct Image {
    entries: HashMap<String, Artifact>,
    search_path: Vec<String>,
    toolchain: Option<(String, String, String, i64)>,
    missing_cache: HashSet<(String, Vec<String>)>,
}

fn image() -> &'static Mutex<Image> {
    static IMAGE: OnceLock<Mutex<Image>> = OnceLock::new();
    IMAGE.get_or_init(|| Mutex::new(Image::default()))
}

fn registered_artifact(name: &str) -> Option<Artifact> {
    image().lock().expect("compiled image registry poisoned").entries.get(name).cloned()
}

fn registered_search_path() -> Vec<String> {
    image().lock().expect("compiled image registry poisoned").search_path.clone()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_set_image_search_path(paths: *const *const c_char, count: i64) {
    unsafe {
        let stored = (0..count as usize).filter_map(|index| object::name_from_c(*paths.add(index)).map(str::to_owned)).collect();
        let mut img = image().lock().expect("compiled image registry poisoned");
        img.search_path = stored;
        img.missing_cache.clear();
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_set_image_toolchain(
    python_host: *const c_char,
    clang: *const c_char,
    rustc: *const c_char,
    optimize: i64,
) {
    unsafe {
        let Some(py) = object::name_from_c(python_host).map(str::to_owned) else { return; };
        let Some(cl) = object::name_from_c(clang).map(str::to_owned) else { return; };
        let Some(rs) = object::name_from_c(rustc).map(str::to_owned) else { return; };
        image().lock().expect("compiled image registry poisoned").toolchain = Some((py, cl, rs, optimize));
    }
}

unsafe extern "C" fn image_search_path(_: Value, args: Value) -> Value {
    unsafe {
        if object::argument_count(args) != Some(0) || crate::arguments::keyword_count(args) != 0 {
            object::error("TypeError", "image_search_path takes no arguments");
            return null_mut();
        }
        let paths = registered_search_path();
        let values: Vec<Value> = paths.iter().map(|path| object::new_value(Data::Text(Text::from_rust_str(path), null_mut(), null_mut()))).collect();
        let result = object::mule_sequence_tuple(values.len() as isize, values.as_ptr());
        for value in values { object::release(value); }
        result
    }
}

unsafe extern "C" fn invalidate_compiled_caches(_: Value, args: Value) -> Value {
    unsafe {
        if object::argument_count(args) != Some(0) || crate::arguments::keyword_count(args) != 0 {
            object::error("TypeError", "invalidate_compiled_caches takes no arguments");
            return null_mut();
        }
        image().lock().expect("compiled image registry poisoned").missing_cache.clear();
        object::none_value()
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
            name: name.into(),
            entry: Some(std::mem::transmute::<*mut c_void, Entry>(entry)),
            create: Some(std::mem::transmute::<*mut c_void, Entry>(create)),
            execute: Some(std::mem::transmute::<*mut c_void, Execute>(execute)),
            filename: if filename.is_null() { None } else { object::name_from_c(filename).map(str::to_owned) },
            paths: (0..count as usize).filter_map(|index| object::name_from_c(*paths.add(index)).map(str::to_owned)).collect(),
        };
        let mut img = image().lock().expect("compiled image registry poisoned");
        if let Some(existing) = img.entries.get(name) {
            if existing.entry.is_some() {
                return;
            }
        }
        img.entries.insert(name.into(), artifact);
    }
}

enum Candidate {
    Missing,
    NamespacePackage(Vec<String>),
    SourceFile( Option<(String, Vec<String>)>),
}

fn valid_module_name(name: &str) -> bool {
    if name.is_empty() {
        return false;
    }
    for part in name.split('.') {
        if part.is_empty() {
            return false;
        }
        let mut chars = part.chars();
        if chars.next().is_some_and(|c| c.is_ascii_digit()) {
            return false;
        }
        if !part.chars().all(|c| c == '_' || c.is_alphanumeric()) {
            return false;
        }
    }
    true
}

unsafe fn resolve_candidate(name: &str, search_roots: &[String]) -> Candidate {
    if !valid_module_name(name) {
        return Candidate::Missing;
    }
    let mut custom_parent: Option<(String, Vec<String>)> = None;
    let (dirs, relative) = if let Some((parent, child)) = name.rsplit_once('.') {
        if let Some(loaded_paths) = unsafe { crate::builtins::loaded_module_paths(parent) } {
            let Some(paths) = loaded_paths else {
                return Candidate::Missing;
            };
            if paths.is_empty() {
                return Candidate::Missing;
            }
            custom_parent = Some((parent.to_owned(), paths.clone()));
            (paths, PathBuf::from(child))
        } else if let Some(parent_art) = registered_artifact(parent) {
            if parent_art.paths.is_empty() {
                return Candidate::Missing;
            }
            custom_parent = Some((parent.to_owned(), parent_art.paths.clone()));
            (parent_art.paths, PathBuf::from(child))
        } else {
            (search_roots.to_vec(), PathBuf::from(name.replace('.', "/")))
        }
    } else {
        (search_roots.to_vec(), PathBuf::from(name))
    };

    let mut namespace_paths = Vec::new();
    for dir in &dirs {
        let base = Path::new(dir).join(&relative);
        if base.join("__init__.py").is_file() || base.with_extension("py").is_file() {
            return Candidate::SourceFile(custom_parent);
        }
        if base.is_dir() {
            namespace_paths.push(base.to_string_lossy().into_owned());
        }
    }
    if !namespace_paths.is_empty() {
        Candidate::NamespacePackage(namespace_paths)
    } else {
        Candidate::Missing
    }
}

unsafe fn ensure_artifact(name: &str) -> Option<Artifact> {
    if let Some(art) = registered_artifact(name) {
        return Some(art);
    }
    let toolchain = image().lock().expect("compiled image registry poisoned").toolchain.clone()?;
    let mut roots = unsafe { crate::builtins::current_sys_path() }.unwrap_or_else(registered_search_path);
    for r in registered_search_path() {
        if !roots.contains(&r) {
            roots.push(r);
        }
    }
    if let Some((parent, _)) = name.rsplit_once('.') {
        if unsafe { crate::builtins::loaded_module_paths(parent) }.is_none()
            && registered_artifact(parent).is_none()
        {
            let _ = unsafe { ensure_artifact(parent) };
        }
    }
    let key = (name.to_owned(), roots.clone());
    if image().lock().expect("compiled image registry poisoned").missing_cache.contains(&key) {
        return None;
    }
    match unsafe { resolve_candidate(name, &roots) } {
        Candidate::Missing => {
            image().lock().expect("compiled image registry poisoned").missing_cache.insert(key);
            None
        }
        Candidate::NamespacePackage(paths) => {
            let art = Artifact {
                name: name.to_owned(),
                entry: None,
                create: None,
                execute: None,
                filename: None,
                paths,
            };
            let mut img = image().lock().expect("compiled image registry poisoned");
            img.entries.entry(name.to_owned()).or_insert_with(|| art.clone());
            img.entries.get(name).cloned()
        }
        Candidate::SourceFile(custom_parent) => {
            let (py_host, clang, rustc, optimize) = toolchain;
            let loaded_mods: Vec<String> = image()
                .lock()
                .expect("compiled image registry poisoned")
                .entries
                .iter()
                .filter(|(_, art)| art.entry.is_some())
                .map(|(k, _)| k.clone())
                .collect();
            let mut cmd = Command::new(&py_host);
            cmd.env("MULE_IMAGE_MODULES", loaded_mods.join(":"));
            cmd.args([
                "-m",
                "mule.compiler",
                "--dynamic-module",
                name,
                "--clang",
                &clang,
                "--rustc",
                &rustc,
                "-O",
                &optimize.to_string(),
            ]);
            for r in &roots {
                cmd.arg("--root").arg(r);
            }
            if let Some((parent_name, parent_dirs)) = custom_parent {
                for pdir in parent_dirs {
                    cmd.arg("--package-path").arg(format!("{parent_name}={pdir}"));
                }
            }
            let output = cmd.output().ok()?;
            if !output.status.success() {
                image().lock().expect("compiled image registry poisoned").missing_cache.insert(key);
                return None;
            }
            let stdout = String::from_utf8_lossy(&output.stdout);
            for line in stdout.lines() {
                let path = line.trim();
                if path.is_empty() {
                    continue;
                }
                if let Ok(c_path) = CString::new(path) {
                    unsafe {
                        let _ = dlopen(c_path.as_ptr(), RTLD_NOW);
                    }
                }
            }
            let found = registered_artifact(name);
            if found.is_none() {
                image().lock().expect("compiled image registry poisoned").missing_cache.insert(key);
            }
            found
        }
    }
}

unsafe fn create_namespace_module(name: &str, paths: &[String]) -> Value {
    unsafe {
        let globals = object::namespace();
        if globals.is_null() {
            return null_mut();
        }
        for (field, val) in [("__name__", name), ("__package__", name)] {
            let text = object::new_value(Data::Text(Text::from_rust_str(val), null_mut(), null_mut()));
            let ok = object::bind(globals, field, text);
            object::release(text);
            if !ok {
                object::release(globals);
                return null_mut();
            }
        }
        let none = object::none_value();
        for field in ["__spec__", "__loader__", "__file__"] {
            if !object::bind(globals, field, none) {
                object::release(globals);
                return null_mut();
            }
        }
        let mut path_vals = Vec::with_capacity(paths.len());
        for p in paths {
            path_vals.push(object::new_value(Data::Text(Text::from_rust_str(p), null_mut(), null_mut())));
        }
        let path_list = object::mule_sequence_list(path_vals.len() as isize, path_vals.as_ptr());
        for v in path_vals {
            object::release(v);
        }
        if path_list.is_null() || !object::bind(globals, "__path__", path_list) {
            object::release(path_list);
            object::release(globals);
            return null_mut();
        }
        object::release(path_list);
        globals
    }
}

impl Artifact {
    unsafe fn load(&self) -> Value {
        unsafe {
            if let Some(entry) = self.entry {
                return entry();
            }
            match crate::builtins::find_loaded_module(&self.name) {
                Ok(Some(existing)) => return existing,
                Err(()) => return null_mut(),
                Ok(None) => {}
            }
            let globals = create_namespace_module(&self.name, &self.paths);
            if globals.is_null() {
                return null_mut();
            }
            crate::builtins::record_loaded_module(&self.name, globals);
            object::release(globals);
            match crate::builtins::find_loaded_module(&self.name) {
                Ok(Some(loaded)) => loaded,
                _ => null_mut(),
            }
        }
    }

    unsafe fn create(&self) -> Value {
        unsafe {
            if let Some(create) = self.create {
                return create();
            }
            create_namespace_module(&self.name, &self.paths)
        }
    }

    unsafe fn execute(&self, module: Value) -> Value {
        unsafe {
            if let Some(execute) = self.execute {
                return execute(module);
            }
            object::none_value()
        }
    }
}

pub unsafe fn import_or_compile_module(name: &str) -> Option<Value> {
    unsafe {
        if let Some((parent, _)) = name.rsplit_once('.') {
            if crate::builtins::find_loaded_module(parent).ok()?.is_none() {
                let parent_mod = import_or_compile_module(parent)?;
                object::release(parent_mod);
                if object::has_error() {
                    return None;
                }
            }
        }
        let art = ensure_artifact(name)?;
        let module = art.load();
        if module.is_null() {
            return None;
        }
        if let Some((parent, child)) = name.rsplit_once('.') {
            if let Ok(Some(parent_mod)) = crate::builtins::find_loaded_module(parent) {
                let saved = object::save_error();
                let _ = object::bind(parent_mod, child, module);
                object::clear_error();
                object::restore_error(saved);
                object::release(parent_mod);
            }
        }
        Some(module)
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
        Some(ensure_artifact(&name))
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
        artifact.load()
    }
}

unsafe extern "C" fn create_compiled_module(_: Value, args: Value) -> Value {
    unsafe {
        let Some(artifact) = artifact(args, 1) else { return null_mut(); };
        let Some(artifact) = artifact else {
            object::error("ModuleNotFoundError", "no linked compiled module with this name");
            return null_mut();
        };
        artifact.create()
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
        artifact.execute(module)
    }
}

pub unsafe fn populate(globals: Value) -> bool {
    unsafe {
        for (name, entry) in [
            ("compiled_module_info", compiled_module_info as object::Native),
            ("image_search_path", image_search_path as object::Native),
            ("invalidate_compiled_caches", invalidate_compiled_caches as object::Native),
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


#[cfg(test)]
mod registry_tests {
    use super::*;

    unsafe extern "C" fn entry() -> Value { null_mut() }
    unsafe extern "C" fn execute(_: Value) -> Value { null_mut() }

    #[test]
    fn compiled_image_metadata_is_shared_and_snapshots_are_independent() {
        let paths = [c"/native/package".as_ptr()];
        let entry_address = entry as Entry as usize;
        let execute_address = execute as Execute as usize;
        unsafe {
            mule_register_compiled_module(c"registry_test.module".as_ptr(), entry_address as *mut c_void,
                entry_address as *mut c_void, execute_address as *mut c_void, c"/native/module.py".as_ptr(), paths.as_ptr(), 1);
            mule_set_image_search_path(paths.as_ptr(), 1);
        }
        // Only physical code pointers and owned host strings cross threads.
        // This invokes neither a compiled module nor any language callback.
        std::thread::spawn(move || {
            let mut artifact = registered_artifact("registry_test.module").expect("image entry missing on worker");
            assert_eq!(artifact.entry.unwrap() as usize, entry_address);
            assert_eq!(artifact.create.unwrap() as usize, entry_address);
            assert_eq!(artifact.execute.unwrap() as usize, execute_address);
            assert_eq!(artifact.filename.as_deref(), Some("/native/module.py"));
            assert_eq!(artifact.paths, ["/native/package"]);
            assert_eq!(registered_search_path(), ["/native/package"]);
            artifact.paths.clear();
            artifact.filename.as_mut().unwrap().clear();
            let mut paths = registered_search_path();
            paths.clear();
        }).join().unwrap();
        let artifact = registered_artifact("registry_test.module").unwrap();
        assert_eq!(artifact.paths, ["/native/package"]);
        assert_eq!(artifact.filename.as_deref(), Some("/native/module.py"));
        assert_eq!(registered_search_path(), ["/native/package"]);
        assert!(registered_artifact("registry_test.absent").is_none());
    }
}
