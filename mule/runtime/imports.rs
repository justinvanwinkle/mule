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
const RTLD_GLOBAL: c_int = 0x100;

type Entry = unsafe extern "C" fn() -> Value;
type Execute = unsafe extern "C" fn(Value) -> Value;

#[derive(Clone)]
struct Artifact {
    name: String,
    entry: Option<Entry>,
    create: Option<Entry>,
    execute: Option<Execute>,
    c_ext: Option<PathBuf>,
    filename: Option<String>,
    paths: Vec<String>,
}

#[derive(Default)]
struct Image {
    entries: HashMap<String, Artifact>,
    search_path: Vec<String>,
    toolchain: Option<(String, String, String, i64)>,
    missing_cache: HashSet<(String, Vec<String>)>,
    loading_artifacts: HashSet<String>,
    loaded_fasls: HashSet<PathBuf>,
}

struct LoadingGuard(String);

impl Drop for LoadingGuard {
    fn drop(&mut self) {
        image()
            .lock()
            .expect("compiled image registry poisoned")
            .loading_artifacts
            .remove(&self.0);
    }
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
    crate::capi::ensure_linked();
    unsafe {
        let mut stored: Vec<String> = (0..count as usize)
            .filter_map(|index| object::name_from_c(*paths.add(index)).map(str::to_owned))
            .collect();
        let active_dir = std::env::current_exe()
            .ok()
            .and_then(|exe| {
                let is_step_cache = exe
                    .parent()
                    .and_then(|p| p.parent())
                    .and_then(|p| p.file_name())
                    .is_some_and(|n| n == "steps");
                if is_step_cache {
                    None
                } else {
                    exe.parent().map(Path::to_path_buf)
                }
            })
            .or_else(|| {
                let argv0 = std::env::args_os().next()?;
                if argv0 == "-c" || argv0 == "-" {
                    return None;
                }
                let path = PathBuf::from(argv0);
                let resolved = if path.is_absolute() {
                    path
                } else {
                    std::env::current_dir().ok()?.join(path)
                };
                resolved.parent().map(Path::to_path_buf)
            });
        if let Some(dir) = active_dir {
            if dir.is_dir() {
                let dir_str = dir.to_string_lossy().into_owned();
                if stored.first() != Some(&dir_str) {
                    if stored.first().is_some_and(|first| !Path::new(first).is_dir()) {
                        stored[0] = dir_str;
                    } else if !stored.contains(&dir_str) {
                        stored.insert(0, dir_str);
                    }
                }
            }
        }
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
            c_ext: None,
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
    CExtension(PathBuf),
    SourceFile(PathBuf, Option<(String, Vec<String>)>),
}

fn fasl_cache_path(
    compiler_hash: &str,
    optimize: i64,
    module_name: &str,
    source_path: &Path,
) -> Option<PathBuf> {
    if compiler_hash.is_empty() {
        return None;
    }
    let source_bytes = std::fs::read(source_path).ok()?;
    let header = format!("{compiler_hash}\0{optimize}\0{module_name}\0");
    let mut h: u128 = 0x6c62272e07bb014262b821756295c58d;
    const PRIME: u128 = 0x0000000001000000000000000000013b;
    for &b in header.as_bytes().iter().chain(source_bytes.iter()) {
        h = (h ^ (b as u128)).wrapping_mul(PRIME);
    }
    let cache_base = std::env::var_os("XDG_CACHE_HOME")
        .filter(|val| !val.is_empty())
        .map(PathBuf::from)
        .or_else(|| std::env::var_os("HOME").map(|home| PathBuf::from(home).join(".cache")))?;
    Some(cache_base.join("mule").join("fasl").join(format!("{h:032x}.so")))
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

    #[cfg(target_arch = "aarch64")]
    const EXT_SUFFIXES: [&str; 2] = [
        "mule-314-aarch64-linux-gnu.so",
        "mule-314-x86_64-linux-gnu.so",
    ];
    #[cfg(not(target_arch = "aarch64"))]
    const EXT_SUFFIXES: [&str; 2] = [
        "mule-314-x86_64-linux-gnu.so",
        "mule-314-aarch64-linux-gnu.so",
    ];
    let mut namespace_paths = Vec::new();
    for dir in &dirs {
        let base = Path::new(dir).join(&relative);
        for suffix in EXT_SUFFIXES {
            let init_so = base.join(format!("__init__.{suffix}"));
            if init_so.is_file() {
                return Candidate::CExtension(init_so);
            }
            let mod_so = PathBuf::from(format!("{}.{suffix}", base.display()));
            if mod_so.is_file() {
                return Candidate::CExtension(mod_so);
            }
        }
        let init_py = base.join("__init__.py");
        if init_py.is_file() {
            return Candidate::SourceFile(init_py, custom_parent);
        }
        let mod_py = base.with_extension("py");
        if mod_py.is_file() {
            return Candidate::SourceFile(mod_py, custom_parent);
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

fn read_elf_dependencies(path: &Path) -> Vec<String> {
    let Ok(data) = std::fs::read(path) else {
        return Vec::new();
    };
    if data.len() < 64 || &data[0..4] != b"\x7fELF" || data[4] != 2 || data[5] != 1 {
        return Vec::new();
    }
    let e_shoff = u64::from_le_bytes(data[40..48].try_into().unwrap()) as usize;
    let e_shentsize = u16::from_le_bytes(data[58..60].try_into().unwrap()) as usize;
    let e_shnum = u16::from_le_bytes(data[60..62].try_into().unwrap()) as usize;
    let e_shstrndx = u16::from_le_bytes(data[62..64].try_into().unwrap()) as usize;
    if e_shoff == 0
        || e_shnum == 0
        || e_shentsize < 64
        || e_shstrndx >= e_shnum
        || e_shoff.saturating_add(e_shnum.saturating_mul(e_shentsize)) > data.len()
    {
        return Vec::new();
    }
    let shstr_hdr = e_shoff + e_shstrndx * e_shentsize;
    let shstr_offset =
        u64::from_le_bytes(data[shstr_hdr + 24..shstr_hdr + 32].try_into().unwrap()) as usize;
    let shstr_size =
        u64::from_le_bytes(data[shstr_hdr + 32..shstr_hdr + 40].try_into().unwrap()) as usize;
    if shstr_offset.saturating_add(shstr_size) > data.len() {
        return Vec::new();
    }
    for idx in 0..e_shnum {
        let sh_hdr = e_shoff + idx * e_shentsize;
        let sh_name = u32::from_le_bytes(data[sh_hdr..sh_hdr + 4].try_into().unwrap()) as usize;
        let name_start = shstr_offset.saturating_add(sh_name);
        if name_start >= shstr_offset + shstr_size {
            continue;
        }
        let Some(rel_end) = data[name_start..shstr_offset + shstr_size]
            .iter()
            .position(|&b| b == 0)
        else {
            continue;
        };
        if &data[name_start..name_start + rel_end] == b".mule.interface" {
            let sh_offset =
                u64::from_le_bytes(data[sh_hdr + 24..sh_hdr + 32].try_into().unwrap()) as usize;
            let sh_size =
                u64::from_le_bytes(data[sh_hdr + 32..sh_hdr + 40].try_into().unwrap()) as usize;
            if sh_offset.saturating_add(sh_size) > data.len() {
                return Vec::new();
            }
            let raw = &data[sh_offset..sh_offset + sh_size];
            let Ok(text) = std::str::from_utf8(raw) else {
                return Vec::new();
            };
            let Some(key_pos) = text.find("\"dependencies\":") else {
                return Vec::new();
            };
            let rest = &text[key_pos + "\"dependencies\":".len()..];
            let Some(open_bracket) = rest.find('[') else {
                return Vec::new();
            };
            let after_open = &rest[open_bracket + 1..];
            let Some(close_bracket) = after_open.find(']') else {
                return Vec::new();
            };
            let inner = &after_open[..close_bracket];
            let mut deps = Vec::new();
            let mut in_str = false;
            let mut cur = String::new();
            for ch in inner.chars() {
                if ch == '"' {
                    if in_str {
                        if !cur.is_empty() {
                            deps.push(std::mem::take(&mut cur));
                        }
                        in_str = false;
                    } else {
                        in_str = true;
                    }
                } else if in_str {
                    cur.push(ch);
                }
            }
            return deps;
        }
    }
    Vec::new()
}

unsafe fn dlopen_fasl(path: &Path) {
    {
        let mut img = image().lock().expect("compiled image registry poisoned");
        if !img.loaded_fasls.insert(path.to_path_buf()) {
            return;
        }
    }
    for dep in read_elf_dependencies(path) {
        let already = {
            let img = image().lock().expect("compiled image registry poisoned");
            img.entries.contains_key(&dep) || img.loading_artifacts.contains(&dep)
        };
        if !already {
            let _ = unsafe { ensure_artifact(&dep) };
        }
    }
    if let Ok(c_path) = CString::new(path.to_string_lossy().as_bytes()) {
        unsafe {
            let _ = dlopen(c_path.as_ptr(), RTLD_NOW | RTLD_GLOBAL);
        }
    }
}

unsafe fn ensure_artifact(name: &str) -> Option<Artifact> {
    if let Some(art) = registered_artifact(name) {
        return Some(art);
    }
    {
        let mut img = image().lock().expect("compiled image registry poisoned");
        if !img.loading_artifacts.insert(name.to_owned()) {
            return None;
        }
    }
    let _guard = LoadingGuard(name.to_owned());
    let mut roots = unsafe { crate::builtins::current_sys_path() }.unwrap_or_else(registered_search_path);
    for r in registered_search_path() {
        if !roots.contains(&r) {
            roots.push(r);
        }
    }
    if let Some((parent, _)) = name.rsplit_once('.') {
        let parent_loading = image()
            .lock()
            .expect("compiled image registry poisoned")
            .loading_artifacts
            .contains(parent);
        if !parent_loading
            && unsafe { crate::builtins::loaded_module_paths(parent) }.is_none()
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
                c_ext: None,
                filename: None,
                paths,
            };
            let mut img = image().lock().expect("compiled image registry poisoned");
            img.entries.entry(name.to_owned()).or_insert_with(|| art.clone());
            img.entries.get(name).cloned()
        }
        Candidate::CExtension(so_path) => {
            let filename = Some(so_path.to_string_lossy().into_owned());
            let paths = if so_path.file_name().is_some_and(|n| n.to_string_lossy().starts_with("__init__.")) {
                so_path.parent().map(|p| vec![p.to_string_lossy().into_owned()]).unwrap_or_default()
            } else {
                Vec::new()
            };
            let art = Artifact {
                name: name.to_owned(),
                entry: None,
                create: None,
                execute: None,
                c_ext: Some(so_path),
                filename,
                paths,
            };
            let mut img = image().lock().expect("compiled image registry poisoned");
            img.entries.entry(name.to_owned()).or_insert_with(|| art.clone());
            img.entries.get(name).cloned()
        }
        Candidate::SourceFile(src_path, custom_parent) => {
            let toolchain = image().lock().expect("compiled image registry poisoned").toolchain.clone()?;
            let (py_host, clang, rustc_and_hash, optimize) = toolchain;
            let (rustc, compiler_hash) = rustc_and_hash
                .split_once('\n')
                .unwrap_or((&rustc_and_hash, ""));
            if let Some(cached_fasl) = fasl_cache_path(compiler_hash, optimize, name, &src_path) {
                if cached_fasl.is_file() {
                    unsafe {
                        dlopen_fasl(&cached_fasl);
                    }
                    if let Some(found) = registered_artifact(name) {
                        return Some(found);
                    }
                }
            }
            let mut cmd = Command::new(&py_host);
            cmd.args([
                "-m",
                "mule.compiler",
                "--dynamic-module",
                name,
                "--clang",
                &clang,
                "--rustc",
                rustc,
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
                unsafe {
                    dlopen_fasl(Path::new(path));
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
            if let Some(so_path) = &self.c_ext {
                match crate::builtins::find_loaded_module(&self.name) {
                    Ok(Some(existing)) => return existing,
                    Err(()) => return null_mut(),
                    Ok(None) => {}
                }
                return match crate::capi::load_c_extension(&self.name, so_path) {
                    Ok(m) => m,
                    Err(msg) => {
                        object::error("ImportError", &msg);
                        null_mut()
                    }
                };
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
            if let Some(so_path) = &self.c_ext {
                return match crate::capi::load_c_extension(&self.name, so_path) {
                    Ok(m) => m,
                    Err(msg) => {
                        object::error("ImportError", &msg);
                        null_mut()
                    }
                };
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
                let bound = object::bind(parent_mod, child, module);
                object::release(parent_mod);
                if !bound {
                    object::release(module);
                    return None;
                }
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
