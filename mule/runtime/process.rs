//! Trusted process-lifetime foreign storage; compiled Python owns decoding policy.

use crate::numeric;
use crate::text::Text;
use crate::object::{self, Data, Native, Value};
use std::cell::Cell;
use std::ffi::{CStr, c_char};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::DirBuilderExt;
use std::os::unix::io::FromRawFd;
use std::ptr::null_mut;

#[derive(Clone, Copy)]
struct Arguments {
    count: usize,
    values: *const *const c_char,
}

// Compiled Python currently runs on the native main thread only.
/// Unit finalizers in registration order. A module unit registers its
/// finalizer when its import first runs; `mule_run_finalizers` calls them in
/// reverse before the main unit releases its own state.
static FINALIZERS: std::sync::Mutex<Vec<unsafe extern "C" fn()>> =
    std::sync::Mutex::new(Vec::new());

#[unsafe(no_mangle)]
pub extern "C" fn mule_register_finalizer(finalizer: unsafe extern "C" fn()) {
    FINALIZERS.lock().unwrap().push(finalizer);
}

#[unsafe(no_mangle)]
pub extern "C" fn mule_run_finalizers() {
    let finalizers = std::mem::take(&mut *FINALIZERS.lock().unwrap());
    for finalizer in finalizers.into_iter().rev() {
        unsafe { finalizer() };
    }
}

thread_local! {
    static ARGUMENTS: Cell<Option<Arguments>> = const { Cell::new(None) };
}

// The `_time` native module: clock and calendar services for stdlib/time.py.

#[repr(C)]
struct Timespec {
    seconds: i64,
    nanoseconds: i64,
}

#[repr(C)]
#[derive(Default)]
struct ResourceLimits { soft: u64, hard: u64 }

#[repr(C)]
#[derive(Default)]
struct Timeval { seconds: i64, microseconds: i64 }

#[repr(C)]
#[derive(Default)]
struct ResourceUsage { user: Timeval, system: Timeval, counts: [i64; 14] }

#[repr(C)]
#[derive(Default)]
struct Tm {
    sec: i32,
    min: i32,
    hour: i32,
    mday: i32,
    mon: i32,
    year: i32,
    wday: i32,
    yday: i32,
    isdst: i32,
    gmtoff: i64,
    zone: *const c_char,
}

unsafe extern "C" {
    fn clock_gettime(clock: i32, spec: *mut Timespec) -> i32;
    fn clock_getres(clock: i32, spec: *mut Timespec) -> i32;
    fn nanosleep(requested: *const Timespec, remaining: *mut Timespec) -> i32;
    fn localtime_r(time: *const i64, result: *mut Tm) -> *mut Tm;
    fn gmtime_r(time: *const i64, result: *mut Tm) -> *mut Tm;
    fn tzset();
    static timezone: i64;
    static daylight: i32;
    static tzname: [*const c_char; 2];
}

unsafe fn time_float(seconds: i64, nanoseconds: i64) -> Value {
    unsafe {
        object::new_value(Data::Float(seconds as f64 + nanoseconds as f64 / 1_000_000_000.0, std::ptr::null_mut(), std::ptr::null_mut()))
    }
}

unsafe fn time_clock(clock: i32) -> Option<(i64, i64)> {
    unsafe {
        let mut spec = Timespec { seconds: 0, nanoseconds: 0 };
        if clock_gettime(clock, &mut spec) < 0 {
            object::error("OSError", &std::io::Error::last_os_error().to_string());
            return None;
        }
        Some((spec.seconds, spec.nanoseconds))
    }
}

macro_rules! time_clock_fn {
    ($name:ident, $clock:expr) => {
        unsafe extern "C" fn $name(_: Value, args: Value) -> Value {
            unsafe {
                if operands::<0>(args).is_none() { return null_mut(); }
                match time_clock($clock) {
                    Some((seconds, ns)) => time_float(seconds, ns),
                    None => null_mut(),
                }
            }
        }
    };
}
time_clock_fn!(time_monotonic, 1);
time_clock_fn!(time_wall, 0);
time_clock_fn!(time_process, 2);
time_clock_fn!(time_thread, 3);

unsafe fn clock_record(args: Value, read: unsafe extern "C" fn(i32, *mut Timespec) -> i32) -> Value {
    unsafe {
        let Some([clock]) = operands::<1>(args) else { return null_mut() };
        let Some(clock) = posix_int(clock).and_then(|clock| i32::try_from(clock).ok())
            else { return fail("OverflowError", "clock identifier does not fit clockid_t") };
        let mut spec = Timespec { seconds: 0, nanoseconds: 0 };
        let status = read(clock, &mut spec);
        let error = if status < 0 { std::io::Error::last_os_error().raw_os_error().unwrap_or(5) } else { 0 };
        posix_tuple(&[
            object::small(i64::from(error)),
            posix_number(i128::from(spec.seconds)),
            posix_number(i128::from(spec.nanoseconds)),
        ])
    }
}

unsafe extern "C" fn time_clock_parts(_: Value, args: Value) -> Value {
    unsafe { clock_record(args, clock_gettime) }
}

unsafe extern "C" fn time_resolution_parts(_: Value, args: Value) -> Value {
    unsafe { clock_record(args, clock_getres) }
}

unsafe extern "C" fn time_sleep(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands::<1>(args) else { return null_mut() };
        let Some(seconds) = numeric::number_double(value) else { return null_mut() };
        if seconds <= 0.0 {
            return object::none_value();
        }
        let whole = seconds.floor();
        let spec = Timespec {
            seconds: whole as i64,
            nanoseconds: ((seconds - whole) * 1_000_000_000.0) as i64,
        };
        nanosleep(&spec, std::ptr::null_mut());
        object::none_value()
    }
}

unsafe extern "C" fn time_parts(local: bool, value: Value) -> Value {
    unsafe {
        let seconds = numeric::number_double(value).map_or(0, |v| v.floor() as i64);
        let mut tm = Tm::default();
        let result = if local {
            localtime_r(&seconds, &mut tm)
        } else {
            gmtime_r(&seconds, &mut tm)
        };
        if result.is_null() {
            object::error("OSError", "timestamp out of range");
            return null_mut();
        }
        let fields = [
            tm.year + 1900, tm.mon + 1, tm.mday, tm.hour, tm.min, tm.sec,
            (tm.wday + 6) % 7, tm.yday + 1, tm.isdst,
        ];
        let values: Vec<Value> = fields.iter().map(|field| object::small(i64::from(*field))).collect();
        let result = object::mule_sequence_tuple(values.len() as isize, values.as_ptr());
        for value in values { object::release(value); }
        result
    }
}

unsafe extern "C" fn time_localtime(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands::<1>(args) else { return null_mut() };
        time_parts(true, value)
    }
}

unsafe extern "C" fn time_gmtime(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands::<1>(args) else { return null_mut() };
        time_parts(false, value)
    }
}

unsafe extern "C" fn time_zoneinfo(_: Value, args: Value) -> Value {
    unsafe {
        if operands::<0>(args).is_none() { return null_mut(); }
        tzset();
        let names: Vec<Value> = tzname.iter().map(|name| {
            if name.is_null() {
                object::new_value(Data::Text(Text::from_rust_str(""), std::ptr::null_mut(), std::ptr::null_mut()))
            } else {
                object::new_value(Data::Text(Text::from_wtf8(CStr::from_ptr(*name).to_bytes()).unwrap_or_else(|_| Text::from_rust_str("")), std::ptr::null_mut(), std::ptr::null_mut()))
            }
        }).collect();
        let values = [
            object::small(timezone),
            object::small(i64::from(daylight)),
            object::mule_sequence_tuple(names.len() as isize, names.as_ptr()),
        ];
        for name in names { object::release(name); }
        let result = object::mule_sequence_tuple(values.len() as isize, values.as_ptr());
        for value in &values[..2] { object::release(*value); }
        object::release(values[2]);
        result
    }
}

/// Populate the `_time` native module namespace.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_time_module_init(globals: Value) -> Value {
    unsafe {
        if object::has_error() {
            return null_mut();
        }
        let exports: [(&str, Native); 9] = [
            ("clock_parts", time_clock_parts),
            ("resolution_parts", time_resolution_parts),
            ("monotonic", time_monotonic),
            ("wall", time_wall),
            ("process", time_process),
            ("thread", time_thread),
            ("sleep_for", time_sleep),
            ("localtime_parts", time_localtime),
            ("gmtime_parts", time_gmtime),
        ];
        for (name, entry) in exports {
            let function = object::function(null_mut(), entry, name);
            let bound = object::bind(globals, name, function);
            object::release(function);
            if !bound { return null_mut(); }
        }
        let function = object::function(null_mut(), time_zoneinfo, "zoneinfo");
        let bound = object::bind(globals, "zoneinfo", function);
        object::release(function);
        if !bound { return null_mut(); }
        object::none_value()
    }
}

fn fail(kind: &str, message: &str) -> Value {
    object::error(kind, message);
    null_mut()
}

/// Publish native main's arguments before compiled language initialization.
///
/// # Safety
/// For count > 0, values addresses count + 1 valid pointer slots with a final
/// NULL sentinel. Non-NULL entries address NUL-terminated byte buffers. All
/// storage must remain valid and unchanged for the process lifetime. This and
/// module initialization run on the native main thread. Guards cannot validate
/// arbitrary addresses; only native startup may publish this trusted storage.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_set_process_arguments(count: i32, values: *const *const c_char) {
    ARGUMENTS.set(None);
    if object::has_error() {
        return;
    }
    if count < 0 || (count > 0 && values.is_null()) {
        object::error("SystemError", "invalid native process argument vector");
        return;
    }
    if count > 0 && !unsafe { *values.add(count as usize) }.is_null() {
        object::error(
            "SystemError",
            "native process argument vector lacks NULL sentinel",
        );
        return;
    }
    ARGUMENTS.set(Some(Arguments {
        count: count as usize,
        values,
    }));
}

unsafe fn operands<const N: usize>(args: Value) -> Option<[Value; N]> {
    unsafe {
        if object::has_error() {
            return None;
        }
        let count = object::argument_count(args)?;
        if crate::arguments::keyword_count(args) != 0 || count != N {
            object::error(
                "TypeError",
                &format!("expected {N} positional-only arguments"),
            );
            return None;
        }
        let mut values = [null_mut(); N];
        for (index, value) in values.iter_mut().enumerate() {
            *value = object::argument(args, index)?;
        }
        Some(values)
    }
}

unsafe fn storage_index(index: Value, length: usize) -> Option<usize> {
    unsafe {
        let index = if object::is_fixnum(index) {
            usize::try_from(object::fixnum(index)).ok()
        } else {
            numeric::integer_export(index)?
                .to_u64()
                .and_then(|value| usize::try_from(value).ok())
        };
        let Some(index) = index.filter(|index| *index < length) else {
            object::error("IndexError", "foreign storage index out of range");
            return None;
        };
        Some(index)
    }
}

unsafe extern "C" fn read_pointer(_: Value, args: Value) -> Value {
    unsafe {
        let Some([vector, index]) = operands(args) else {
            return null_mut();
        };
        if vector.is_null() || object::is_fixnum(vector) {
            return fail(
                "TypeError",
                "read_pointer requires a foreign pointer vector",
            );
        }
        let Data::ForeignPointerVector { base, length } = &(*vector).data else {
            return fail(
                "TypeError",
                "read_pointer requires a foreign pointer vector",
            );
        };
        let Some(index) = storage_index(index, *length) else {
            return null_mut();
        };
        let address = *base.add(index);
        if address.is_null() {
            object::none_value()
        } else {
            object::new_value(Data::ForeignCString { address })
        }
    }
}

unsafe extern "C" fn read_c_string(_: Value, args: Value) -> Value {
    unsafe {
        let Some([pointer]) = operands(args) else {
            return null_mut();
        };
        if pointer.is_null() || object::is_fixnum(pointer) {
            return fail(
                "TypeError",
                "read_c_string requires a foreign C string pointer",
            );
        }
        let Data::ForeignCString { address } = &(*pointer).data else {
            return fail(
                "TypeError",
                "read_c_string requires a foreign C string pointer",
            );
        };
        object::new_value(Data::Bytes(CStr::from_ptr(*address).to_bytes().to_vec(), std::ptr::null_mut(), std::ptr::null_mut()))
    }
}

unsafe fn bytes_storage<'a>(value: Value) -> Option<&'a [u8]> {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::Bytes(bytes, ..) = &(*value).data {
                return Some(bytes);
            }
        }
        object::error("TypeError", "byte storage requires bytes");
        None
    }
}

unsafe extern "C" fn byte_length(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands(args) else {
            return null_mut();
        };
        let Some(bytes) = bytes_storage(value) else {
            return null_mut();
        };
        numeric::integer_from_usize(bytes.len())
    }
}

unsafe extern "C" fn read_byte(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value, index]) = operands(args) else {
            return null_mut();
        };
        let Some(bytes) = bytes_storage(value) else {
            return null_mut();
        };
        let Some(index) = storage_index(index, bytes.len()) else {
            return null_mut();
        };
        object::small(i64::from(bytes[index]))
    }
}

unsafe extern "C" fn read_u64_le(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value, start]) = operands(args) else {
            return null_mut();
        };
        let Some(source) = bytes_storage(value) else {
            return null_mut();
        };
        if source.len() < 8 {
            return fail("IndexError", "word read exceeds byte storage");
        }
        let Some(start) = storage_index(start, source.len() - 7) else {
            return null_mut();
        };
        let word = u64::from_le_bytes(source[start..start + 8].try_into().unwrap());
        numeric::integer_from_usize(word as usize)
    }
}

unsafe extern "C" fn byte_copy(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value, start, length]) = operands(args) else {
            return null_mut();
        };
        let Some(source) = bytes_storage(value) else {
            return null_mut();
        };
        // Vec byte storage is bounded by isize::MAX, so the inclusive
        // offset/count bounds fit usize even for an empty source.
        let Some(start) = storage_index(start, source.len() + 1) else {
            return null_mut();
        };
        let Some(length) = storage_index(length, source.len() - start + 1) else {
            return null_mut();
        };
        let mut bytes = Vec::new();
        if bytes.try_reserve_exact(length).is_err() {
            return fail("MemoryError", "byte storage allocation failed");
        }
        bytes.extend_from_slice(&source[start..start + length]);
        object::new_value(Data::Bytes(bytes, null_mut(), null_mut()))
    }
}

unsafe extern "C" fn text_from_codepoints(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands(args) else {
            return null_mut();
        };
        if value.is_null() || object::is_fixnum(value) {
            return fail(
                "TypeError",
                "text_from_codepoints requires list or tuple storage",
            );
        }
        let (Data::List(values, ..) | Data::Args(values, ..)) = &(*value).data else {
            return fail(
                "TypeError",
                "text_from_codepoints requires list or tuple storage",
            );
        };
        let mut codes = Vec::new();
        if codes.try_reserve_exact(values.len()).is_err() {
            return fail("MemoryError", "text storage allocation failed");
        }
        for value in values {
            let code = if object::is_fixnum(*value) {
                u64::try_from(object::fixnum(*value)).ok()
            } else {
                let Some(integer) = numeric::integer_export(*value) else {
                    return null_mut();
                };
                integer.to_u64()
            };
            let Some(code) = code.filter(|code| *code <= 0x10ffff) else {
                return fail("ValueError", "text codepoint must be in range(0x110000)");
            };
            codes.push(code as u32);
        }
        match Text::from_codepoints(codes.into_iter()) {
            Ok(text) => object::new_value(Data::Text(text, std::ptr::null_mut(), std::ptr::null_mut())),
            Err(_) => fail("MemoryError", "text storage allocation failed"),
        }
    }
}

unsafe extern "C" fn bytes_from_values(_: Value, args: Value) -> Value {
    unsafe {
        let Some([value]) = operands(args) else {
            return null_mut();
        };
        let values = if !value.is_null() && !object::is_fixnum(value) {
            match &(*value).data {
                Data::List(values, ..) | Data::Args(values, ..) => Some(values),
                _ => None,
            }
        } else {
            None
        };
        let Some(values) = values else {
            return fail("TypeError", "bytes_from_values requires list or tuple storage");
        };
        let mut bytes = Vec::new();
        if bytes.try_reserve_exact(values.len()).is_err() {
            return fail("MemoryError", "byte storage allocation failed");
        }
        for value in values {
            let byte = if object::is_fixnum(*value) {
                u8::try_from(object::fixnum(*value)).ok()
            } else {
                None
            };
            let Some(byte) = byte else {
                return fail("ValueError", "byte must be in range(0, 256)");
            };
            bytes.push(byte);
        }
        object::new_value(Data::Bytes(bytes, std::ptr::null_mut(), std::ptr::null_mut()))
    }
}

unsafe extern "C" {
    fn isatty(fd: i32) -> i32;
    fn write(fd: i32, buffer: *const u8, count: usize) -> isize;
}

/// `write_fd(fd, data)`: write bytes to a descriptor, returning the count
/// written. Standard output and error share the buffers `print` uses.
unsafe extern "C" fn write_fd(_: Value, args: Value) -> Value {
    unsafe {
        use std::io::Write;
        let Some([fd, data]) = operands(args) else {
            return null_mut();
        };
        if !object::is_fixnum(fd) {
            object::error("TypeError", "a file descriptor must be an integer");
            return null_mut();
        }
        let Data::Bytes(bytes, ..) = &(*data).data else {
            object::error("TypeError", "a bytes-like object is required");
            return null_mut();
        };
        let written = match object::fixnum(fd) {
            1 => std::io::stdout().lock().write_all(bytes).map(|_| bytes.len()),
            2 => std::io::stderr().lock().write_all(bytes).map(|_| bytes.len()),
            descriptor => {
                let count = write(descriptor as i32, bytes.as_ptr(), bytes.len());
                if count < 0 {
                    Err(std::io::Error::last_os_error())
                } else {
                    Ok(count as usize)
                }
            }
        };
        match written {
            Ok(count) => numeric::integer_from_usize(count),
            Err(failure) => {
                object::error("OSError", &failure.to_string());
                null_mut()
            }
        }
    }
}

unsafe extern "C" fn flush_fd(_: Value, args: Value) -> Value {
    unsafe {
        use std::io::Write;
        let Some([fd]) = operands(args) else {
            return null_mut();
        };
        let flushed = match object::is_fixnum(fd).then(|| object::fixnum(fd)) {
            Some(1) => std::io::stdout().lock().flush(),
            Some(2) => std::io::stderr().lock().flush(),
            _ => Ok(()),
        };
        if let Err(failure) = flushed {
            object::error("OSError", &failure.to_string());
            return null_mut();
        }
        object::none_value()
    }
}

/// The exception being handled, or None (`sys.exception()`).
unsafe extern "C" fn active_exception(_: Value, args: Value) -> Value {
    unsafe {
        let Some([]) = operands::<0>(args) else {
            return null_mut();
        };
        crate::sequences::mule_active_exception()
    }
}

/// The frame `depth` steps out from `sys._getframe`'s own frame, or None
/// when the call stack is not that deep (`sys._getframe`).
unsafe extern "C" fn current_frame(_: Value, args: Value) -> Value {
    unsafe {
        let Some([depth]) = operands::<1>(args) else {
            return null_mut();
        };
        if !object::is_fixnum(depth) || object::fixnum(depth) < 0 {
            object::error("SystemError", "frame depth must be a non-negative integer");
            return null_mut();
        }
        let frame = crate::traceback::frame_at(object::fixnum(depth) as usize);
        if frame.is_null() {
            object::none_value()
        } else {
            frame
        }
    }
}

unsafe extern "C" fn isatty_fd(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd]) = operands(args) else {
            return null_mut();
        };
        let Some(fd) = crate::numeric::integer_export(fd) else {
            return null_mut();
        };
        let Some(fd) = fd.to_i64().and_then(|fd| i32::try_from(fd).ok()) else {
            object::error("OverflowError", "Python int too large to convert to C int");
            return null_mut();
        };
        object::bool_value(isatty(fd) == 1)
    }
}

// The `_posix` native module: operating-system services for stdlib/posix.py.
// Values cross as bytes/rows; compiled Python owns the Python-level types.

unsafe fn posix_path(value: Value) -> Option<Vec<u8>> {
    unsafe {
        if value.is_null() || object::is_fixnum(value) {
            object::error("TypeError", "expected str or bytes path");
            return None;
        }
        match &(*value).data {
            Data::Text(text, ..) => Some(text.to_wtf8()),
            Data::Bytes(bytes, ..) => Some(bytes.clone()),
            _ => {
                object::error("TypeError", "expected str or bytes path");
                None
            }
        }
    }
}

unsafe fn posix_int(value: Value) -> Option<i64> {
    unsafe {
        if object::is_fixnum(value) {
            Some(object::fixnum(value))
        } else {
            numeric::integer_export(value)?.to_i64()
        }
    }
}

unsafe fn posix_number(value: i128) -> Value {
    unsafe {
        if let Ok(small) = i64::try_from(value) {
            if (-(1i64 << 62)..(1i64 << 62)).contains(&small) {
                return object::small(small);
            }
        }
        numeric::integer_import_owned(crate::gmp::BigInt::from_decimal(&value.to_string()).expect("i128 decimal"))
    }
}

fn posix_error(error: &std::io::Error) -> Value {
    let kind = match error.kind() {
        std::io::ErrorKind::NotFound => "FileNotFoundError",
        std::io::ErrorKind::AlreadyExists => "FileExistsError",
        std::io::ErrorKind::PermissionDenied => "PermissionError",
        std::io::ErrorKind::IsADirectory => "IsADirectoryError",
        std::io::ErrorKind::NotADirectory => "NotADirectoryError",
        _ => "OSError",
    };
    object::error(kind, &error.to_string());
    null_mut()
}

unsafe fn posix_text(bytes: &[u8]) -> Value {
    unsafe { object::new_value(Data::Text(Text::from_wtf8(bytes).unwrap_or_else(|_| Text::from_rust_str("")), std::ptr::null_mut(), std::ptr::null_mut())) }
}

unsafe fn posix_bytes(bytes: &[u8]) -> Value {
    unsafe { object::new_value(Data::Bytes(bytes.to_vec(), std::ptr::null_mut(), std::ptr::null_mut())) }
}

unsafe fn posix_tuple(values: &[Value]) -> Value {
    unsafe {
        let result = object::mule_sequence_tuple(values.len() as isize, values.as_ptr());
        for value in values {
            object::release(*value);
        }
        result
    }
}

unsafe extern "C" fn posix_cwd(_: Value, args: Value) -> Value {
    unsafe {
        if operands::<0>(args).is_none() {
            return null_mut();
        }
        match std::env::current_dir() {
            Ok(path) => posix_text(path.as_os_str().as_encoded_bytes()),
            Err(error) => posix_error(&error),
        }
    }
}

unsafe extern "C" fn posix_chdir(_: Value, args: Value) -> Value {
    unsafe {
        let Some([path]) = operands::<1>(args) else { return null_mut() };
        let Some(path) = posix_path(path) else { return null_mut() };
        match std::env::set_current_dir(std::ffi::OsStr::from_bytes(&path)) {
            Ok(()) => object::none_value(),
            Err(error) => posix_error(&error),
        }
    }
}

unsafe extern "C" fn posix_listdir(_: Value, args: Value) -> Value {
    unsafe {
        let Some([path]) = operands::<1>(args) else { return null_mut() };
        let Some(path) = posix_path(path) else { return null_mut() };
        match std::fs::read_dir(std::ffi::OsStr::from_bytes(&path)) {
            Ok(entries) => {
                let mut names = Vec::new();
                for entry in entries {
                    match entry {
                        Ok(entry) => names.push(posix_text(entry.file_name().as_encoded_bytes())),
                        Err(error) => {
                            for name in names { object::release(name); }
                            return posix_error(&error);
                        }
                    }
                }
                posix_tuple(&names)
            }
            Err(error) => posix_error(&error),
        }
    }
}

unsafe fn posix_stat_row(metadata: &std::fs::Metadata) -> Value {
    unsafe {
        use std::os::unix::fs::MetadataExt;
        let values = [
            posix_number(i128::from(metadata.mode())),
            posix_number(i128::from(metadata.ino())),
            posix_number(i128::from(metadata.dev())),
            posix_number(i128::from(metadata.nlink())),
            posix_number(i128::from(metadata.uid())),
            posix_number(i128::from(metadata.gid())),
            posix_number(i128::from(metadata.size())),
            posix_number(i128::from(metadata.atime_nsec()) + i128::from(metadata.atime()) * 1_000_000_000),
            posix_number(i128::from(metadata.mtime_nsec()) + i128::from(metadata.mtime()) * 1_000_000_000),
            posix_number(i128::from(metadata.ctime_nsec()) + i128::from(metadata.ctime()) * 1_000_000_000),
        ];
        posix_tuple(&values)
    }
}

unsafe extern "C" fn posix_stat(_: Value, args: Value) -> Value {
    unsafe {
        let Some([path, follow]) = operands::<2>(args) else { return null_mut() };
        let Some(path) = posix_path(path) else { return null_mut() };
        let path = std::ffi::OsStr::from_bytes(&path);
        let result = if object::truth(follow).unwrap_or(false) {
            std::fs::metadata(path)
        } else {
            std::fs::symlink_metadata(path)
        };
        match result {
            Ok(metadata) => posix_stat_row(&metadata),
            Err(error) => posix_error(&error),
        }
    }
}

#[repr(C)]
struct DescriptorStat {
    device: u64,
    inode: u64,
    links: u64,
    mode: u32,
    uid: u32,
    gid: u32,
    padding: i32,
    special_device: u64,
    size: i64,
    block_size: i64,
    blocks: i64,
    accessed: Timespec,
    modified: Timespec,
    changed: Timespec,
    reserved: [i64; 3],
}

const _: () = {
    assert!(std::mem::size_of::<DescriptorStat>() == 144);
    assert!(std::mem::offset_of!(DescriptorStat, mode) == 24);
    assert!(std::mem::offset_of!(DescriptorStat, accessed) == 72);
};

unsafe extern "C" {
    #[link_name = "fstat"]
    fn descriptor_stat(fd: i32, result: *mut DescriptorStat) -> i32;
}

unsafe extern "C" fn posix_fstat(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd]) = operands::<1>(args) else { return null_mut() };
        let Some(fd) = posix_int(fd).and_then(|value| i32::try_from(value).ok())
            else { return fail("OverflowError", "file descriptor does not fit C int") };
        let mut metadata: DescriptorStat = std::mem::zeroed();
        if descriptor_stat(fd, &mut metadata) < 0 {
            let error = std::io::Error::last_os_error().raw_os_error().unwrap_or(5);
            return posix_tuple(&[object::small(i64::from(error)), object::none_value()]);
        }
        let row = posix_tuple(&[
            posix_number(i128::from(metadata.mode)),
            posix_number(i128::from(metadata.inode)),
            posix_number(i128::from(metadata.device)),
            posix_number(i128::from(metadata.links)),
            posix_number(i128::from(metadata.uid)),
            posix_number(i128::from(metadata.gid)),
            posix_number(i128::from(metadata.size)),
            posix_number(i128::from(metadata.accessed.seconds) * 1_000_000_000 + i128::from(metadata.accessed.nanoseconds)),
            posix_number(i128::from(metadata.modified.seconds) * 1_000_000_000 + i128::from(metadata.modified.nanoseconds)),
            posix_number(i128::from(metadata.changed.seconds) * 1_000_000_000 + i128::from(metadata.changed.nanoseconds)),
        ]);
        posix_tuple(&[object::small(0), row])
    }
}

unsafe extern "C" fn posix_mkdir(_: Value, args: Value) -> Value {
    unsafe {
        let Some([path, mode]) = operands::<2>(args) else { return null_mut() };
        let Some(path) = posix_path(path) else { return null_mut() };
        let Some(mode) = posix_int(mode) else { return fail("TypeError", "mode must be an integer") };
        match std::fs::DirBuilder::new().mode(mode as u32).create(std::ffi::OsStr::from_bytes(&path)) {
            Ok(()) => object::none_value(),
            Err(error) => posix_error(&error),
        }
    }
}

unsafe extern "C" fn posix_rmdir(_: Value, args: Value) -> Value {
    unsafe {
        let Some([path]) = operands::<1>(args) else { return null_mut() };
        let Some(path) = posix_path(path) else { return null_mut() };
        match std::fs::remove_dir(std::ffi::OsStr::from_bytes(&path)) {
            Ok(()) => object::none_value(),
            Err(error) => posix_error(&error),
        }
    }
}

unsafe extern "C" fn posix_unlink(_: Value, args: Value) -> Value {
    unsafe {
        let Some([path]) = operands::<1>(args) else { return null_mut() };
        let Some(path) = posix_path(path) else { return null_mut() };
        match std::fs::remove_file(std::ffi::OsStr::from_bytes(&path)) {
            Ok(()) => object::none_value(),
            Err(error) => posix_error(&error),
        }
    }
}

unsafe extern "C" fn posix_rename(_: Value, args: Value) -> Value {
    unsafe {
        let Some([source, target]) = operands::<2>(args) else { return null_mut() };
        let (Some(source), Some(target)) = (posix_path(source), posix_path(target)) else { return null_mut() };
        match std::fs::rename(std::ffi::OsStr::from_bytes(&source), std::ffi::OsStr::from_bytes(&target)) {
            Ok(()) => object::none_value(),
            Err(error) => posix_error(&error),
        }
    }
}

unsafe extern "C" fn posix_chmod_at(_: Value, args: Value) -> Value {
    unsafe {
        let Some([path, mode, directory, follow]) = operands::<4>(args) else { return null_mut() };
        let (Some(path), Some(mode), Some(directory), Some(follow)) =
            (posix_path(path), posix_int(mode), posix_int(directory), object::truth(follow))
            else { return null_mut() };
        let path = match std::ffi::CString::new(path) {
            Ok(path) => path,
            Err(_) => return fail("ValueError", "embedded null byte"),
        };
        if fchmodat(directory as i32, path.as_ptr(), mode as u32, if follow { 0 } else { 0x100 }) < 0 {
            return posix_error(&std::io::Error::last_os_error());
        }
        object::none_value()
    }
}

unsafe extern "C" fn posix_fchmod(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd, mode]) = operands::<2>(args) else { return null_mut() };
        let (Some(fd), Some(mode)) = (posix_int(fd), posix_int(mode)) else { return null_mut() };
        if fchmod(fd as i32, mode as u32) < 0 {
            return posix_error(&std::io::Error::last_os_error());
        }
        object::none_value()
    }
}

unsafe extern "C" fn posix_utime_at(_: Value, args: Value) -> Value {
    unsafe {
        let Some([path, directory, at_sec, at_ns, mt_sec, mt_ns, flags]) = operands::<7>(args)
            else { return null_mut() };
        let default_directory = directory == object::none_value();
        let (Some(directory), Some(flags)) = (if default_directory { Some(-100) } else { posix_int(directory) }, posix_int(flags))
            else { return null_mut() };
        let mut times = [Timespec { seconds: 0, nanoseconds: 0 }, Timespec { seconds: 0, nanoseconds: 0 }];
        let pointer = if at_sec == object::none_value() {
            std::ptr::null()
        } else {
            let (Some(a), Some(b), Some(c), Some(d)) =
                (posix_int(at_sec), posix_int(at_ns), posix_int(mt_sec), posix_int(mt_ns))
                else { return null_mut() };
            times[0] = Timespec { seconds: a, nanoseconds: b };
            times[1] = Timespec { seconds: c, nanoseconds: d };
            times.as_ptr()
        };
        let filename = if path == object::none_value() { None } else {
            let Some(bytes) = posix_path(path) else { return null_mut() };
            match std::ffi::CString::new(bytes) {
                Ok(name) => Some(name),
                Err(_) => return fail("ValueError", "embedded null byte"),
            }
        };
        let result = match filename.as_ref() {
            Some(name) => utimensat(directory as i32, name.as_ptr(), pointer, flags as i32),
            None if default_directory => utimensat(directory as i32, std::ptr::null(), pointer, flags as i32),
            None => futimens(directory as i32, pointer),
        };
        let error = if result < 0 { std::io::Error::last_os_error().raw_os_error().unwrap_or(5) } else { 0 };
        object::small(i64::from(error))
    }
}

// Descriptor-control services return errno and the physical result; compiled
// fcntl policy selects exception classes and retry behavior.
unsafe fn descriptor_control_result(value: i32, payload: Value) -> Value {
    unsafe {
        let error = if value < 0 {
            std::io::Error::last_os_error().raw_os_error().unwrap_or(5)
        } else { 0 };
        posix_tuple(&[object::small(i64::from(error)), payload])
    }
}

unsafe extern "C" fn posix_flock(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd, operation]) = operands::<2>(args) else { return null_mut() };
        let (Some(fd), Some(operation)) = (posix_int(fd), posix_int(operation)) else { return null_mut() };
        descriptor_control_result(flock(fd as i32, operation as i32), object::none_value())
    }
}

unsafe extern "C" fn posix_strerror(_: Value, args: Value) -> Value {
    unsafe {
        let Some([code]) = operands::<1>(args) else { return null_mut() };
        let Some(code) = posix_int(code) else { return null_mut() };
        let message = strerror(code as i32);
        if message.is_null() { object::error("ValueError", "strerror() argument out of range"); return null_mut(); }
        posix_bytes(CStr::from_ptr(message).to_bytes())
    }
}

unsafe fn resource_limits_result(status: i32, limits: ResourceLimits) -> Value {
    unsafe {
        let error = if status < 0 { std::io::Error::last_os_error().raw_os_error().unwrap_or(5) } else { 0 };
        let payload = if status < 0 { object::none_value() } else {
            let soft = if limits.soft == u64::MAX { -1 } else { i128::from(limits.soft) };
            let hard = if limits.hard == u64::MAX { -1 } else { i128::from(limits.hard) };
            posix_tuple(&[posix_number(soft), posix_number(hard)])
        };
        posix_tuple(&[object::small(i64::from(error)), payload])
    }
}

unsafe extern "C" fn posix_getrlimit(_: Value, args: Value) -> Value {
    unsafe {
        let Some([resource]) = operands::<1>(args) else { return null_mut() };
        let Some(resource) = posix_int(resource) else { return null_mut() };
        let mut limits = ResourceLimits::default();
        let status = getrlimit(resource as i32, &mut limits);
        resource_limits_result(status, limits)
    }
}

unsafe extern "C" fn posix_setrlimit(_: Value, args: Value) -> Value {
    unsafe {
        let Some([resource, soft, hard]) = operands::<3>(args) else { return null_mut() };
        let (Some(resource), Some(soft), Some(hard)) = (posix_int(resource), posix_int(soft), posix_int(hard)) else { return null_mut() };
        let limits = ResourceLimits { soft: soft as u64, hard: hard as u64 };
        descriptor_control_result(setrlimit(resource as i32, &limits), object::none_value())
    }
}

unsafe extern "C" fn posix_prlimit(_: Value, args: Value) -> Value {
    unsafe {
        let Some([pid, resource, soft, hard, change]) = operands::<5>(args) else { return null_mut() };
        let (Some(pid), Some(resource), Some(soft), Some(hard), Some(change)) = (posix_int(pid), posix_int(resource), posix_int(soft), posix_int(hard), posix_int(change)) else { return null_mut() };
        let proposed = ResourceLimits { soft: soft as u64, hard: hard as u64 };
        let mut previous = ResourceLimits::default();
        let status = prlimit(pid as i32, resource as i32, if change == 0 { std::ptr::null() } else { &proposed }, &mut previous);
        resource_limits_result(status, previous)
    }
}

unsafe extern "C" fn posix_getrusage(_: Value, args: Value) -> Value {
    unsafe {
        let Some([who]) = operands::<1>(args) else { return null_mut() };
        let Some(who) = posix_int(who) else { return null_mut() };
        let mut usage = ResourceUsage::default();
        if getrusage(who as i32, &mut usage) < 0 { return descriptor_control_result(-1, object::none_value()); }
        let mut values = Vec::with_capacity(16);
        for time in [usage.user, usage.system] {
            values.push(object::new_value(Data::Float(time.seconds as f64 + time.microseconds as f64 * 0.000001, null_mut(), null_mut())));
        }
        for count in usage.counts { values.push(posix_number(i128::from(count))); }
        let payload = posix_tuple(&values);
        posix_tuple(&[object::small(0), payload])
    }
}

unsafe extern "C" fn posix_getpagesize(_: Value, args: Value) -> Value {
    unsafe {
        if operands::<0>(args).is_none() { return null_mut(); }
        object::small(i64::from(getpagesize()))
    }
}

unsafe extern "C" fn posix_fcntl_int(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd, command, argument]) = operands::<3>(args) else { return null_mut() };
        let (Some(fd), Some(command), Some(argument)) = (posix_int(fd), posix_int(command), posix_int(argument)) else { return null_mut() };
        let value = fcntl(fd as i32, command as i32, argument as i32);
        descriptor_control_result(value, object::small(i64::from(value)))
    }
}

unsafe extern "C" fn posix_fcntl_buffer(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd, command, argument]) = operands::<3>(args) else { return null_mut() };
        let (Some(fd), Some(command)) = (posix_int(fd), posix_int(command)) else { return null_mut() };
        if argument.is_null() || object::is_fixnum(argument) { object::error("TypeError", "native fcntl requires byte storage"); return null_mut(); }
        let Data::Bytes(data, ..) = &(*argument).data else { object::error("TypeError", "native fcntl requires byte storage"); return null_mut(); };
        if data.len() > 1024 { object::error("ValueError", "fcntl argument 3 is too long"); return null_mut(); }
        let mut buffer = [0u8; 1024];
        buffer[..data.len()].copy_from_slice(data);
        let value = fcntl(fd as i32, command as i32, buffer.as_mut_ptr());
        // Capture errno before allocating the output buffer.
        let error = if value < 0 { std::io::Error::last_os_error().raw_os_error().unwrap_or(5) } else { 0 };
        posix_tuple(&[object::small(i64::from(error)), posix_bytes(&buffer[..data.len()])])
    }
}

unsafe extern "C" fn posix_link(_: Value, args: Value) -> Value {
    unsafe {
        let Some([source, target]) = operands::<2>(args) else { return null_mut() };
        let (Some(source), Some(target)) = (posix_path(source), posix_path(target)) else { return null_mut() };
        match std::fs::hard_link(std::ffi::OsStr::from_bytes(&source), std::ffi::OsStr::from_bytes(&target)) {
            Ok(()) => object::none_value(),
            Err(error) => posix_error(&error),
        }
    }
}

unsafe extern "C" fn posix_symlink(_: Value, args: Value) -> Value {
    unsafe {
        let Some([source, target]) = operands::<2>(args) else { return null_mut() };
        let (Some(source), Some(target)) = (posix_path(source), posix_path(target)) else { return null_mut() };
        match std::os::unix::fs::symlink(std::ffi::OsStr::from_bytes(&source), std::ffi::OsStr::from_bytes(&target)) {
            Ok(()) => object::none_value(),
            Err(error) => posix_error(&error),
        }
    }
}

unsafe extern "C" fn posix_executable_path(_: Value, args: Value) -> Value {
    unsafe {
        let Some([]) = operands::<0>(args) else { return null_mut() };
        match std::env::current_exe() {
            Ok(path) => posix_bytes(path.as_os_str().as_encoded_bytes()),
            Err(error) => posix_error(&error),
        }
    }
}

unsafe extern "C" fn posix_readlink(_: Value, args: Value) -> Value {
    unsafe {
        let Some([path]) = operands::<1>(args) else { return null_mut() };
        let Some(path) = posix_path(path) else { return null_mut() };
        match std::fs::read_link(std::ffi::OsStr::from_bytes(&path)) {
            Ok(target) => posix_text(target.as_os_str().as_encoded_bytes()),
            Err(error) => posix_error(&error),
        }
    }
}

unsafe extern "C" fn posix_access(_: Value, args: Value) -> Value {
    unsafe {
        let Some([path, mode]) = operands::<2>(args) else { return null_mut() };
        let Some(path) = posix_path(path) else { return null_mut() };
        let Some(mode) = posix_int(mode) else { return fail("TypeError", "mode must be an integer") };
        let Ok(c_path) = std::ffi::CString::new(path) else { return object::bool_value(false) };
        object::bool_value(unsafe { access(c_path.as_ptr(), mode as i32) } == 0)
    }
}

unsafe extern "C" fn posix_open(_: Value, args: Value) -> Value {
    unsafe {
        let Some([path, flags, mode]) = operands::<3>(args) else { return null_mut() };
        let Some(path) = posix_path(path) else { return null_mut() };
        let (Some(flags), Some(mode)) = (posix_int(flags), posix_int(mode)) else { return fail("TypeError", "flags and mode must be integers") };
        let Ok(c_path) = std::ffi::CString::new(path) else { return fail("ValueError", "embedded null byte") };
        let fd = unsafe { open(c_path.as_ptr(), flags as i32, mode as i32) };
        if fd < 0 { return posix_error(&std::io::Error::last_os_error()); }
        object::small(i64::from(fd))
    }
}

unsafe extern "C" fn posix_read(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd, count]) = operands::<2>(args) else { return null_mut() };
        let (Some(fd), Some(count)) = (posix_int(fd), posix_int(count)) else { return fail("TypeError", "fd and count must be integers") };
        let mut buffer = vec![0u8; count.max(0) as usize];
        let read = unsafe { read(fd as i32, buffer.as_mut_ptr().cast(), buffer.len()) };
        if read < 0 { return posix_error(&std::io::Error::last_os_error()); }
        buffer.truncate(read as usize);
        posix_bytes(&buffer)
    }
}

unsafe extern "C" fn posix_write(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd, data]) = operands::<2>(args) else { return null_mut() };
        let Some(fd) = posix_int(fd) else { return fail("TypeError", "fd must be an integer") };
        let Some(data) = posix_path(data) else { return null_mut() };
        let written = unsafe { write(fd as i32, data.as_ptr().cast(), data.len()) };
        if written < 0 { return posix_error(&std::io::Error::last_os_error()); }
        posix_number(i128::from(written as i64))
    }
}

unsafe extern "C" fn posix_close(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd]) = operands::<1>(args) else { return null_mut() };
        let Some(fd) = posix_int(fd) else { return fail("TypeError", "fd must be an integer") };
        if unsafe { close(fd as i32) } < 0 { return posix_error(&std::io::Error::last_os_error()); }
        object::none_value()
    }
}

unsafe extern "C" fn posix_lseek(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd, offset, whence]) = operands::<3>(args) else { return null_mut() };
        let (Some(fd), Some(offset), Some(whence)) = (posix_int(fd), posix_int(offset), posix_int(whence)) else { return fail("TypeError", "lseek arguments must be integers") };
        let result = unsafe { lseek(fd as i32, offset, whence as i32) };
        if result < 0 { return posix_error(&std::io::Error::last_os_error()); }
        posix_number(i128::from(result))
    }
}

unsafe extern "C" fn posix_fsync(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd]) = operands::<1>(args) else { return null_mut() };
        let Some(fd) = posix_int(fd) else { return fail("TypeError", "fd must be an integer") };
        if unsafe { fsync(fd as i32) } < 0 { return posix_error(&std::io::Error::last_os_error()); }
        object::none_value()
    }
}

unsafe extern "C" fn posix_ftruncate(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd, length]) = operands::<2>(args) else { return null_mut(); };
        let (Some(fd), Some(length)) = (posix_int(fd), posix_int(length)) else {
            return fail("OverflowError", "file truncation arguments exceed native integer range");
        };
        loop {
            if ftruncate(fd as i32, length) == 0 { return object::none_value(); }
            let error = std::io::Error::last_os_error();
            if error.raw_os_error() == Some(4) { continue; }
            return posix_error(&error);
        }
    }
}

unsafe extern "C" fn posix_dup(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd]) = operands::<1>(args) else { return null_mut() };
        let Some(fd) = posix_int(fd) else { return fail("TypeError", "fd must be an integer") };
        let result = unsafe { dup(fd as i32) };
        if result < 0 { return posix_error(&std::io::Error::last_os_error()); }
        object::small(i64::from(result))
    }
}

unsafe extern "C" fn posix_dup2(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd, fd2]) = operands::<2>(args) else { return null_mut() };
        let (Some(fd), Some(fd2)) = (posix_int(fd), posix_int(fd2)) else { return fail("TypeError", "fds must be integers") };
        if unsafe { dup2(fd as i32, fd2 as i32) } < 0 { return posix_error(&std::io::Error::last_os_error()); }
        object::none_value()
    }
}

unsafe extern "C" fn posix_pipe(_: Value, args: Value) -> Value {
    unsafe {
        if operands::<0>(args).is_none() { return null_mut(); }
        let mut fds = [0i32; 2];
        if unsafe { pipe2(fds.as_mut_ptr(), 0x80000) } < 0 { return posix_error(&std::io::Error::last_os_error()); }
        posix_tuple(&[object::small(i64::from(fds[0])), object::small(i64::from(fds[1]))])
    }
}

unsafe extern "C" {
    fn ioctl(fd: i32, request: u64, ...) -> i32;
}

unsafe extern "C" fn posix_terminal_size(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd]) = operands::<1>(args) else { return null_mut(); };
        let Some(fd) = posix_int(fd) else { return fail("TypeError", "fd must be an integer"); };
        let Ok(fd) = i32::try_from(fd) else { return fail("OverflowError", "Python int too large to convert to C int"); };
        let mut dimensions = [0u16; 4];
        if ioctl(fd, 0x5413, dimensions.as_mut_ptr()) < 0 {
            return posix_error(&std::io::Error::last_os_error());
        }
        posix_tuple(&[object::small(i64::from(dimensions[1])), object::small(i64::from(dimensions[0]))])
    }
}

unsafe extern "C" fn posix_environ(_: Value, args: Value) -> Value {
    unsafe {
        if operands::<0>(args).is_none() { return null_mut(); }
        let mut pairs = Vec::new();
        for (key, value) in std::env::vars_os() {
            let pair = posix_tuple(&[posix_bytes(key.as_encoded_bytes()), posix_bytes(value.as_encoded_bytes())]);
            pairs.push(pair);
        }
        posix_tuple(&pairs)
    }
}

unsafe extern "C" {
    fn getrandom(buffer: *mut u8, count: usize, flags: u32) -> isize;
}

unsafe extern "C" fn posix_entropy(_: Value, args: Value) -> Value {
    unsafe {
        let Some([length]) = operands::<1>(args) else { return null_mut() };
        let Some(length) = posix_int(length) else {
            if !object::has_error() {
                object::error("OverflowError", "byte storage length is too large");
            }
            return null_mut();
        };
        let Ok(length) = usize::try_from(length) else {
            object::error("ValueError", "byte storage length must be nonnegative");
            return null_mut();
        };
        let mut bytes = Vec::new();
        if bytes.try_reserve_exact(length).is_err() {
            object::error("MemoryError", "entropy byte storage allocation failed");
            return null_mut();
        }
        bytes.resize(length, 0);
        let mut offset = 0;
        while offset < length {
            let count = getrandom(bytes.as_mut_ptr().add(offset), length - offset, 0);
            if count < 0 {
                let error = std::io::Error::last_os_error();
                if error.kind() == std::io::ErrorKind::Interrupted {
                    continue;
                }
                return posix_error(&error);
            }
            if count == 0 {
                return posix_error(&std::io::Error::from(std::io::ErrorKind::UnexpectedEof));
            }
            offset += count as usize;
        }
        object::new_value(Data::Bytes(bytes, null_mut(), null_mut()))
    }
}

unsafe extern "C" fn posix_setenv(_: Value, args: Value) -> Value {
    unsafe {
        let Some([name, value]) = operands::<2>(args) else { return null_mut() };
        let (Some(name), Some(value)) = (posix_path(name), posix_path(value)) else { return null_mut() };
        unsafe { std::env::set_var(std::ffi::OsStr::from_bytes(&name), std::ffi::OsStr::from_bytes(&value)) };
        object::none_value()
    }
}

unsafe extern "C" fn posix_unsetenv(_: Value, args: Value) -> Value {
    unsafe {
        let Some([name]) = operands::<1>(args) else { return null_mut() };
        let Some(name) = posix_path(name) else { return null_mut() };
        unsafe { std::env::remove_var(std::ffi::OsStr::from_bytes(&name)) };
        object::none_value()
    }
}

unsafe extern "C" fn posix_uname(_: Value, args: Value) -> Value {
    unsafe {
        if operands::<0>(args).is_none() { return null_mut(); }
        let mut info: UtsName = unsafe { std::mem::zeroed() };
        if unsafe { uname(&mut info) } < 0 { return posix_error(&std::io::Error::last_os_error()); }
        let fields = [&info.sysname, &info.nodename, &info.release, &info.version, &info.machine];
        let values: Vec<Value> = fields.iter().map(|field| {
            let bytes = unsafe { std::ffi::CStr::from_ptr(field.as_ptr()) }.to_bytes();
            posix_text(bytes)
        }).collect();
        posix_tuple(&values)
    }
}

unsafe fn posix_identity(name: &str) -> Value {
    unsafe {
        let value = match name {
            "getpid" => i64::from(std::process::id()),
            "getppid" => i64::from(getppid()),
            "getuid" => i64::from(getuid()),
            "geteuid" => i64::from(geteuid()),
            "getgid" => i64::from(getgid()),
            "getegid" => i64::from(getegid()),
            _ => unreachable!(),
        };
        object::small(value)
    }
}

macro_rules! posix_zero {
    ($name:ident, $label:literal) => {
        unsafe extern "C" fn $name(_: Value, args: Value) -> Value {
            unsafe {
                if operands::<0>(args).is_none() { return null_mut(); }
                posix_identity($label)
            }
        }
    };
}
posix_zero!(posix_getpid, "getpid");
posix_zero!(posix_getppid, "getppid");
posix_zero!(posix_getuid, "getuid");
posix_zero!(posix_geteuid, "geteuid");
posix_zero!(posix_getgid, "getgid");
posix_zero!(posix_getegid, "getegid");

unsafe extern "C" fn posix_umask(_: Value, args: Value) -> Value {
    unsafe {
        let Some([mask]) = operands::<1>(args) else { return null_mut() };
        let Some(mask) = posix_int(mask) else { return fail("TypeError", "mask must be an integer") };
        object::small(i64::from(unsafe { umask(mask as u32) }))
    }
}

unsafe extern "C" fn posix_kill(_: Value, args: Value) -> Value {
    unsafe {
        let Some([pid, signal]) = operands::<2>(args) else { return null_mut() };
        let (Some(pid), Some(signal)) = (posix_int(pid), posix_int(signal)) else { return fail("TypeError", "pid and signal must be integers") };
        if unsafe { kill(pid as i32, signal as i32) } < 0 { return posix_error(&std::io::Error::last_os_error()); }
        object::none_value()
    }
}

unsafe extern "C" fn posix_sysconf(_: Value, args: Value) -> Value {
    unsafe {
        let Some([name]) = operands::<1>(args) else { return null_mut() };
        let Some(name) = posix_int(name) else { return fail("TypeError", "sysconf name must be an integer") };
        let value = unsafe { sysconf(name as i32) };
        if value < 0 { return posix_error(&std::io::Error::last_os_error()); }
        posix_number(i128::from(value))
    }
}

unsafe extern "C" fn posix_sched_getaffinity(_: Value, args: Value) -> Value {
    unsafe {
        let Some([pid, size]) = operands::<2>(args) else { return null_mut() };
        let Some(pid) = posix_int(pid).and_then(|pid| i32::try_from(pid).ok())
            else { return fail("OverflowError", "process ID does not fit pid_t") };
        let Some(size) = posix_int(size).and_then(|size| usize::try_from(size).ok())
            else { return fail("ValueError", "invalid CPU mask size") };
        if size == 0 || size % 8 != 0 { return fail("ValueError", "CPU mask size must be a positive multiple of eight") }
        let mut mask = Vec::<u64>::new();
        if mask.try_reserve_exact(size / 8).is_err() { return fail("MemoryError", "cannot allocate CPU affinity mask") }
        mask.resize(size / 8, 0);
        let result = sched_getaffinity(pid, size, mask.as_mut_ptr().cast());
        let error = if result < 0 { std::io::Error::last_os_error().raw_os_error().unwrap_or(5) } else { 0 };
        let bytes = if result < 0 { &[][..] } else { std::slice::from_raw_parts(mask.as_ptr().cast::<u8>(), size) };
        let payload = posix_bytes(bytes);
        posix_tuple(&[object::small(i64::from(error)), payload])
    }
}

unsafe extern "C" fn posix_sched_setaffinity(_: Value, args: Value) -> Value {
    unsafe {
        let Some([pid, mask]) = operands::<2>(args) else { return null_mut() };
        let Some(pid) = posix_int(pid).and_then(|pid| i32::try_from(pid).ok())
            else { return fail("OverflowError", "process ID does not fit pid_t") };
        if mask.is_null() || object::is_fixnum(mask) { return fail("TypeError", "CPU mask must be bytes") }
        let Data::Bytes(mask, _, _) = &(*mask).data else { return fail("TypeError", "CPU mask must be bytes") };
        let result = sched_setaffinity(pid, mask.len(), mask.as_ptr().cast());
        let error = if result < 0 { std::io::Error::last_os_error().raw_os_error().unwrap_or(5) } else { 0 };
        object::small(i64::from(error))
    }
}

#[repr(C)]
struct UtsName {
    sysname: [c_char; 65],
    nodename: [c_char; 65],
    release: [c_char; 65],
    version: [c_char; 65],
    machine: [c_char; 65],
    domainname: [c_char; 65],
}

unsafe extern "C" {
    fn access(path: *const c_char, mode: i32) -> i32;
    fn open(path: *const c_char, flags: i32, mode: i32) -> i32;
    fn read(fd: i32, buffer: *mut std::ffi::c_void, count: usize) -> isize;
    fn close(fd: i32) -> i32;
    fn lseek(fd: i32, offset: i64, whence: i32) -> i64;
    fn fsync(fd: i32) -> i32;
    fn ftruncate(fd: i32, length: i64) -> i32;
    fn fchmod(fd: i32, mode: u32) -> i32;
    fn fchmodat(directory: i32, path: *const c_char, mode: u32, flags: i32) -> i32;
    fn utimensat(directory: i32, path: *const c_char, times: *const Timespec, flags: i32) -> i32;
    fn futimens(fd: i32, times: *const Timespec) -> i32;
    fn fcntl(fd: i32, command: i32, ...) -> i32;
    fn flock(fd: i32, operation: i32) -> i32;
    fn strerror(code: i32) -> *const c_char;
    fn getrlimit(resource: i32, limits: *mut ResourceLimits) -> i32;
    fn setrlimit(resource: i32, limits: *const ResourceLimits) -> i32;
    fn prlimit(pid: i32, resource: i32, proposed: *const ResourceLimits, previous: *mut ResourceLimits) -> i32;
    fn getrusage(who: i32, usage: *mut ResourceUsage) -> i32;
    fn getpagesize() -> i32;
    fn dup(fd: i32) -> i32;
    fn dup2(fd: i32, fd2: i32) -> i32;
    fn pipe(fds: *mut i32) -> i32;
    fn pipe2(fds: *mut i32, flags: i32) -> i32;
    fn getppid() -> i32;
    fn getuid() -> u32;
    fn geteuid() -> u32;
    fn getgid() -> u32;
    fn getegid() -> u32;
    fn umask(mask: u32) -> u32;
    fn kill(pid: i32, signal: i32) -> i32;
    fn sysconf(name: i32) -> i64;
    fn sched_getaffinity(pid: i32, size: usize, mask: *mut std::ffi::c_void) -> i32;
    fn sched_setaffinity(pid: i32, size: usize, mask: *const std::ffi::c_void) -> i32;
    fn uname(info: *mut UtsName) -> i32;
}

/// Populate the `_posix` native module namespace.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_posix_module_init(globals: Value) -> Value {
    unsafe {
        if object::has_error() {
            return null_mut();
        }
        use std::os::unix::ffi::OsStrExt;
        let _ = globals;
        let exports: [(&str, Native); 51] = [
            ("sched_getaffinity_mask", posix_sched_getaffinity),
            ("sched_setaffinity_mask", posix_sched_setaffinity),
            ("getrlimit", posix_getrlimit),
            ("setrlimit", posix_setrlimit),
            ("prlimit", posix_prlimit),
            ("getrusage", posix_getrusage),
            ("getpagesize", posix_getpagesize),
            ("strerror_bytes", posix_strerror),
            ("flock", posix_flock),
            ("fcntl_int", posix_fcntl_int),
            ("fcntl_buffer", posix_fcntl_buffer),
            ("ftruncate", posix_ftruncate),
            ("terminal_size", posix_terminal_size),
            ("cwd", posix_cwd),
            ("chdir", posix_chdir),
            ("chmod_at", posix_chmod_at),
            ("utime_at", posix_utime_at),
            ("fchmod", posix_fchmod),
            ("listdir", posix_listdir),
            ("stat", posix_stat),
            ("fstat", posix_fstat),
            ("mkdir", posix_mkdir),
            ("rmdir", posix_rmdir),
            ("unlink", posix_unlink),
            ("rename", posix_rename),
            ("link", posix_link),
            ("symlink", posix_symlink),
            ("readlink", posix_readlink),
            ("executable_path_bytes", posix_executable_path),
            ("access", posix_access),
            ("open_fd", posix_open),
            ("read", posix_read),
            ("write", posix_write),
            ("close", posix_close),
            ("lseek", posix_lseek),
            ("fsync", posix_fsync),
            ("dup", posix_dup),
            ("dup2", posix_dup2),
            ("pipe", posix_pipe),
            ("environ_items", posix_environ),
            ("entropy_bytes", posix_entropy),
            ("setenv_bytes", posix_setenv),
            ("unsetenv_bytes", posix_unsetenv),
            ("uname_items", posix_uname),
            ("getpid", posix_getpid),
            ("getppid", posix_getppid),
            ("getuid", posix_getuid),
            ("geteuid", posix_geteuid),
            ("getgid", posix_getgid),
            ("getegid", posix_getegid),
            ("umask", posix_umask),
        ];
        for (name, entry) in exports {
            let function = object::function(null_mut(), entry, name);
            let bound = object::bind(globals, name, function);
            object::release(function);
            if !bound {
                return null_mut();
            }
        }
        let kill_function = object::function(null_mut(), posix_kill, "kill");
        let bound = object::bind(globals, "kill", kill_function);
        object::release(kill_function);
        if !bound { return null_mut(); }
        let sysconf_function = object::function(null_mut(), posix_sysconf, "sysconf_value");
        let bound = object::bind(globals, "sysconf_value", sysconf_function);
        object::release(sysconf_function);
        if !bound { return null_mut(); }
        if !crate::terminal::populate(globals) { return null_mut(); }
        object::none_value()
    }
}

/// Populate an ordinary compiler-owned module namespace; no separate cache.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_native_module_init(globals: Value) -> Value {
    unsafe {
        if object::has_error() {
            return null_mut();
        }
        let Some(arguments) = ARGUMENTS.get() else {
            return fail(
                "SystemError",
                "native process arguments are not initialized",
            );
        };
        let argc = numeric::integer_from_usize(arguments.count);
        let argv = object::new_value(Data::ForeignPointerVector {
            base: arguments.values,
            length: if arguments.count == 0 {
                0
            } else {
                arguments.count + 1
            },
        });
        let bound = object::bind(globals, "argc", argc) && object::bind(globals, "argv", argv);
        object::release(argc);
        object::release(argv);
        if !bound {
            return null_mut();
        }
        let exports: [(&str, Native); 16] = [
            ("read_pointer", read_pointer),
            ("read_c_string", read_c_string),
            ("byte_length", byte_length),
            ("read_byte", read_byte),
            ("read_u64_le", read_u64_le),
            ("byte_copy", byte_copy),
            ("text_from_codepoints", text_from_codepoints),
            ("bytes_from_values", bytes_from_values),
            ("call_with", crate::arguments::call_with),
            ("default_construct", crate::arguments::default_construct),
            ("write_fd", write_fd),
            ("flush_fd", flush_fd),
            ("isatty", isatty_fd),
            ("register_modules", crate::builtins::register_modules),
            ("active_exception", active_exception),
            ("current_frame", current_frame),
        ];
        for (name, entry) in exports {
            let function = object::function(null_mut(), entry, name);
            let bound = object::bind(globals, name, function);
            object::release(function);
            if !bound {
                return null_mut();
            }
        }
        if !crate::math::populate(globals)
            || !crate::classes::populate(globals)
            || !crate::hashing::populate(globals)
            || !crate::weakref::populate(globals)
            || !crate::imports::populate(globals)
            || !crate::signals::populate(globals)
            || !crate::spawn::populate(globals)
            || !crate::multiplex::populate(globals)
            || !crate::networking::populate(globals)
            || !crate::gc::populate(globals)
            || !crate::buffers::populate(globals)
            || !crate::faults::populate(globals)
            || !crate::coroutine::populate(globals)
        {
            return null_mut();
        }
        object::none_value()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    unsafe fn invoke(module: Value, name: &str, args: &[Value]) -> Value {
        unsafe { object::call(object::lookup(module, name), args) }
    }

    #[test]
    fn foreign_vector_preserves_bytes_empty_strings_and_null_sentinel() {
        unsafe {
            object::clear_error();
            // Leaked storage models native startup's process-lifetime ownership.
            let vector = Box::leak(Box::new([
                c"".as_ptr(),
                b"\xff\xed\xa0\x80\0".as_ptr().cast(),
                std::ptr::null(),
            ]));
            mule_set_process_arguments(2, vector.as_ptr());
            let module = object::namespace();
            assert_eq!(mule_native_module_init(module), object::none_value());
            assert_eq!(object::fixnum(object::lookup(module, "argc")), 2);
            let vector = object::lookup(module, "argv");
            let empty = invoke(module, "read_pointer", &[vector, object::small(0)]);
            assert!(matches!(&(*empty).data, Data::ForeignCString { .. }));
            let bytes = invoke(module, "read_c_string", &[empty]);
            assert!(matches!(&(*bytes).data, Data::Bytes(bytes, ..) if bytes.is_empty()));
            assert_eq!(object::fixnum(invoke(module, "byte_length", &[bytes])), 0);
            object::release(bytes);
            object::release(empty);
            let pointer = invoke(module, "read_pointer", &[vector, object::small(1)]);
            let bytes = invoke(module, "read_c_string", &[pointer]);
            assert!(matches!(&(*bytes).data, Data::Bytes(bytes, ..) if bytes == b"\xff\xed\xa0\x80"));
            assert_eq!(
                object::fixnum(invoke(module, "read_byte", &[bytes, object::small(0)])),
                255
            );
            assert_eq!(
                invoke(module, "read_pointer", &[vector, object::small(2)]),
                object::none_value()
            );
            for index in [-1, 3] {
                assert!(invoke(module, "read_pointer", &[vector, object::small(index)]).is_null());
                assert_eq!(object::error_kind(), "IndexError");
                object::clear_error();
            }
            for index in [-1, 4] {
                assert!(invoke(module, "read_byte", &[bytes, object::small(index)]).is_null());
                assert_eq!(object::error_kind(), "IndexError");
                object::clear_error();
            }
            object::release(bytes);
            object::release(pointer);
            object::release(module);
            ARGUMENTS.set(None);
        }
    }

    #[test]
    fn native_exports_guard_types_arity_and_remain_mutable() {
        unsafe {
            object::clear_error();
            mule_set_process_arguments(0, std::ptr::null());
            let module = object::namespace();
            assert_eq!(mule_native_module_init(module), object::none_value());
            let vector = object::lookup(module, "argv");
            assert!(invoke(module, "read_pointer", &[vector, object::small(0)]).is_null());
            assert_eq!(object::error_kind(), "IndexError");
            object::clear_error();
            let text = object::new_value(Data::Text(Text::empty(), std::ptr::null_mut(), std::ptr::null_mut()));
            for (name, args) in [
                ("byte_length", vec![text]),
                ("read_byte", vec![text, object::small(0)]),
                ("read_pointer", vec![object::small(0), object::small(0)]),
                ("read_c_string", vec![object::small(0)]),
                ("read_c_string", vec![object::none_value()]),
                ("byte_length", vec![]),
                ("text_from_codepoints", vec![text]),
                ("read_pointer", vec![vector, text]),
            ] {
                assert!(invoke(module, name, &args).is_null());
                assert_eq!(object::error_kind(), "TypeError", "{name}");
                object::clear_error();
            }
            assert!(object::bind(module, "byte_length", object::small(42)));
            assert_eq!(object::fixnum(object::lookup(module, "byte_length")), 42);
            object::release(text);
            object::release(module);
            ARGUMENTS.set(None);
        }
    }

    #[test]
    fn text_storage_encodes_list_and_tuple_codepoints_including_surrogates() {
        unsafe {
            object::clear_error();
            mule_set_process_arguments(0, std::ptr::null());
            let module = object::namespace();
            mule_native_module_init(module);
            let codes = [
                0, 0x7f, 0x80, 0x7ff, 0x800, 0xd800, 0xdfff, 0xffff, 0x10000, 0x10ffff,
            ];
            for tuple in [false, true] {
                let values = codes.map(object::small).to_vec();
                let sequence = object::new_value(if tuple {
                    Data::Args(values, ..)
                } else {
                    Data::List(values, ..)
                });
                let result = invoke(module, "text_from_codepoints", &[sequence]);
                let Data::Text(text, ..) = &(*result).data else {
                    panic!("expected text")
                };
                assert_eq!(text.kind(), 4);
                assert_eq!(
                    text.codepoints().collect::<Vec<_>>(),
                    codes.map(|code| code as u32)
                );
                object::release(result);
                object::release(sequence);
            }
            for code in [-1, 0x110000] {
                let sequence = object::new_value(Data::List(vec![object::small(code)], std::ptr::null_mut(), std::ptr::null_mut()));
                assert!(invoke(module, "text_from_codepoints", &[sequence]).is_null());
                assert_eq!(object::error_kind(), "ValueError");
                object::clear_error();
                object::release(sequence);
            }
            let sequence =
                object::new_value(Data::List(vec![object::new_value(Data::Text(Text::empty(), std::ptr::null_mut(), std::ptr::null_mut()))], std::ptr::null_mut(), std::ptr::null_mut()));
            assert!(invoke(module, "text_from_codepoints", &[sequence]).is_null());
            assert_eq!(object::error_kind(), "TypeError");
            object::clear_error();
            object::release(sequence);
            object::release(module);
            ARGUMENTS.set(None);
        }
    }

    #[test]
    fn initialization_requires_published_valid_vector_and_preserves_sticky_errors() {
        unsafe {
            object::clear_error();
            ARGUMENTS.set(None);
            let module = object::namespace();
            assert!(mule_native_module_init(module).is_null());
            assert_eq!(object::error_kind(), "SystemError");
            object::clear_error();
            mule_set_process_arguments(1, std::ptr::null());
            assert_eq!(object::error_kind(), "SystemError");
            object::clear_error();
            let vector = [c"x".as_ptr(), c"not-null".as_ptr()];
            mule_set_process_arguments(1, vector.as_ptr());
            assert_eq!(object::error_kind(), "SystemError");
            object::clear_error();
            mule_set_process_arguments(0, std::ptr::null());
            object::error("ValueError", "existing failure");
            assert!(mule_native_module_init(module).is_null());
            assert_eq!(object::error_kind(), "ValueError");
            object::clear_error();
            object::release(module);
            ARGUMENTS.set(None);
        }
    }
}
