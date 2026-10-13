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
        object::new_value(Data::Bytes(CStr::from_ptr(*address).to_bytes().to_vec()))
    }
}

unsafe fn bytes_storage<'a>(value: Value) -> Option<&'a [u8]> {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::Bytes(bytes) = &(*value).data {
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
        let (Data::List(values) | Data::Args(values)) = &(*value).data else {
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
            Ok(text) => object::new_value(Data::Text(text, std::ptr::null_mut())),
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
                Data::List(values) | Data::Args(values) => Some(values),
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
        object::new_value(Data::Bytes(bytes))
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
        let Data::Bytes(bytes) = &(*data).data else {
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
        if !object::is_fixnum(fd) {
            object::error("TypeError", "a file descriptor must be an integer");
            return null_mut();
        }
        object::bool_value(isatty(object::fixnum(fd) as i32) == 1)
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
            Data::Bytes(bytes) => Some(bytes.clone()),
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
    unsafe { object::new_value(Data::Text(Text::from_wtf8(bytes).unwrap_or_else(|_| Text::from_rust_str("")), std::ptr::null_mut())) }
}

unsafe fn posix_bytes(bytes: &[u8]) -> Value {
    unsafe { object::new_value(Data::Bytes(bytes.to_vec())) }
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

unsafe extern "C" fn posix_fstat(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd]) = operands::<1>(args) else { return null_mut() };
        let Some(fd) = posix_int(fd) else { return fail("TypeError", "fd must be an integer") };
        let file: std::mem::ManuallyDrop<std::fs::File> = std::mem::ManuallyDrop::new(unsafe { std::os::unix::io::FromRawFd::from_raw_fd(fd as i32) });
        match file.metadata() {
            Ok(metadata) => posix_stat_row(&metadata),
            Err(error) => posix_error(&error),
        }
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
        if unsafe { pipe(fds.as_mut_ptr()) } < 0 { return posix_error(&std::io::Error::last_os_error()); }
        posix_tuple(&[object::small(i64::from(fds[0])), object::small(i64::from(fds[1]))])
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
    fn dup(fd: i32) -> i32;
    fn dup2(fd: i32, fd2: i32) -> i32;
    fn pipe(fds: *mut i32) -> i32;
    fn getppid() -> i32;
    fn getuid() -> u32;
    fn geteuid() -> u32;
    fn getgid() -> u32;
    fn getegid() -> u32;
    fn umask(mask: u32) -> u32;
    fn kill(pid: i32, signal: i32) -> i32;
    fn sysconf(name: i32) -> i64;
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
        let exports: [(&str, Native); 33] = [
            ("cwd", posix_cwd),
            ("chdir", posix_chdir),
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
        let exports: [(&str, Native); 13] = [
            ("read_pointer", read_pointer),
            ("read_c_string", read_c_string),
            ("byte_length", byte_length),
            ("read_byte", read_byte),
            ("text_from_codepoints", text_from_codepoints),
            ("bytes_from_values", bytes_from_values),
            ("call_with", crate::arguments::call_with),
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
            assert!(matches!(&(*bytes).data, Data::Bytes(bytes) if bytes.is_empty()));
            assert_eq!(object::fixnum(invoke(module, "byte_length", &[bytes])), 0);
            object::release(bytes);
            object::release(empty);
            let pointer = invoke(module, "read_pointer", &[vector, object::small(1)]);
            let bytes = invoke(module, "read_c_string", &[pointer]);
            assert!(matches!(&(*bytes).data, Data::Bytes(bytes) if bytes == b"\xff\xed\xa0\x80"));
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
            let text = object::new_value(Data::Text(Text::empty(), std::ptr::null_mut()));
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
                    Data::Args(values)
                } else {
                    Data::List(values)
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
                let sequence = object::new_value(Data::List(vec![object::small(code)]));
                assert!(invoke(module, "text_from_codepoints", &[sequence]).is_null());
                assert_eq!(object::error_kind(), "ValueError");
                object::clear_error();
                object::release(sequence);
            }
            let sequence =
                object::new_value(Data::List(vec![object::new_value(Data::Text(Text::empty(), std::ptr::null_mut()))]));
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
