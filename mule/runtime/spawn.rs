//! POSIX fork/exec, descriptor flags and raw wait status. Argument policy is Python.

use crate::object::{self, Data, Value};
use crate::numeric;
use std::ffi::{c_char, CString};
use std::ptr::null_mut;

unsafe extern "C" {
    fn fork() -> i32;
    fn execve(path: *const c_char, argv: *const *const c_char, env: *const *const c_char) -> i32;
    static environ: *const *const c_char;
    fn close(fd: i32) -> i32;
    fn dup2(fd: i32, target: i32) -> i32;
    fn fcntl(fd: i32, operation: i32, ...) -> i32;
    fn chdir(path: *const c_char) -> i32;
    fn setsid() -> i32;
    fn setpgid(pid: i32, group: i32) -> i32;
    fn setgid(gid: u32) -> i32;
    fn setuid(uid: u32) -> i32;
    fn setgroups(count: usize, groups: *const u32) -> i32;
    fn umask(mask: u32) -> u32;
    fn signal(number: i32, handler: usize) -> usize;
    fn waitpid(pid: i32, status: *mut i32, options: i32) -> i32;
    fn write(fd: i32, bytes: *const u8, count: usize) -> isize;
    fn close_range(first: u32, last: u32, flags: i32) -> i32;
    fn sysconf(name: i32) -> i64;
    fn __errno_location() -> *mut i32;
    fn _exit(status: i32) -> !;
}

fn fail(kind: &str, message: &str) -> Value {
    object::error(kind, message); null_mut()
}

fn os_error() -> Value {
    let error = std::io::Error::last_os_error();
    fail(if error.raw_os_error() == Some(10) { "ChildProcessError" } else { "OSError" }, &error.to_string())
}

unsafe fn operands<const N: usize>(args: Value) -> Option<[Value; N]> {
    unsafe {
        if object::has_error() { return None; }
        if object::argument_count(args)? != N || crate::arguments::keyword_count(args) != 0 {
            fail("TypeError", "invalid process primitive argument count"); return None;
        }
        let mut values = [null_mut(); N];
        for (index, value) in values.iter_mut().enumerate() { *value = object::argument(args, index)?; }
        Some(values)
    }
}

unsafe fn integer(value: Value) -> Option<i64> {
    unsafe {
        let result = numeric::integer_export(value)?.to_i64();
        if result.is_none() { fail("OverflowError", "process integer exceeds native range"); }
        result
    }
}

unsafe fn c_string(value: Value) -> Option<CString> {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::Bytes(bytes, ..) = &(*value).data {
                return match CString::new(bytes.as_slice()) {
                    Ok(value) => Some(value),
                    Err(_) => { fail("ValueError", "embedded null byte"); None }
                };
            }
        }
        fail("TypeError", "process path storage must be bytes"); None
    }
}

unsafe fn strings(values: Value) -> Option<Vec<CString>> {
    unsafe { (0..object::argument_count(values)?).map(|index| c_string(object::argument(values, index)?)).collect() }
}

unsafe fn numbers(values: Value) -> Option<Vec<i64>> {
    unsafe { (0..object::argument_count(values)?).map(|index| integer(object::argument(values, index)?)).collect() }
}

fn pointers(values: &[CString]) -> Vec<*const c_char> {
    let mut pointers: Vec<_> = values.iter().map(|value| value.as_ptr()).collect();
    pointers.push(std::ptr::null()); pointers
}

// All buffers exist before fork. Child code below performs no allocation.
unsafe fn child_error(fd: i32, error: i32, stage: &[u8]) -> ! {
    unsafe {
        let mut buffer = [0u8; 96];
        buffer[..8].copy_from_slice(b"OSError:");
        let mut digits = [0u8; 8];
        let mut number = error as u32;
        let mut count = 0;
        loop {
            digits[count] = b"0123456789abcdef"[(number & 15) as usize];
            count += 1; number >>= 4;
            if number == 0 { break; }
        }
        for index in 0..count { buffer[8 + index] = digits[count - index - 1]; }
        let mut length = 8 + count;
        buffer[length] = b':'; length += 1;
        buffer[length..length + stage.len()].copy_from_slice(stage); length += stage.len();
        let mut offset = 0;
        while offset < length {
            let result = write(fd, buffer.as_ptr().add(offset), length - offset);
            if result > 0 { offset += result as usize; }
            else if result < 0 && *__errno_location() == 4 { continue; }
            else { break; }
        }
        _exit(255)
    }
}

unsafe fn set_inheritance(fd: i32, inheritable: bool) -> i32 {
    unsafe {
        let flags = fcntl(fd, 1);
        if flags < 0 { return -1; }
        fcntl(fd, 2, if inheritable { flags & !1 } else { flags | 1 })
    }
}

unsafe fn close_interval(first: u32, last: u32, maximum: u32) {
    unsafe {
        if first > last { return; }
        if close_range(first, last, 0) == 0 { return; }
        for fd in first..=last.min(maximum) { close(fd as i32); }
    }
}

unsafe extern "C" fn fork_exec(_: Value, args: Value) -> Value {
    unsafe {
        let Some([argv, candidates, close_fds, keep, cwd, env, fds, restore, session, group, gid, groups, uid, mask]) = operands(args) else { return null_mut(); };
        let Some(argv_strings) = strings(argv) else { return null_mut(); };
        let Some(candidates) = strings(candidates) else { return null_mut(); };
        let argv = pointers(&argv_strings);
        let env_strings = if env == object::none_value() { None } else {
            let Some(values) = strings(env) else { return null_mut(); }; Some(values)
        };
        let env_pointers = env_strings.as_ref().map(|values| pointers(values));
        let environment = env_pointers.as_ref().map_or(environ, |values| values.as_ptr());
        let cwd = if cwd == object::none_value() { None } else {
            let Some(value) = c_string(cwd) else { return null_mut(); }; Some(value)
        };
        let Some(keep) = numbers(keep) else { return null_mut(); };
        let Some(descriptors) = numbers(fds) else { return null_mut(); };
        let Ok(descriptors): Result<[i64;8], _> = descriptors.try_into() else { return fail("ValueError", "eight process descriptors are required"); };
        let [p2cread, p2cwrite, c2pread, c2pwrite, errread, errwrite, errpipe_read, errpipe_write] = descriptors.map(|fd| fd as i32);
        let (Some(group), Some(mask)) = (integer(group), integer(mask)) else { return null_mut(); };
        let optional_id = |value| if value == object::none_value() { Some(None) } else { integer(value).map(|value| Some(value as u32)) };
        let (Some(gid), Some(uid)) = (optional_id(gid), optional_id(uid)) else { return null_mut(); };
        let groups = if groups == object::none_value() { None } else {
            let Some(values) = numbers(groups) else { return null_mut(); }; Some(values.into_iter().map(|value| value as u32).collect::<Vec<_>>())
        };
        let Some(close_fds) = object::truth(close_fds) else { return null_mut(); };
        let Some(restore) = object::truth(restore) else { return null_mut(); };
        let Some(session) = object::truth(session) else { return null_mut(); };
        let maximum = sysconf(4).clamp(3, i64::from(i32::MAX)) as u32;
        let pid = fork();
        if pid < 0 { return os_error(); }
        if pid > 0 { return object::small(i64::from(pid)); }
        let check = |result: i32, stage: &[u8]| { if result < 0 { child_error(errpipe_write, *__errno_location(), stage); } };
        check(set_inheritance(errpipe_write, false), b"noexec");
        let mut sources = [p2cread, c2pwrite, errwrite];
        for index in 0..3 {
            let source = sources[index];
            if source >= 0 && source < 3 && source != index as i32 {
                let duplicate = fcntl(source, 1030, 3);
                check(duplicate, b"noexec");
                for value in &mut sources { if *value == source { *value = duplicate; } }
            }
        }
        for fd in [p2cwrite, c2pread, errread, errpipe_read] { if fd >= 0 { close(fd); } }
        for (target, source) in sources.into_iter().enumerate() {
            if source >= 0 {
                check(dup2(source, target as i32), b"noexec");
                check(set_inheritance(target as i32, true), b"noexec");
            }
        }
        for fd in &keep { if *fd as i32 != errpipe_write { check(set_inheritance(*fd as i32, true), b"noexec"); } }
        if close_fds {
            let mut first = 3u32;
            for fd in &keep {
                let fd = *fd as u32;
                if fd < 3 { continue; }
                close_interval(first, fd.saturating_sub(1), maximum);
                first = fd.saturating_add(1);
            }
            close_interval(first, u32::MAX, maximum);
        }
        if let Some(cwd) = cwd.as_ref() { check(chdir(cwd.as_ptr()), b"noexec:chdir"); }
        if restore { signal(13, 0); signal(25, 0); }
        if session { check(setsid(), b"noexec"); }
        if group >= 0 { check(setpgid(0, group as i32), b"noexec"); }
        if let Some(groups) = groups.as_ref() { check(setgroups(groups.len(), groups.as_ptr()), b"noexec"); }
        if let Some(gid) = gid { check(setgid(gid), b"noexec"); }
        if let Some(uid) = uid { check(setuid(uid), b"noexec"); }
        if mask >= 0 { umask(mask as u32); }
        let mut saved_error = 2;
        let mut selected = false;
        for path in &candidates {
            execve(path.as_ptr(), argv.as_ptr(), environment);
            let error = *__errno_location();
            if !selected || saved_error == 2 || saved_error == 20 { saved_error = error; selected = true; }
        }
        child_error(errpipe_write, saved_error, b"")
    }
}

unsafe extern "C" fn wait(_: Value, args: Value) -> Value {
    unsafe {
        let Some([pid, options]) = operands(args) else { return null_mut(); };
        let (Some(pid), Some(options)) = (integer(pid), integer(options)) else { return null_mut(); };
        let mut status = 0;
        loop {
            let result = waitpid(pid as i32, &mut status, options as i32);
            if result < 0 {
                if *__errno_location() == 4 {
                    if !crate::signals::check_signals() { return null_mut(); }
                    continue;
                }
                return os_error();
            }
            let values = [object::small(i64::from(result)), object::small(i64::from(status))];
            return object::mule_sequence_tuple(2, values.as_ptr());
        }
    }
}

unsafe extern "C" fn inheritable(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd]) = operands(args) else { return null_mut(); };
        let Some(fd) = integer(fd) else { return null_mut(); };
        let flags = fcntl(fd as i32, 1);
        if flags < 0 { os_error() } else { object::bool_value(flags & 1 == 0) }
    }
}

unsafe extern "C" fn set_inheritable(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd, value]) = operands(args) else { return null_mut(); };
        let Some(fd) = integer(fd) else { return null_mut(); };
        let Some(value) = object::truth(value) else { return null_mut(); };
        if set_inheritance(fd as i32, value) < 0 { os_error() } else { object::none_value() }
    }
}

pub unsafe fn populate(globals: Value) -> bool {
    unsafe {
        let exports: [(&str, object::Native);4] = [
            ("fork_exec", fork_exec), ("waitpid", wait),
            ("get_inheritable", inheritable), ("set_inheritable", set_inheritable),
        ];
        for (name, entry) in exports {
            let function = object::function(null_mut(), entry, name);
            let bound = object::bind(globals, name, function);
            object::release(function);
            if !bound { return false; }
        }
        true
    }
}
