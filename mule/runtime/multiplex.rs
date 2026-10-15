//! Temporary libc descriptor buffers and monotonic interrupted-wait deadlines.

use crate::object::{self, Value};
use crate::numeric;
use std::ptr::null_mut;
use std::time::{Duration, Instant};

#[repr(C)]
#[derive(Clone, Copy)]
struct PollFd { fd: i32, events: i16, revents: i16 }

#[repr(C)]
#[derive(Clone, Copy)]
struct FdSet { words: [u64; 16] }

#[repr(C)]
struct Timeval { seconds: i64, microseconds: i64 }

unsafe extern "C" {
    fn poll(fds: *mut PollFd, count: usize, timeout: i32) -> i32;
    fn select(count: i32, read: *mut FdSet, write: *mut FdSet, error: *mut FdSet, timeout: *mut Timeval) -> i32;
}

unsafe fn operands<const N: usize>(args: Value) -> Option<[Value; N]> {
    unsafe {
        if object::has_error() { return None; }
        if object::argument_count(args)? != N || crate::arguments::keyword_count(args) != 0 {
            object::error("TypeError", "invalid descriptor primitive argument count");
            return None;
        }
        let mut values = [null_mut(); N];
        for (index, value) in values.iter_mut().enumerate() { *value = object::argument(args, index)?; }
        Some(values)
    }
}

unsafe fn integer(value: Value) -> Option<i64> {
    unsafe {
        let number = numeric::integer_export(value)?.to_i64();
        if number.is_none() { object::error("OverflowError", "descriptor primitive integer exceeds native range"); }
        number
    }
}

fn interrupted() -> bool {
    let error = std::io::Error::last_os_error();
    if error.raw_os_error() == Some(4) { unsafe { crate::signals::check_signals() } }
    else { object::error("OSError", &error.to_string()); false }
}

fn remaining(start: Instant, timeout: Duration) -> Duration {
    timeout.saturating_sub(start.elapsed())
}

unsafe extern "C" fn wait_poll(_: Value, args: Value) -> Value {
    unsafe {
        let Some([rows, timeout]) = operands(args) else { return null_mut(); };
        let Some(timeout) = integer(timeout) else { return null_mut(); };
        let Some(count) = object::argument_count(rows) else { return null_mut(); };
        let mut fds = Vec::with_capacity(count);
        for index in 0..count {
            let Some(row) = object::argument(rows, index) else { return null_mut(); };
            let Some([fd, events]) = operands(row) else { return null_mut(); };
            let (Some(fd), Some(events)) = (integer(fd), integer(events)) else { return null_mut(); };
            let (Ok(fd), Ok(events)) = (i32::try_from(fd), u16::try_from(events)) else {
                object::error("OverflowError", "pollfd storage exceeds native range"); return null_mut();
            };
            fds.push(PollFd { fd, events: events as i16, revents: 0 });
        }
        let start = Instant::now();
        let duration = Duration::from_millis(timeout.max(0) as u64);
        loop {
            let delay = if timeout < 0 { -1 } else {
                let left = remaining(start, duration);
                left.as_millis().saturating_add(u128::from(left.subsec_nanos() % 1_000_000 != 0)).min(i32::MAX as u128) as i32
            };
            if poll(fds.as_mut_ptr(), fds.len(), delay) >= 0 { break; }
            if !interrupted() { return null_mut(); }
        }
        let mut rows = Vec::new();
        for fd in fds {
            if fd.revents != 0 {
                let pair = [object::small(i64::from(fd.fd)), object::small(i64::from(fd.revents as u16))];
                rows.push(object::mule_sequence_tuple(2, pair.as_ptr()));
            }
        }
        let result = object::mule_sequence_tuple(rows.len() as isize, rows.as_ptr());
        for row in rows { object::release(row); }
        result
    }
}

unsafe fn fd_set(values: Value) -> Option<(FdSet, i32)> {
    unsafe {
        let mut set = FdSet { words: [0; 16] };
        let mut maximum = 0;
        for index in 0..object::argument_count(values)? {
            let fd = integer(object::argument(values, index)?)?;
            if !(0..1024).contains(&fd) { object::error("ValueError", "filedescriptor out of range in select()"); return None; }
            set.words[fd as usize / 64] |= 1u64 << (fd % 64);
            maximum = maximum.max(fd as i32 + 1);
        }
        Some((set, maximum))
    }
}

unsafe fn ready_set(set: FdSet) -> Value {
    unsafe {
        let values: Vec<Value> = (0..1024).filter(|fd| set.words[fd / 64] & (1u64 << (fd % 64)) != 0).map(|fd| object::small(fd as i64)).collect();
        object::mule_sequence_tuple(values.len() as isize, values.as_ptr())
    }
}

unsafe extern "C" fn wait_select(_: Value, args: Value) -> Value {
    unsafe {
        let Some([read, write, error, timeout]) = operands(args) else { return null_mut(); };
        let (Some((read, nr)), Some((write, nw)), Some((error, ne)), Some(timeout)) =
            (fd_set(read), fd_set(write), fd_set(error), integer(timeout)) else { return null_mut(); };
        let start = Instant::now();
        let duration = Duration::from_micros(timeout.max(0) as u64);
        loop {
            let (mut r, mut w, mut e) = (read, write, error);
            let left = remaining(start, duration);
            let mut time = Timeval { seconds: left.as_secs() as i64, microseconds: i64::from(left.subsec_micros()) };
            let pointer = if timeout < 0 { null_mut() } else { &mut time };
            if select(nr.max(nw).max(ne), &mut r, &mut w, &mut e, pointer) >= 0 {
                let rows = [ready_set(r), ready_set(w), ready_set(e)];
                let result = object::mule_sequence_tuple(3, rows.as_ptr());
                for row in rows { object::release(row); }
                return result;
            }
            if !interrupted() { return null_mut(); }
        }
    }
}

pub unsafe fn populate(globals: Value) -> bool {
    unsafe {
        let exports: [(&str, object::Native); 2] = [("descriptor_poll", wait_poll), ("descriptor_select", wait_select)];
        for (name, entry) in exports {
            let function = object::function(null_mut(), entry, name);
            let bound = object::bind(globals, name, function);
            object::release(function);
            if !bound { return false; }
        }
        true
    }
}
