//! Linux terminal records and descriptor syscalls; policy is compiled Python.

use crate::object::{self, Data, Native, Value};
use std::ptr::{null, null_mut};

#[repr(C)]
#[derive(Default)]
struct Termios {
    input: u32, output: u32, control: u32, local: u32,
    line: u8, characters: [u8; 32], input_speed: u32, output_speed: u32,
}

const _: () = assert!(std::mem::size_of::<Termios>() == 60);

unsafe extern "C" {
    fn tcgetattr(fd: i32, attributes: *mut Termios) -> i32;
    fn tcsetattr(fd: i32, action: i32, attributes: *const Termios) -> i32;
    fn cfgetispeed(attributes: *const Termios) -> u32;
    fn cfgetospeed(attributes: *const Termios) -> u32;
    fn cfsetispeed(attributes: *mut Termios, speed: u32) -> i32;
    fn cfsetospeed(attributes: *mut Termios, speed: u32) -> i32;
    fn tcdrain(fd: i32) -> i32;
    fn tcflush(fd: i32, queue: i32) -> i32;
    fn tcflow(fd: i32, action: i32) -> i32;
    fn tcsendbreak(fd: i32, duration: i32) -> i32;
    fn ioctl(fd: i32, request: u64, ...) -> i32;
    fn openpty(master: *mut i32, slave: *mut i32, name: *mut u8,
               attributes: *const Termios, dimensions: *const u8) -> i32;
    fn fcntl(fd: i32, command: i32, ...) -> i32;
    fn close(fd: i32) -> i32;
}

unsafe fn operands<const N: usize>(args: Value) -> Option<[Value; N]> {
    unsafe {
        if object::has_error() { return None; }
        if object::argument_count(args) != Some(N) || crate::arguments::keyword_count(args) != 0 {
            object::error("TypeError", "invalid terminal service arguments"); return None;
        }
        let mut values = [null_mut(); N];
        for (index, value) in values.iter_mut().enumerate() { *value = object::argument(args, index)?; }
        Some(values)
    }
}

unsafe fn integer(value: Value) -> Option<i64> {
    unsafe {
        let value = crate::numeric::integer_export(value)?.to_i64();
        if value.is_none() { object::error("OverflowError", "native terminal integer overflow"); }
        value
    }
}
unsafe fn bytes(value: Value) -> Option<&'static [u8]> {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::Bytes(bytes, ..) = &(*value).data { return Some(bytes); }
        }
        object::error("TypeError", "native terminal service requires bytes"); None
    }
}
unsafe fn tuple(values: &[Value]) -> Value {
    unsafe {
        let result = object::mule_sequence_tuple(values.len() as isize, values.as_ptr());
        for value in values { object::release(*value); }
        result
    }
}
unsafe fn byte_storage(bytes: Vec<u8>) -> Value {
    unsafe { object::new_value(Data::Bytes(bytes, null_mut(), null_mut())) }
}
unsafe fn result(error: i32, value: Value) -> Value {
    unsafe { tuple(&[object::small(i64::from(error)), value]) }
}
fn errno() -> i32 { std::io::Error::last_os_error().raw_os_error().unwrap_or(5) }
unsafe fn status(value: i32) -> Value {
    unsafe { result(if value < 0 { errno() } else { 0 }, object::none_value()) }
}

unsafe extern "C" fn terminal_getattr(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd]) = operands(args) else { return null_mut(); };
        let Some(fd) = integer(fd) else { return null_mut(); };
        let mut attributes = Termios::default();
        if tcgetattr(fd as i32, &mut attributes) < 0 { return status(-1); }
        let values = [attributes.input, attributes.output, attributes.control, attributes.local,
                      cfgetispeed(&attributes), cfgetospeed(&attributes)];
        let mut fields: Vec<Value> = values.iter().map(|value| object::small(i64::from(*value))).collect();
        fields.push(byte_storage(attributes.characters.to_vec()));
        result(0, tuple(&fields))
    }
}

unsafe extern "C" fn terminal_setattr(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd, action, input, output, control, local, input_speed, output_speed, characters]) = operands(args) else { return null_mut(); };
        let mut fields = [0i64; 8];
        for (index, value) in [fd, action, input, output, control, local, input_speed, output_speed].iter().enumerate() {
            let Some(value) = integer(*value) else { return null_mut(); }; fields[index] = value;
        }
        let Some(characters) = bytes(characters) else { return null_mut(); };
        if characters.len() != 32 { object::error("ValueError", "native termios requires 32 control characters"); return null_mut(); }
        let mut attributes = Termios::default();
        if tcgetattr(fields[0] as i32, &mut attributes) < 0 { return status(-1); }
        attributes.input = fields[2] as u32; attributes.output = fields[3] as u32;
        attributes.control = fields[4] as u32; attributes.local = fields[5] as u32;
        attributes.characters.copy_from_slice(characters);
        if cfsetispeed(&mut attributes, fields[6] as u32) < 0 || cfsetospeed(&mut attributes, fields[7] as u32) < 0 { return status(-1); }
        status(tcsetattr(fields[0] as i32, fields[1] as i32, &attributes))
    }
}

unsafe extern "C" fn terminal_drain(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd]) = operands(args) else { return null_mut(); };
        let Some(fd) = integer(fd) else { return null_mut(); };
        status(tcdrain(fd as i32))
    }
}
macro_rules! action {
    ($name:ident, $function:ident) => {
        unsafe extern "C" fn $name(_: Value, args: Value) -> Value {
            unsafe {
                let Some([fd, argument]) = operands(args) else { return null_mut(); };
                let (Some(fd), Some(argument)) = (integer(fd), integer(argument)) else { return null_mut(); };
                status($function(fd as i32, argument as i32))
            }
        }
    };
}
action!(terminal_flush, tcflush);
action!(terminal_flow, tcflow);
action!(terminal_break, tcsendbreak);

unsafe extern "C" fn ioctl_integer(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd, request, argument]) = operands(args) else { return null_mut(); };
        let (Some(fd), Some(request), Some(argument)) = (integer(fd), integer(request), integer(argument)) else { return null_mut(); };
        let returned = ioctl(fd as i32, request as u64, argument as i32);
        let error = if returned < 0 { errno() } else { 0 };
        result(error, object::small(i64::from(returned)))
    }
}

unsafe extern "C" fn ioctl_buffer(_: Value, args: Value) -> Value {
    unsafe {
        let Some([fd, request, buffer]) = operands(args) else { return null_mut(); };
        let (Some(fd), Some(request)) = (integer(fd), integer(request)) else { return null_mut(); };
        let Some(input) = bytes(buffer) else { return null_mut(); };
        let size = input.len().max(1024);
        let mut scratch = Vec::new();
        if scratch.try_reserve_exact(size).is_err() { object::error("MemoryError", "ioctl buffer allocation failed"); return null_mut(); }
        scratch.resize(size, 0);
        scratch[..input.len()].copy_from_slice(input);
        let returned = ioctl(fd as i32, request as u64, scratch.as_mut_ptr());
        let error = if returned < 0 { errno() } else { 0 };
        scratch.truncate(input.len());
        result(error, tuple(&[object::small(i64::from(returned)), byte_storage(scratch)]))
    }
}

unsafe extern "C" fn terminal_openpty(_: Value, args: Value) -> Value {
    unsafe {
        if operands::<0>(args).is_none() { return null_mut(); }
        let (mut master, mut slave) = (-1, -1);
        if openpty(&mut master, &mut slave, null_mut(), null(), null()) < 0 { return status(-1); }
        if fcntl(master, 2, 1) < 0 || fcntl(slave, 2, 1) < 0 {
            let error = errno(); close(master); close(slave);
            return result(error, object::none_value());
        }
        result(0, tuple(&[object::small(i64::from(master)), object::small(i64::from(slave))]))
    }
}

pub unsafe fn populate(globals: Value) -> bool {
    unsafe {
        let exports: [(&str, Native); 9] = [
            ("terminal_getattr", terminal_getattr), ("terminal_setattr", terminal_setattr),
            ("terminal_drain", terminal_drain), ("terminal_flush", terminal_flush),
            ("terminal_flow", terminal_flow), ("terminal_break", terminal_break),
            ("ioctl_integer", ioctl_integer), ("ioctl_buffer", ioctl_buffer),
            ("openpty", terminal_openpty),
        ];
        for (name, function) in exports {
            let function = object::function(null_mut(), function, name);
            let bound = object::bind(globals, name, function);
            object::release(function);
            if !bound { return false; }
        }
        true
    }
}
