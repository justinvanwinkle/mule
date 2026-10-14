//! Libc socket calls and owned descriptor leases; Python owns address policy.

use crate::object::{self, Data, Value};
use crate::text::Text;
use std::ffi::{CStr, CString, c_char, c_void};
use std::fs::File;
use std::os::fd::{AsRawFd, FromRawFd, IntoRawFd};
use std::ptr::{null, null_mut};

#[repr(C)]
struct AddressInfo {
    flags: i32, family: i32, kind: i32, protocol: i32,
    length: u32, address: *mut c_void, canonical: *mut c_char,
    next: *mut AddressInfo,
}

unsafe extern "C" {
    #[link_name = "socket"] fn os_socket(family: i32, kind: i32, protocol: i32) -> i32;
    fn socketpair(family: i32, kind: i32, protocol: i32, descriptors: *mut i32) -> i32;
    fn bind(fd: i32, address: *const u8, length: u32) -> i32;
    fn connect(fd: i32, address: *const u8, length: u32) -> i32;
    fn listen(fd: i32, backlog: i32) -> i32;
    fn accept4(fd: i32, address: *mut u8, length: *mut u32, flags: i32) -> i32;
    fn shutdown(fd: i32, how: i32) -> i32;
    fn send(fd: i32, buffer: *const u8, count: usize, flags: i32) -> isize;
    fn recv(fd: i32, buffer: *mut u8, count: usize, flags: i32) -> isize;
    fn sendto(fd: i32, buffer: *const u8, count: usize, flags: i32, address: *const u8, length: u32) -> isize;
    fn recvfrom(fd: i32, buffer: *mut u8, count: usize, flags: i32, address: *mut u8, length: *mut u32) -> isize;
    fn getsockname(fd: i32, address: *mut u8, length: *mut u32) -> i32;
    fn getpeername(fd: i32, address: *mut u8, length: *mut u32) -> i32;
    fn getsockopt(fd: i32, level: i32, option: i32, value: *mut u8, length: *mut u32) -> i32;
    fn setsockopt(fd: i32, level: i32, option: i32, value: *const u8, length: u32) -> i32;
    fn fcntl(fd: i32, operation: i32, ...) -> i32;
    fn inet_pton(family: i32, text: *const c_char, output: *mut u8) -> i32;
    fn inet_ntop(family: i32, address: *const u8, text: *mut c_char, length: u32) -> *const c_char;
    fn gethostname(text: *mut c_char, length: usize) -> i32;
    fn getaddrinfo(host: *const c_char, service: *const c_char, hints: *const AddressInfo, result: *mut *mut AddressInfo) -> i32;
    fn freeaddrinfo(result: *mut AddressInfo);
    fn gai_strerror(code: i32) -> *const c_char;
}

unsafe fn operands<const N: usize>(args: Value) -> Option<[Value; N]> {
    unsafe {
        if object::has_error() { return None; }
        if object::argument_count(args) != Some(N) || crate::arguments::keyword_count(args) != 0 {
            object::error("TypeError", "invalid native socket arguments"); return None;
        }
        let mut values = [null_mut(); N];
        for (index, value) in values.iter_mut().enumerate() { *value = object::argument(args, index)?; }
        Some(values)
    }
}
unsafe fn integer(value: Value) -> Option<i32> {
    unsafe {
        let Some(number) = crate::numeric::integer_export(value)?.to_i64() else {
            object::error("OverflowError", "socket integer does not fit C int"); return None;
        };
        match i32::try_from(number) {
            Ok(number) => Some(number),
            Err(_) => { object::error("OverflowError", "socket integer does not fit C int"); None }
        }
    }
}
unsafe fn count(value: Value) -> Option<usize> {
    unsafe {
        let number = crate::numeric::integer_export(value)?.to_i64();
        match number.and_then(|number| usize::try_from(number).ok()) {
            Some(number) => Some(number),
            None => { object::error("OverflowError", "invalid socket buffer size"); None }
        }
    }
}
unsafe fn fd(value: Value) -> Option<i32> {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::DescriptorLease(file) = &(*value).data { return Some(file.as_ref().map_or(-1, File::as_raw_fd)); }
        }
        object::error("TypeError", "descriptor lease required"); None
    }
}
unsafe fn lease(descriptor: i32) -> Value {
    unsafe { object::new_value(Data::DescriptorLease(Some(File::from_raw_fd(descriptor)))) }
}
unsafe fn buffer(value: Value) -> Option<&'static [u8]> {
    unsafe {
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::Bytes(bytes, ..) = &(*value).data { return Some(bytes); }
        }
        object::error("TypeError", "native socket buffer requires bytes"); None
    }
}
unsafe fn text(value: Value) -> Option<CString> {
    unsafe {
        if value == object::none_value() { return None; }
        if !value.is_null() && !object::is_fixnum(value) {
            if let Data::Text(value, ..) = &(*value).data {
                if let Some(value) = value.to_rust_string() {
                    return match CString::new(value) {
                        Ok(value) => Some(value),
                        Err(_) => { object::error("ValueError", "embedded null character"); None }
                    };
                }
            }
        }
        object::error("TypeError", "native socket text required"); None
    }
}
unsafe fn bytes(value: Vec<u8>) -> Value {
    unsafe { object::new_value(Data::Bytes(value, null_mut(), null_mut())) }
}
unsafe fn tuple(values: &[Value]) -> Value {
    unsafe {
        let result = object::mule_sequence_tuple(values.len() as isize, values.as_ptr());
        for value in values { object::release(*value); }
        result
    }
}
unsafe fn result(error: i32, payload: Value) -> Value {
    unsafe { tuple(&[object::small(i64::from(error)), payload]) }
}
unsafe fn failure() -> Value {
    unsafe { result(std::io::Error::last_os_error().raw_os_error().unwrap_or(5), object::none_value()) }
}
unsafe fn status(value: i32) -> Value {
    unsafe { if value < 0 { failure() } else { result(0, object::none_value()) } }
}

unsafe extern "C" fn socket_new(_: Value, args: Value) -> Value {
    unsafe {
        let Some([family, kind, protocol]) = operands(args) else { return null_mut(); };
        let (Some(family), Some(kind), Some(protocol)) = (integer(family), integer(kind), integer(protocol)) else { return null_mut(); };
        let descriptor = os_socket(family, kind | 0x80000, protocol);
        if descriptor < 0 { failure() } else { result(0, lease(descriptor)) }
    }
}
unsafe extern "C" fn socket_pair(_: Value, args: Value) -> Value {
    unsafe {
        let Some([family, kind, protocol]) = operands(args) else { return null_mut(); };
        let (Some(family), Some(kind), Some(protocol)) = (integer(family), integer(kind), integer(protocol)) else { return null_mut(); };
        let mut descriptors = [-1; 2];
        if socketpair(family, kind | 0x80000, protocol, descriptors.as_mut_ptr()) < 0 { return failure(); }
        result(0, tuple(&[lease(descriptors[0]), lease(descriptors[1])]))
    }
}
unsafe extern "C" fn socket_adopt(_: Value, args: Value) -> Value {
    unsafe {
        let Some([descriptor]) = operands(args) else { return null_mut(); };
        let Some(descriptor) = integer(descriptor) else { return null_mut(); };
        if descriptor < 0 || fcntl(descriptor, 1) < 0 {
            object::error("OSError", "invalid socket descriptor"); return null_mut();
        }
        lease(descriptor)
    }
}
unsafe extern "C" fn socket_fd(_: Value, args: Value) -> Value {
    unsafe {
        let Some([handle]) = operands(args) else { return null_mut(); };
        fd(handle).map_or(null_mut(), |fd| object::small(i64::from(fd)))
    }
}
unsafe extern "C" fn socket_close(_: Value, args: Value) -> Value {
    unsafe {
        let Some([handle]) = operands(args) else { return null_mut(); };
        if fd(handle).is_none() { return null_mut(); }
        let Data::DescriptorLease(file) = &mut (*handle).data else { unreachable!(); };
        drop(file.take());
        object::none_value()
    }
}
unsafe extern "C" fn socket_detach(_: Value, args: Value) -> Value {
    unsafe {
        let Some([handle]) = operands(args) else { return null_mut(); };
        if fd(handle).is_none() { return null_mut(); }
        let Data::DescriptorLease(file) = &mut (*handle).data else { unreachable!(); };
        object::small(i64::from(file.take().map_or(-1, File::into_raw_fd)))
    }
}
unsafe extern "C" fn socket_dup(_: Value, args: Value) -> Value {
    unsafe {
        let Some([handle]) = operands(args) else { return null_mut(); };
        let Some(fd) = fd(handle) else { return null_mut(); };
        let descriptor = fcntl(fd, 1030, 0);
        if descriptor < 0 { failure() } else { result(0, lease(descriptor)) }
    }
}
unsafe fn address_call(args: Value, call: unsafe extern "C" fn(i32, *const u8, u32) -> i32) -> Value {
    unsafe {
        let Some([handle, address]) = operands(args) else { return null_mut(); };
        let (Some(fd), Some(address)) = (fd(handle), buffer(address)) else { return null_mut(); };
        status(call(fd, address.as_ptr(), address.len() as u32))
    }
}
unsafe extern "C" fn socket_bind(_: Value, args: Value) -> Value { unsafe { address_call(args, bind) } }
unsafe extern "C" fn socket_connect(_: Value, args: Value) -> Value { unsafe { address_call(args, connect) } }
unsafe fn integer_call(args: Value, call: unsafe extern "C" fn(i32, i32) -> i32) -> Value {
    unsafe {
        let Some([handle, number]) = operands(args) else { return null_mut(); };
        let (Some(fd), Some(number)) = (fd(handle), integer(number)) else { return null_mut(); };
        status(call(fd, number))
    }
}
unsafe extern "C" fn socket_listen(_: Value, args: Value) -> Value { unsafe { integer_call(args, listen) } }
unsafe extern "C" fn socket_shutdown(_: Value, args: Value) -> Value { unsafe { integer_call(args, shutdown) } }
unsafe fn address_result(args: Value, call: unsafe extern "C" fn(i32, *mut u8, *mut u32) -> i32) -> Value {
    unsafe {
        let Some([handle]) = operands(args) else { return null_mut(); };
        let Some(fd) = fd(handle) else { return null_mut(); };
        let mut address = vec![0; 256]; let mut length = address.len() as u32;
        if call(fd, address.as_mut_ptr(), &mut length) < 0 { return failure(); }
        address.truncate(length as usize);
        result(0, bytes(address))
    }
}
unsafe extern "C" fn socket_name(_: Value, args: Value) -> Value { unsafe { address_result(args, getsockname) } }
unsafe extern "C" fn socket_peer(_: Value, args: Value) -> Value { unsafe { address_result(args, getpeername) } }
unsafe extern "C" fn socket_accept(_: Value, args: Value) -> Value {
    unsafe {
        let Some([handle]) = operands(args) else { return null_mut(); };
        let Some(fd) = fd(handle) else { return null_mut(); };
        let mut address = vec![0; 256]; let mut length = address.len() as u32;
        let descriptor = accept4(fd, address.as_mut_ptr(), &mut length, 0x80000);
        if descriptor < 0 { return failure(); }
        address.truncate(length as usize);
        result(0, tuple(&[lease(descriptor), bytes(address)]))
    }
}
unsafe extern "C" fn socket_send(_: Value, args: Value) -> Value {
    unsafe {
        let Some([handle, data, flags]) = operands(args) else { return null_mut(); };
        let (Some(fd), Some(data), Some(flags)) = (fd(handle), buffer(data), integer(flags)) else { return null_mut(); };
        let count = send(fd, data.as_ptr(), data.len(), flags | 0x4000);
        if count < 0 { failure() } else { result(0, crate::numeric::integer_from_usize(count as usize)) }
    }
}
unsafe extern "C" fn socket_receive(_: Value, args: Value) -> Value {
    unsafe {
        let Some([handle, size, flags]) = operands(args) else { return null_mut(); };
        let (Some(fd), Some(size), Some(flags)) = (fd(handle), count(size), integer(flags)) else { return null_mut(); };
        let mut data = Vec::new();
        if data.try_reserve_exact(size).is_err() { object::error("MemoryError", "socket receive allocation failed"); return null_mut(); }
        data.resize(size, 0);
        let count = recv(fd, data.as_mut_ptr(), size, flags);
        if count < 0 { return failure(); }
        data.truncate(count as usize); result(0, bytes(data))
    }
}
unsafe extern "C" fn socket_send_to(_: Value, args: Value) -> Value {
    unsafe {
        let Some([handle, data, flags, address]) = operands(args) else { return null_mut(); };
        let (Some(fd), Some(data), Some(flags), Some(address)) = (fd(handle), buffer(data), integer(flags), buffer(address)) else { return null_mut(); };
        let count = sendto(fd, data.as_ptr(), data.len(), flags | 0x4000, address.as_ptr(), address.len() as u32);
        if count < 0 { failure() } else { result(0, crate::numeric::integer_from_usize(count as usize)) }
    }
}
unsafe extern "C" fn socket_receive_from(_: Value, args: Value) -> Value {
    unsafe {
        let Some([handle, size, flags]) = operands(args) else { return null_mut(); };
        let (Some(fd), Some(size), Some(flags)) = (fd(handle), count(size), integer(flags)) else { return null_mut(); };
        let mut data = Vec::new();
        if data.try_reserve_exact(size).is_err() { object::error("MemoryError", "socket receive allocation failed"); return null_mut(); }
        data.resize(size, 0);
        let mut address = vec![0; 256]; let mut length = address.len() as u32;
        let count = recvfrom(fd, data.as_mut_ptr(), size, flags, address.as_mut_ptr(), &mut length);
        if count < 0 { return failure(); }
        data.truncate(count as usize); address.truncate(length as usize);
        result(0, tuple(&[bytes(data), bytes(address)]))
    }
}
unsafe extern "C" fn socket_option(_: Value, args: Value) -> Value {
    unsafe {
        let Some([handle, level, option, length]) = operands(args) else { return null_mut(); };
        let (Some(fd), Some(level), Some(option), Some(length)) = (fd(handle), integer(level), integer(option), count(length)) else { return null_mut(); };
        let mut data = vec![0; length]; let mut actual = length as u32;
        if getsockopt(fd, level, option, data.as_mut_ptr(), &mut actual) < 0 { return failure(); }
        data.truncate(actual as usize); result(0, bytes(data))
    }
}
unsafe extern "C" fn socket_set_option(_: Value, args: Value) -> Value {
    unsafe {
        let Some([handle, level, option, data]) = operands(args) else { return null_mut(); };
        let (Some(fd), Some(level), Some(option), Some(data)) = (fd(handle), integer(level), integer(option), buffer(data)) else { return null_mut(); };
        status(setsockopt(fd, level, option, data.as_ptr(), data.len() as u32))
    }
}
unsafe extern "C" fn socket_nonblocking(_: Value, args: Value) -> Value {
    unsafe {
        let Some([handle, enabled]) = operands(args) else { return null_mut(); };
        let (Some(fd), Some(enabled)) = (fd(handle), object::truth(enabled)) else { return null_mut(); };
        let flags = fcntl(fd, 3);
        if flags < 0 { return failure(); }
        status(fcntl(fd, 4, if enabled { flags | 0x800 } else { flags & !0x800 }))
    }
}
unsafe extern "C" fn address_pack(_: Value, args: Value) -> Value {
    unsafe {
        let Some([family, address]) = operands(args) else { return null_mut(); };
        let (Some(family), Some(address)) = (integer(family), text(address)) else { return null_mut(); };
        let mut data = vec![0; if family == 2 { 4 } else { 16 }];
        let code = inet_pton(family, address.as_ptr(), data.as_mut_ptr());
        if code <= 0 { result(if code == 0 { 22 } else { std::io::Error::last_os_error().raw_os_error().unwrap_or(5) }, object::none_value()) }
        else { result(0, bytes(data)) }
    }
}
unsafe extern "C" fn address_text(_: Value, args: Value) -> Value {
    unsafe {
        let Some([family, address]) = operands(args) else { return null_mut(); };
        let (Some(family), Some(address)) = (integer(family), buffer(address)) else { return null_mut(); };
        if address.len() != (if family == 2 { 4 } else { 16 }) { object::error("ValueError", "invalid packed address length"); return null_mut(); }
        let mut output = [0i8; 128];
        if inet_ntop(family, address.as_ptr(), output.as_mut_ptr(), output.len() as u32).is_null() { return failure(); }
        let text = CStr::from_ptr(output.as_ptr()).to_string_lossy();
        result(0, object::new_value(Data::Text(Text::from_rust_str(&text), null_mut(), null_mut())))
    }
}
unsafe extern "C" fn host_name(_: Value, args: Value) -> Value {
    unsafe {
        let Some([]) = operands(args) else { return null_mut(); };
        let mut output = [0i8; 256];
        if gethostname(output.as_mut_ptr(), output.len()) < 0 { return failure(); }
        output[255] = 0;
        let text = CStr::from_ptr(output.as_ptr()).to_string_lossy();
        result(0, object::new_value(Data::Text(Text::from_rust_str(&text), null_mut(), null_mut())))
    }
}
unsafe extern "C" fn address_info(_: Value, args: Value) -> Value {
    unsafe {
        let Some([host, service, family, kind, protocol, flags]) = operands(args) else { return null_mut(); };
        let host = text(host); let service = text(service);
        if object::has_error() { return null_mut(); }
        let (Some(family), Some(kind), Some(protocol), Some(flags)) = (integer(family), integer(kind), integer(protocol), integer(flags)) else { return null_mut(); };
        let hints = AddressInfo { flags, family, kind, protocol, length: 0, address: null_mut(), canonical: null_mut(), next: null_mut() };
        let mut addresses = null_mut();
        let code = getaddrinfo(host.as_ref().map_or(null(), |host| host.as_ptr()), service.as_ref().map_or(null(), |service| service.as_ptr()), &hints, &mut addresses);
        if code != 0 {
            let message = CStr::from_ptr(gai_strerror(code)).to_string_lossy();
            return result(code, object::new_value(Data::Text(Text::from_rust_str(&message), null_mut(), null_mut())));
        }
        let mut rows = Vec::new(); let mut current = addresses;
        while !current.is_null() {
            let address = &*current;
            let canonical = if address.canonical.is_null() { "" } else { CStr::from_ptr(address.canonical).to_str().unwrap_or("") };
            let data = std::slice::from_raw_parts(address.address.cast::<u8>(), address.length as usize).to_vec();
            rows.push(tuple(&[
                object::small(i64::from(address.family)), object::small(i64::from(address.kind)),
                object::small(i64::from(address.protocol)),
                object::new_value(Data::Text(Text::from_rust_str(canonical), null_mut(), null_mut())),
                bytes(data),
            ]));
            current = address.next;
        }
        freeaddrinfo(addresses);
        result(0, tuple(&rows))
    }
}

pub unsafe fn populate(globals: Value) -> bool {
    unsafe {
        for (name, entry) in [
            ("socket_new", socket_new as object::Native), ("socket_pair", socket_pair),
            ("socket_adopt", socket_adopt), ("socket_fd", socket_fd), ("socket_close", socket_close),
            ("socket_detach", socket_detach), ("socket_dup", socket_dup),
            ("socket_bind", socket_bind), ("socket_connect", socket_connect), ("socket_listen", socket_listen),
            ("socket_shutdown", socket_shutdown), ("socket_name", socket_name), ("socket_peer", socket_peer),
            ("socket_accept", socket_accept), ("socket_send", socket_send), ("socket_receive", socket_receive),
            ("socket_send_to", socket_send_to), ("socket_receive_from", socket_receive_from),
            ("socket_option", socket_option), ("socket_set_option", socket_set_option),
            ("socket_nonblocking", socket_nonblocking), ("address_pack", address_pack),
            ("address_text", address_text), ("address_info", address_info), ("host_name", host_name),
        ] {
            let function = object::function(null_mut(), entry, name);
            let bound = object::bind(globals, name, function); object::release(function);
            if !bound { return false; }
        }
        true
    }
}
