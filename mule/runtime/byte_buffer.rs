//! Stable compiler-visible ownership header for compact mutable byte storage.
use std::mem::ManuallyDrop;
use std::ptr::NonNull;

#[repr(C)]
pub struct ByteBuffer {
    pub data: *mut u8,
    pub length: usize,
    pub capacity: usize,
    pub exports: usize,
}

impl ByteBuffer {
    pub fn new() -> Self {
        Self {
            data: NonNull::dangling().as_ptr(),
            length: 0,
            capacity: 0,
            exports: 0,
        }
    }

    pub fn as_slice(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.data, self.length) }
    }

    pub fn as_mut_slice(&mut self) -> &mut [u8] {
        unsafe { std::slice::from_raw_parts_mut(self.data, self.length) }
    }

    pub fn reserve(&mut self, additional: usize) -> bool {
        let mut bytes = ManuallyDrop::new(unsafe {
            Vec::from_raw_parts(self.data, self.length, self.capacity)
        });
        let result = bytes.try_reserve(additional).is_ok();
        self.data = bytes.as_mut_ptr();
        self.capacity = bytes.capacity();
        result
    }

    pub fn push(&mut self, value: u8) -> bool {
        if self.length == self.capacity && !self.reserve(1) {
            return false;
        }
        unsafe {
            self.data.add(self.length).write(value);
        }
        self.length += 1;
        true
    }

    pub fn delete(&mut self, start: usize, stop: usize) {
        unsafe {
            std::ptr::copy(
                self.data.add(stop),
                self.data.add(start),
                self.length - stop,
            );
        }
        self.length -= stop - start;
    }
}

impl Drop for ByteBuffer {
    fn drop(&mut self) {
        unsafe {
            drop(Vec::from_raw_parts(self.data, self.length, self.capacity));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn byte_header_layout_and_storage() {
        assert_eq!(std::mem::offset_of!(ByteBuffer, data), 0);
        assert_eq!(std::mem::offset_of!(ByteBuffer, length), 8);
        assert_eq!(std::mem::offset_of!(ByteBuffer, capacity), 16);
        assert_eq!(std::mem::offset_of!(ByteBuffer, exports), 24);
        let mut bytes = ByteBuffer::new();
        assert!(bytes.push(0));
        assert!(bytes.push(255));
        assert_eq!(bytes.as_slice(), &[0, 255]);
        bytes.delete(0, 1);
        assert_eq!(bytes.as_slice(), &[255]);
    }

    #[test]
    fn object_header_matches_compiled_byte_access() {
        unsafe {
            let value = crate::sequences::mule_byte_storage_new();
            let base = value.cast::<u8>();
            assert_eq!(base.add(16).cast::<u32>().read(), 27);
            let crate::object::Data::ByteStorage(buffer) = &mut (*value).data else {
                panic!("byte storage");
            };
            assert_eq!(&buffer.data as *const _ as usize - value as usize, 24);
            assert_eq!(&buffer.length as *const _ as usize - value as usize, 32);
            assert_eq!(&buffer.capacity as *const _ as usize - value as usize, 40);
            assert_eq!(&buffer.exports as *const _ as usize - value as usize, 48);
            assert!(!crate::buffers::has_exports(value));
            buffer.exports = 1;
            assert!(crate::buffers::has_exports(value));
            crate::buffers::release(value);
            assert!(!crate::buffers::has_exports(value));
            crate::object::release(value);
        }
    }
}
