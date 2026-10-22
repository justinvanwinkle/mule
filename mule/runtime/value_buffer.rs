//! Compiler-visible object-element storage, corresponding to SBCL's vector
//! length/data slots (`src/compiler/generic/objdef.lisp`). This header owns
//! the allocation. Rust's unspecified `Vec` layout never crosses the ABI.

use crate::object::Value;
use std::collections::TryReserveError;
use std::mem::ManuallyDrop;
use std::ops::{Deref, DerefMut, Range};
use std::ptr::NonNull;

#[repr(C)]
pub struct ValueBuffer {
    data: *mut Value,
    length: usize,
    capacity: usize,
}

impl ValueBuffer {
    pub const fn new() -> Self {
        Self {
            data: NonNull::dangling().as_ptr(),
            length: 0,
            capacity: 0,
        }
    }

    pub fn as_slice(&self) -> &[Value] {
        unsafe { std::slice::from_raw_parts(self.data, self.length) }
    }

    pub fn capacity(&self) -> usize {
        self.capacity
    }

    /// The caller initialized every element below `length`, or transferred
    /// ownership of elements removed from the initialized range.
    pub unsafe fn set_len(&mut self, length: usize) {
        assert!(length <= self.capacity);
        self.length = length;
    }

    fn mutate<R>(&mut self, operation: impl FnOnce(&mut Vec<Value>) -> R) -> R {
        let mut owner = VectorMutation::new(self);
        operation(&mut owner.vector)
    }

    pub fn try_reserve(&mut self, additional: usize) -> Result<(), TryReserveError> {
        self.mutate(|values| values.try_reserve(additional))
    }

    pub fn try_reserve_exact(&mut self, additional: usize) -> Result<(), TryReserveError> {
        self.mutate(|values| values.try_reserve_exact(additional))
    }

    pub fn push(&mut self, value: Value) {
        self.mutate(|values| values.push(value));
    }

    pub fn pop(&mut self) -> Option<Value> {
        self.mutate(Vec::pop)
    }

    pub fn insert(&mut self, index: usize, value: Value) {
        self.mutate(|values| values.insert(index, value));
    }

    pub fn remove(&mut self, index: usize) -> Value {
        self.mutate(|values| values.remove(index))
    }

    /// The caller previously collected `Vec::drain` before releasing removed
    /// values. Keep that one allocation and finish compaction before returning.
    pub fn remove_range(&mut self, range: Range<usize>) -> Vec<Value> {
        self.mutate(|values| values.drain(range).collect())
    }
}

/// Restore the authoritative header even when a standard Vec operation panics.
struct VectorMutation<'a> {
    owner: &'a mut ValueBuffer,
    vector: ManuallyDrop<Vec<Value>>,
}

impl<'a> VectorMutation<'a> {
    fn new(owner: &'a mut ValueBuffer) -> Self {
        let buffer = std::mem::take(owner);
        Self {
            owner,
            vector: ManuallyDrop::new(buffer.into()),
        }
    }
}

impl Drop for VectorMutation<'_> {
    fn drop(&mut self) {
        self.owner.data = self.vector.as_mut_ptr();
        self.owner.length = self.vector.len();
        self.owner.capacity = self.vector.capacity();
    }
}

impl Default for ValueBuffer {
    fn default() -> Self {
        Self::new()
    }
}

impl From<Vec<Value>> for ValueBuffer {
    fn from(values: Vec<Value>) -> Self {
        let mut values = ManuallyDrop::new(values);
        Self {
            data: values.as_mut_ptr(),
            length: values.len(),
            capacity: values.capacity(),
        }
    }
}

impl From<ValueBuffer> for Vec<Value> {
    fn from(buffer: ValueBuffer) -> Self {
        let buffer = ManuallyDrop::new(buffer);
        unsafe { Vec::from_raw_parts(buffer.data, buffer.length, buffer.capacity) }
    }
}

impl Drop for ValueBuffer {
    fn drop(&mut self) {
        // Raw Value pointers do not release their referents. Data's ordinary
        // owned-value traversal performs that separately, exactly as for Vec.
        unsafe {
            drop(Vec::from_raw_parts(self.data, self.length, self.capacity));
        }
    }
}

impl Clone for ValueBuffer {
    fn clone(&self) -> Self {
        self.as_slice().to_vec().into()
    }
}

impl Deref for ValueBuffer {
    type Target = [Value];
    fn deref(&self) -> &[Value] {
        self.as_slice()
    }
}

impl DerefMut for ValueBuffer {
    fn deref_mut(&mut self) -> &mut [Value] {
        unsafe { std::slice::from_raw_parts_mut(self.data, self.length) }
    }
}

impl<'a> IntoIterator for &'a ValueBuffer {
    type Item = &'a Value;
    type IntoIter = std::slice::Iter<'a, Value>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a> IntoIterator for &'a mut ValueBuffer {
    type Item = &'a mut Value;
    type IntoIter = std::slice::IterMut<'a, Value>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl IntoIterator for ValueBuffer {
    type Item = Value;
    type IntoIter = std::vec::IntoIter<Value>;
    fn into_iter(self) -> Self::IntoIter {
        Vec::from(self).into_iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::{Data, Object};
    use std::sync::atomic::{AtomicUsize, Ordering};

    fn value(number: usize) -> Value {
        (number * 2 + 1) as Value
    }

    #[test]
    fn sequence_header_has_the_compiler_layout() {
        assert_eq!(std::mem::size_of::<ValueBuffer>(), 24);
        assert_eq!(std::mem::offset_of!(ValueBuffer, data), 0);
        assert_eq!(std::mem::offset_of!(ValueBuffer, length), 8);
        assert_eq!(std::mem::offset_of!(ValueBuffer, capacity), 16);
        for (data, tag) in [
            (
                Data::Args(
                    vec![value(1)].into(),
                    std::ptr::null_mut(),
                    std::ptr::null_mut(),
                ),
                9,
            ),
            (
                Data::List(
                    vec![value(1)].into(),
                    std::ptr::null_mut(),
                    std::ptr::null_mut(),
                ),
                13,
            ),
        ] {
            let object = Object {
                references: AtomicUsize::new(1),
                tracked: false,
                gc_slot: 0,
                data,
            };
            let base = std::ptr::from_ref(&object) as usize;
            let (buffer, class, attributes) = match &object.data {
                Data::Args(buffer, class, attributes) | Data::List(buffer, class, attributes) => {
                    (buffer, class, attributes)
                }
                _ => unreachable!(),
            };
            assert_eq!(std::ptr::from_ref(buffer) as usize - base, 24);
            assert_eq!(std::ptr::from_ref(&buffer.length) as usize - base, 32);
            assert_eq!(std::ptr::from_ref(&buffer.capacity) as usize - base, 40);
            assert_eq!(std::ptr::from_ref(class) as usize - base, 48);
            assert_eq!(std::ptr::from_ref(attributes) as usize - base, 56);
            unsafe {
                assert_eq!(*((base + 16) as *const u32), tag);
                assert_eq!(*((base + 24) as *const *mut Value), buffer.data);
            }
        }
    }

    #[test]
    fn dropping_a_buffer_does_not_release_its_raw_values() {
        let mut object = Box::new(Object {
            references: AtomicUsize::new(4),
            tracked: false,
            gc_slot: 0,
            data: Data::None,
        });
        let pointer = std::ptr::from_mut(&mut *object);
        let buffer: ValueBuffer = vec![pointer].into();
        drop(buffer);
        assert_eq!(object.references.load(Ordering::Relaxed), 4);
    }

    #[test]
    fn growth_and_consumption_preserve_storage() {
        let mut buffer = ValueBuffer::new();
        buffer.try_reserve_exact(2).unwrap();
        buffer.push(value(1));
        buffer.push(value(2));
        let old_capacity = buffer.capacity();
        buffer.try_reserve(old_capacity + 1).unwrap();
        buffer.insert(1, value(3));
        assert_eq!(buffer.remove(0), value(1));
        assert_eq!(buffer.pop(), Some(value(2)));
        buffer.push(value(4));
        assert_eq!(buffer.remove_range(0..1), vec![value(3)]);
        let allocation = buffer.data;
        let values: Vec<Value> = buffer.into();
        assert_eq!(values.as_ptr(), allocation);
        assert_eq!(values, vec![value(4)]);
    }

    #[test]
    fn mutation_restores_header_on_unwind() {
        let mut buffer: ValueBuffer = vec![value(1)].into();
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            buffer.mutate(|values| {
                values.push(value(2));
                panic!("mutation interrupted");
            });
        }));
        assert!(result.is_err());
        assert_eq!(buffer.as_slice(), &[value(1), value(2)]);
    }

    #[test]
    fn pooled_storage_can_transfer_and_reinitialize_elements() {
        let mut buffer: ValueBuffer = vec![value(1), value(2)].into();
        let allocation = buffer.data;
        unsafe {
            buffer.set_len(0);
            assert!(buffer.is_empty());
            assert_eq!(buffer.data, allocation);
            allocation.write(value(3));
            allocation.add(1).write(value(4));
            buffer.set_len(2);
        }
        assert_eq!(buffer.as_slice(), &[value(3), value(4)]);
    }

    #[test]
    fn failed_reservation_keeps_elements_and_allocation() {
        let mut buffer: ValueBuffer = vec![value(1), value(2)].into();
        let pointer = buffer.data;
        assert!(buffer.try_reserve(usize::MAX).is_err());
        assert_eq!(buffer.data, pointer);
        assert_eq!(buffer.as_slice(), &[value(1), value(2)]);
    }
}
