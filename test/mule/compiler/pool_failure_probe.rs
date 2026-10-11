//! Fail parent tuple allocation after a child and owned text have been created.
use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicBool, AtomicIsize, AtomicUsize, Ordering};

struct AllocationFailure;
static PAIRS: AtomicUsize = AtomicUsize::new(0);
static FAILED: AtomicBool = AtomicBool::new(false);
static OBJECTS: AtomicIsize = AtomicIsize::new(0);
static TEXT: AtomicIsize = AtomicIsize::new(0);

fn track(layout: Layout, amount: isize) {
    if layout.size() == std::mem::size_of::<mule_runtime::Object>()
        && layout.align() == std::mem::align_of::<mule_runtime::Object>()
    {
        OBJECTS.fetch_add(amount, Ordering::SeqCst);
    }
    if layout.size() == 10 && layout.align() == 1 {
        TEXT.fetch_add(amount, Ordering::SeqCst);
    }
}

unsafe impl GlobalAlloc for AllocationFailure {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        // The child tuple is allocated first. The parent retains that child
        // and allocates its ten-byte text before requesting this second pair.
        if layout.size() == 2 * std::mem::size_of::<mule_runtime::Value>()
            && layout.align() == std::mem::align_of::<mule_runtime::Value>()
            && PAIRS.fetch_add(1, Ordering::SeqCst) == 1
        {
            FAILED.store(true, Ordering::SeqCst);
            return std::ptr::null_mut();
        }
        let pointer = unsafe { System.alloc(layout) };
        if !pointer.is_null() {
            track(layout, 1);
        }
        pointer
    }

    unsafe fn dealloc(&self, pointer: *mut u8, layout: Layout) {
        track(layout, -1);
        unsafe { System.dealloc(pointer, layout) };
    }
}

#[global_allocator]
static ALLOCATOR: AllocationFailure = AllocationFailure;

unsafe extern "C" {
    fn mule_entry(argc: i32, argv: *const *const std::ffi::c_char) -> i32;
}

fn main() {
    std::hint::black_box(mule_runtime::mule_none);
    let objects_before = OBJECTS.load(Ordering::SeqCst);
    let text_before = TEXT.load(Ordering::SeqCst);
    let status = unsafe { mule_entry(0, std::ptr::null()) };
    assert_eq!(status, 1);
    assert!(FAILED.load(Ordering::SeqCst));
    assert_eq!(OBJECTS.load(Ordering::SeqCst), objects_before);
    assert_eq!(TEXT.load(Ordering::SeqCst), text_before);
    println!("partial pool cleanup verified");
}
