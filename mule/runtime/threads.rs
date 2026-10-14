//! Native mutator registration and cooperative stop-the-world coordination.
//!
//! This is the physical counterpart of SBCL thread.c's init_new_thread and
//! safepoint.c's GC state lock. It does not start language workers or make
//! language heap storage safe to share.

use std::cell::Cell;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Condvar, Mutex, MutexGuard, OnceLock};
use std::thread::{self, ThreadId};

/// The compiled poll reads this byte with acquire ordering. The coordinator
/// publishes/releases collection requests through this same atomic object.
#[unsafe(export_name = "mule_safepoint_requested")]
pub static SAFEPOINT_REQUESTED: AtomicBool = AtomicBool::new(false);

#[derive(Clone, Copy, PartialEq, Eq)]
enum State {
    Running,
    Parked,
    Foreign,
}

#[derive(Default)]
struct Registry {
    threads: HashMap<ThreadId, State>,
    collector: Option<ThreadId>,
}

struct Coordinator {
    registry: Mutex<Registry>,
    changed: Condvar,
    requested: &'static AtomicBool,
}

impl Coordinator {
    fn new() -> Self {
        Self::with_request_flag(&SAFEPOINT_REQUESTED)
    }

    fn with_request_flag(requested: &'static AtomicBool) -> Self {
        Self {
            registry: Mutex::new(Registry::default()),
            changed: Condvar::new(),
            requested,
        }
    }

    fn lock(&self) -> MutexGuard<'_, Registry> {
        self.registry
            .lock()
            .expect("native thread registry poisoned")
    }

    fn wait<'a>(&self, registry: MutexGuard<'a, Registry>) -> MutexGuard<'a, Registry> {
        self.changed
            .wait(registry)
            .expect("native thread registry poisoned")
    }

    fn park<'a>(
        &self,
        mut registry: MutexGuard<'a, Registry>,
        id: ThreadId,
    ) -> MutexGuard<'a, Registry> {
        while registry.collector.is_some() && registry.collector != Some(id) {
            *registry
                .threads
                .get_mut(&id)
                .expect("unregistered native mutator") = State::Parked;
            self.changed.notify_all();
            registry = self.wait(registry);
        }
        *registry
            .threads
            .get_mut(&id)
            .expect("unregistered native mutator") = State::Running;
        registry
    }
}

fn coordinator() -> &'static Coordinator {
    static COORDINATOR: OnceLock<Coordinator> = OnceLock::new();
    COORDINATOR.get_or_init(Coordinator::new)
}

thread_local! {
    static REGISTERED: Cell<Option<&'static Coordinator>> = const { Cell::new(None) };
}

/// Must exist before entering compiled language code, and outlive all its
/// owned roots. Registration waits for any current collection before publishing.
pub struct Registration {
    coordinator: &'static Coordinator,
    id: ThreadId,
    owns_registration: bool,
    // A native mutator's state belongs to the thread creating this guard.
    _thread_bound: PhantomData<Rc<()>>,
}

impl Registration {
    pub fn enter() -> Self {
        assert!(
            REGISTERED.with(Cell::get).is_none(),
            "native mutator already registered"
        );
        Self::enter_with(coordinator())
    }

    fn enter_with(coordinator: &'static Coordinator) -> Self {
        let id = thread::current().id();
        let mut registry = coordinator.lock();
        while registry.collector.is_some() {
            registry = coordinator.wait(registry);
        }
        assert!(
            registry.threads.insert(id, State::Running).is_none(),
            "native mutator already registered"
        );
        REGISTERED.with(|slot| slot.set(Some(coordinator)));
        Self {
            coordinator,
            id,
            owns_registration: true,
            _thread_bound: PhantomData,
        }
    }

    /// The caller must keep its roots alive and refrain from touching language
    /// storage until the returned guard is dropped. Reentry waits for GC.
    pub fn foreign(&mut self) -> Foreign<'_> {
        let mut registry = self.coordinator.lock();
        let state = registry
            .threads
            .get_mut(&self.id)
            .expect("unregistered native mutator");
        assert!(
            *state == State::Running,
            "foreign transition from a stopped mutator"
        );
        *state = State::Foreign;
        self.coordinator.changed.notify_all();
        Foreign { registration: self }
    }

    pub fn stop_world(&mut self) -> Collection<'_> {
        let registry = self.coordinator.lock();
        assert!(
            registry.threads.get(&self.id) == Some(&State::Running),
            "collection requested from a stopped mutator"
        );
        let mut registry = self.coordinator.park(registry, self.id);
        assert!(registry.collector.is_none(), "recursive native collection");
        registry.collector = Some(self.id);
        self.coordinator.requested.store(true, Ordering::Release);
        self.coordinator.changed.notify_all();
        while registry
            .threads
            .iter()
            .any(|(id, state)| *id != self.id && *state == State::Running)
        {
            registry = self.coordinator.wait(registry);
        }
        Collection { registration: self }
    }
}

impl Drop for Registration {
    fn drop(&mut self) {
        if !self.owns_registration {
            return;
        }
        let mut registry = self.coordinator.lock();
        assert!(
            registry.collector != Some(self.id),
            "collector unregistered before collection ended"
        );
        registry
            .threads
            .remove(&self.id)
            .expect("native mutator unregistered twice");
        REGISTERED.with(|slot| slot.set(None));
        self.coordinator.changed.notify_all();
    }
}

/// Collection can also be called by the native ABI outside a program's main
/// entry. Such a caller registers for the duration; a language mutator borrows
/// its existing registration and must remain registered after collection.
pub fn with_world_stopped<R>(operation: impl FnOnce() -> R) -> R {
    let mut registration = if let Some(coordinator) = REGISTERED.with(Cell::get) {
        Registration {
            coordinator,
            id: thread::current().id(),
            owns_registration: false,
            _thread_bound: PhantomData,
        }
    } else {
        Registration::enter()
    };
    // Weak-reference callbacks can query tracked objects while their enclosing
    // collection already owns the stopped world. Do not release that guard.
    let already_stopped = registration.coordinator.lock().collector == Some(registration.id);
    if already_stopped {
        return operation();
    }
    let _collection = registration.stop_world();
    operation()
}

pub struct Foreign<'a> {
    registration: &'a mut Registration,
}
impl Drop for Foreign<'_> {
    fn drop(&mut self) {
        let registration = &self.registration;
        drop(
            registration
                .coordinator
                .park(registration.coordinator.lock(), registration.id),
        );
    }
}

pub struct Collection<'a> {
    registration: &'a mut Registration,
}
impl Drop for Collection<'_> {
    fn drop(&mut self) {
        let registration = &self.registration;
        let mut registry = registration.coordinator.lock();
        assert_eq!(registry.collector, Some(registration.id));
        registry.collector = None;
        registration
            .coordinator
            .requested
            .store(false, Ordering::Release);
        registration.coordinator.changed.notify_all();
    }
}

/// The fast path takes no registry lock. Owned native values remain live while
/// parked; moving/reclaiming heap algorithms must still enumerate those roots.
#[unsafe(no_mangle)]
pub extern "C" fn mule_safepoint() {
    let coordinator = coordinator();
    if !coordinator.requested.load(Ordering::Acquire) {
        return;
    }
    assert!(
        REGISTERED
            .with(Cell::get)
            .is_some_and(|registered| std::ptr::eq(registered, coordinator)),
        "unregistered language entry at a safepoint"
    );
    drop(coordinator.park(coordinator.lock(), thread::current().id()));
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::mpsc;
    use std::time::Duration;

    fn isolated() -> &'static Coordinator {
        Box::leak(Box::new(Coordinator::with_request_flag(
            Box::leak(Box::new(AtomicBool::new(false))),
        )))
    }

    #[test]
    fn compiled_poll_uses_the_coordinators_same_byte_sized_atomic() {
        assert_eq!(std::mem::size_of::<AtomicBool>(), 1);
        assert_eq!(std::mem::align_of::<AtomicBool>(), 1);
        assert!(std::ptr::eq(coordinator().requested, &SAFEPOINT_REQUESTED));
    }

    #[test]
    fn nested_collector_queries_keep_outer_collection_and_registration_alive() {
        let coordinator = isolated();
        let parent = Registration::enter_with(coordinator);
        with_world_stopped(|| {
            assert_eq!(coordinator.lock().collector, Some(parent.id));
            with_world_stopped(|| assert_eq!(coordinator.lock().collector, Some(parent.id)));
            assert_eq!(coordinator.lock().collector, Some(parent.id));
        });
        assert!(coordinator.lock().collector.is_none());
        assert_eq!(coordinator.lock().threads.len(), 1);
        drop(parent);
        assert!(coordinator.lock().threads.is_empty());
    }

    #[test]
    fn collection_waits_for_mutator_and_holds_it_until_completion() {
        let coordinator = isolated();
        let mut parent = Registration::enter_with(coordinator);
        let (ready_send, ready) = mpsc::channel();
        let (poll_send, poll) = mpsc::channel();
        let (done_send, done) = mpsc::channel();
        thread::scope(|scope| {
            scope.spawn(move || {
                let child = Registration::enter_with(coordinator);
                ready_send.send(()).unwrap();
                poll.recv().unwrap();
                drop(coordinator.park(coordinator.lock(), child.id));
                done_send.send(()).unwrap();
            });
            ready.recv().unwrap();
            scope.spawn(move || {
                let mut registry = coordinator.lock();
                while registry.collector.is_none() {
                    registry = coordinator.wait(registry);
                }
                poll_send.send(()).unwrap();
            });
            let collection = parent.stop_world();
            assert!(matches!(done.try_recv(), Err(mpsc::TryRecvError::Empty)));
            drop(collection);
            done.recv_timeout(Duration::from_secs(10)).unwrap();
        });
    }

    #[test]
    fn foreign_thread_is_safe_but_reentry_waits_for_collection() {
        let coordinator = isolated();
        let mut parent = Registration::enter_with(coordinator);
        let (ready_send, ready) = mpsc::channel();
        let (resume_send, resume) = mpsc::channel();
        let (done_send, done) = mpsc::channel();
        thread::scope(|scope| {
            scope.spawn(move || {
                let mut child = Registration::enter_with(coordinator);
                let foreign = child.foreign();
                ready_send.send(()).unwrap();
                resume.recv().unwrap();
                drop(foreign);
                done_send.send(()).unwrap();
            });
            ready.recv().unwrap();
            let collection = parent.stop_world();
            resume_send.send(()).unwrap();
            let mut registry = coordinator.lock();
            while !registry
                .threads
                .values()
                .any(|state| *state == State::Parked)
            {
                registry = coordinator.wait(registry);
            }
            assert!(matches!(done.try_recv(), Err(mpsc::TryRecvError::Empty)));
            drop(registry);
            drop(collection);
            done.recv_timeout(Duration::from_secs(10)).unwrap();
        });
    }

    #[test]
    fn thread_registration_waits_for_collection_and_unregisters_on_exit() {
        let coordinator = isolated();
        let mut parent = Registration::enter_with(coordinator);
        let collection = parent.stop_world();
        let (attempt_send, attempt) = mpsc::channel();
        let (registered_send, registered) = mpsc::channel();
        thread::scope(|scope| {
            scope.spawn(move || {
                attempt_send.send(()).unwrap();
                let _child = Registration::enter_with(coordinator);
                registered_send.send(()).unwrap();
            });
            attempt.recv().unwrap();
            assert!(matches!(
                registered.try_recv(),
                Err(mpsc::TryRecvError::Empty)
            ));
            assert_eq!(coordinator.lock().threads.len(), 1);
            drop(collection);
            registered.recv_timeout(Duration::from_secs(10)).unwrap();
        });
        assert_eq!(coordinator.lock().threads.len(), 1);
        drop(parent);
        assert!(coordinator.lock().threads.is_empty());
    }
}
