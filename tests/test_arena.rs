//! Integration tests for the `arena` flag in `#[make_dst_factory(arena)]`.

use core::sync::atomic::{AtomicUsize, Ordering};
use dst_factory::make_dst_factory;
use multitude::{Arc as MArc, Arena, Box as MBox, Rc as MRc};

// ── str tail ────────────────────────────────────────────────────────

#[make_dst_factory(arena)]
struct User {
    age: u8,
    name: str,
}

#[test]
fn str_tail_typed_arena() {
    let arena = Arena::new();
    let alice: MArc<User> = User::build_arena_arc(&arena, 33, "Alice");
    let bob: MArc<User> = User::build_arena_arc(&arena, 25, "Bob");

    assert_eq!(alice.age, 33);
    assert_eq!(&alice.name, "Alice");
    assert_eq!(bob.age, 25);
    assert_eq!(&bob.name, "Bob");
}

#[test]
fn str_tail_untyped_arena() {
    let arena = Arena::new();
    let user: MArc<User> = User::build_arena_arc(&arena, 42, "Charlie");
    assert_eq!(user.age, 42);
    assert_eq!(&user.name, "Charlie");
}

#[test]
fn str_tail_ap_outlives_arena() {
    let ap = {
        let arena = Arena::new();
        User::build_arena_arc(&arena, 99, "Survivor")
    };
    assert_eq!(ap.age, 99);
    assert_eq!(&ap.name, "Survivor");
}

#[test]
fn str_tail_ap_clone() {
    let arena = Arena::new();
    let a = User::build_arena_arc(&arena, 1, "original");
    let b = a.clone();
    assert_eq!(&a.name, &b.name);
    assert_eq!(a.age, b.age);
}

// ── slice tail ──────────────────────────────────────────────────────

#[make_dst_factory(arena)]
struct Data {
    id: u32,
    values: [f64],
}

#[test]
fn slice_tail_iter_arg() {
    let arena = Arena::new();
    let data: MArc<Data> = Data::build_arena_arc(&arena, 1, vec![1.0, 2.0, 3.0]);
    assert_eq!(data.id, 1);
    assert_eq!(data.values.len(), 3);
    assert!((data.values[0] - 1.0).abs() < f64::EPSILON);
    assert!((data.values[2] - 3.0).abs() < f64::EPSILON);
}

#[make_dst_factory(arena)]
struct ByteData {
    tag: u8,
    bytes: [u8],
}

#[test]
fn slice_tail_from_slice() {
    let arena = Arena::new();
    let data: MArc<ByteData> = ByteData::build_arena_arc_from_slice(&arena, 42, &[10, 20, 30]);
    assert_eq!(data.tag, 42);
    assert_eq!(&data.bytes, &[10, 20, 30]);
}

#[test]
fn slice_tail_empty() {
    let arena = Arena::new();
    let data: MArc<Data> = Data::build_arena_arc(&arena, 0, core::iter::empty::<f64>());
    assert_eq!(data.id, 0);
    assert_eq!(data.values.len(), 0);
}

// ── trait object tail ───────────────────────────────────────────────

#[multitude::dst::pointee(crate = ::multitude::dst)]
trait Greeter: Send + Sync {
    fn greet(&self) -> &'static str;
}

struct English;
impl Greeter for English {
    fn greet(&self) -> &'static str {
        "Hello"
    }
}

struct French;
impl Greeter for French {
    fn greet(&self) -> &'static str {
        "Bonjour"
    }
}

#[make_dst_factory(arena)]
struct Greeting {
    priority: u8,
    greeter: dyn Greeter,
}

#[test]
fn trait_object_tail() {
    let arena = Arena::new();
    let en: MArc<Greeting> = Greeting::build_arena_arc(&arena, 1, English);
    let fr: MArc<Greeting> = Greeting::build_arena_arc(&arena, 2, French);

    assert_eq!(en.priority, 1);
    assert_eq!(en.greeter.greet(), "Hello");
    assert_eq!(fr.priority, 2);
    assert_eq!(fr.greeter.greet(), "Bonjour");
}

// ── arena stats ─────────────────────────────────────────────────────

// `arena_stats_with_dst` removed: multitude::Arena does not expose
// per-allocation len/is_empty (chunks-based stats are gated behind the `stats` feature).

// ── threading ───────────────────────────────────────────────────────

#[test]
fn send_dst_ap_across_threads() {
    let arena = Arena::new();
    let ap = User::build_arena_arc(&arena, 42, "ThreadUser");

    let handle = std::thread::spawn(move || {
        assert_eq!(ap.age, 42);
        assert_eq!(&ap.name, "ThreadUser");
    });
    handle.join().expect("thread panicked");
}

// ── existing build/build_arc/build_rc still work alongside arena ────

#[test]
fn existing_factories_still_work() {
    let boxed: Box<User> = User::build(10, "Boxed");
    assert_eq!(boxed.age, 10);
    assert_eq!(&boxed.name, "Boxed");

    let arced: std::sync::Arc<User> = User::build_arc(20, "Arced");
    assert_eq!(arced.age, 20);
    assert_eq!(&arced.name, "Arced");

    let rced: std::rc::Rc<User> = User::build_rc(30, "Rced");
    assert_eq!(rced.age, 30);
    assert_eq!(&rced.name, "Rced");
}

// ── multitude Box / Rc arena variants ───────────────────────────────

#[test]
fn arena_box_str_tail() {
    let arena = Arena::new();
    let user: MBox<User> = User::build_arena_box(&arena, 7, "Boxed");
    assert_eq!(user.age, 7);
    assert_eq!(&user.name, "Boxed");
}

#[test]
fn arena_rc_str_tail() {
    let arena = Arena::new();
    let user: MRc<User> = User::build_arena_rc(&arena, 8, "Rced");
    let clone = user.clone();
    assert_eq!(user.age, 8);
    assert_eq!(&clone.name, "Rced");
}

#[test]
fn arena_box_rc_slice_tail() {
    let arena = Arena::new();

    let from_iter: MBox<Data> = Data::build_arena_box(&arena, 1, vec![1.0, 2.0, 3.0]);
    assert_eq!(from_iter.id, 1);
    assert_eq!(from_iter.values.len(), 3);

    let from_slice: MRc<ByteData> = ByteData::build_arena_rc_from_slice(&arena, 9, &[4, 5, 6]);
    assert_eq!(from_slice.tag, 9);
    assert_eq!(&from_slice.bytes, &[4, 5, 6]);
}

#[test]
fn arena_box_rc_trait_tail() {
    let arena = Arena::new();

    let en: MBox<Greeting> = Greeting::build_arena_box(&arena, 1, English);
    assert_eq!(en.priority, 1);
    assert_eq!(en.greeter.greet(), "Hello");

    let fr: MRc<Greeting> = Greeting::build_arena_rc(&arena, 2, French);
    assert_eq!(fr.priority, 2);
    assert_eq!(fr.greeter.greet(), "Bonjour");
}

#[test]
fn arena_box_outlives_arena() {
    let boxed = {
        let arena = Arena::new();
        User::build_arena_box(&arena, 99, "BoxSurvivor")
    };
    assert_eq!(boxed.age, 99);
    assert_eq!(&boxed.name, "BoxSurvivor");
}

// ── arena Box / Rc work for non-Send/Sync types ─────────────────────
// `build_arena_arc` requires `Self: Send + Sync` (a `multitude::Arc`
// requirement) and is merely uncallable for a non-`Send` type; the
// `build_arena_box` / `build_arena_rc` variants remain usable.

#[make_dst_factory(arena)]
struct Generic<T> {
    id: u32,
    data: [T],
}

#[test]
fn arena_box_rc_non_send_elements() {
    use std::rc::Rc;

    let arena = Arena::new();

    let boxed: MBox<Generic<Rc<u8>>> = Generic::build_arena_box(&arena, 1, [Rc::new(7u8), Rc::new(8)]);
    assert_eq!(boxed.id, 1);
    assert_eq!(boxed.data.len(), 2);
    assert_eq!(*boxed.data[0], 7);

    let rced: MRc<Generic<Rc<u8>>> = Generic::build_arena_rc(&arena, 2, vec![Rc::new(9u8)]);
    assert_eq!(rced.id, 2);
    assert_eq!(*rced.data[0], 9);
}

// ── iterator panic cleanup ──────────────────────────────────────────
// If a user iterator panics partway through an arena `[T]`-tail build, the tail
// elements already written into the arena must still be dropped. The arena owns
// the backing storage (so it is not reclaimed), but leaking the elements' `Drop`
// would be observable - hence the build's per-element cleanup guard.

static ARENA_ELEM_DROPS: AtomicUsize = AtomicUsize::new(0);

struct ArenaDropElem;

impl Drop for ArenaDropElem {
    fn drop(&mut self) {
        let _ = ARENA_ELEM_DROPS.fetch_add(1, Ordering::Relaxed);
    }
}

struct PanicAfterTwoElems {
    produced: usize,
}

impl Iterator for PanicAfterTwoElems {
    type Item = ArenaDropElem;
    fn next(&mut self) -> Option<ArenaDropElem> {
        assert!(self.produced != 2, "iterator boom");
        self.produced += 1;
        Some(ArenaDropElem)
    }
}

impl ExactSizeIterator for PanicAfterTwoElems {
    fn len(&self) -> usize {
        5
    }
}

#[test]
fn arena_iter_panic_drops_initialized_tail() {
    // The Box and Rc variants share the same iterator factory; verify both drop
    // exactly the two elements written before the iterator panics.
    ARENA_ELEM_DROPS.store(0, Ordering::Relaxed);
    let result = std::panic::catch_unwind(|| {
        let arena = Arena::new();
        let _: MBox<Generic<ArenaDropElem>> = Generic::build_arena_box(&arena, 7, PanicAfterTwoElems { produced: 0 });
    });
    assert!(result.is_err(), "build should have panicked");
    assert_eq!(ARENA_ELEM_DROPS.load(Ordering::Relaxed), 2);

    ARENA_ELEM_DROPS.store(0, Ordering::Relaxed);
    let result = std::panic::catch_unwind(|| {
        let arena = Arena::new();
        let _: MRc<Generic<ArenaDropElem>> = Generic::build_arena_rc(&arena, 9, PanicAfterTwoElems { produced: 0 });
    });
    assert!(result.is_err(), "build should have panicked");
    assert_eq!(ARENA_ELEM_DROPS.load(Ordering::Relaxed), 2);
}
