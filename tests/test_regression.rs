use core::sync::atomic::{AtomicUsize, Ordering};
use dst_factory::make_dst_factory;

// --- Regression: fully zero-sized DST must preserve slice length ---
//
// When a struct has no sized header field and a ZST element slice tail (e.g.
// `[()]`), the whole DST is zero-sized and the `Box` factory takes its ZST
// allocation fast-path. That path must still carry the requested slice length
// as the pointer metadata, otherwise `.len()` silently reports 0.

#[make_dst_factory(pub)]
struct OnlyZstSlice {
    data: [()],
}

#[test]
fn zero_sized_dst_preserves_slice_length() {
    let from_slice: Box<OnlyZstSlice> = OnlyZstSlice::build_from_slice(&[(), (), ()]);
    assert_eq!(from_slice.data.len(), 3);

    let from_iter: Box<OnlyZstSlice> = OnlyZstSlice::build([(), (), ()]);
    assert_eq!(from_iter.data.len(), 3);

    // `destructure` must yield exactly the requested number of elements.
    let mut it = OnlyZstSlice::destructure(OnlyZstSlice::build_from_slice(&[(), (), (), ()]));
    let mut count = 0;
    while it.next().is_some() {
        count += 1;
    }
    assert_eq!(count, 4);

    // Arc/Rc preserve the length; assert Box matches them.
    let arc: std::sync::Arc<OnlyZstSlice> = OnlyZstSlice::build_arc_from_slice(&[(), (), ()]);
    let rc: std::rc::Rc<OnlyZstSlice> = OnlyZstSlice::build_rc_from_slice(&[(), (), ()]);
    assert_eq!(arc.data.len(), 3);
    assert_eq!(rc.data.len(), 3);
}

// --- Regression: a ZST tail element that has a `Drop` impl must be dropped
//     exactly once per requested element, even in the zero-sized fast-path. ---

static ZST_DROP_COUNT: AtomicUsize = AtomicUsize::new(0);

struct ZstDrop;

impl Drop for ZstDrop {
    fn drop(&mut self) {
        let _ = ZST_DROP_COUNT.fetch_add(1, Ordering::Relaxed);
    }
}

#[make_dst_factory(pub)]
struct OnlyZstDropSlice<T> {
    data: [T],
}

#[test]
fn zero_sized_dst_drops_each_element_once() {
    ZST_DROP_COUNT.store(0, Ordering::Relaxed);

    let instance: Box<OnlyZstDropSlice<ZstDrop>> = OnlyZstDropSlice::build([ZstDrop, ZstDrop, ZstDrop]);
    assert_eq!(instance.data.len(), 3);
    drop(instance);

    assert_eq!(ZST_DROP_COUNT.load(Ordering::Relaxed), 3);
}

// --- Regression: header fields must be dropped if the iterator panics ---
//
// The iterator factory moves the header fields into the freshly allocated
// buffer before consuming the iterator. If the user's iterator panics partway
// through, the cleanup guard must drop those already-moved header fields,
// otherwise they leak (their destructors never run).

static HEADER_DROP_COUNT: AtomicUsize = AtomicUsize::new(0);

struct TracksHeaderDrop(#[expect(dead_code, reason = "exists to observe drops")] u32);

impl Drop for TracksHeaderDrop {
    fn drop(&mut self) {
        let _ = HEADER_DROP_COUNT.fetch_add(1, Ordering::Relaxed);
    }
}

struct PanicAfterTwo {
    produced: usize,
}

impl Iterator for PanicAfterTwo {
    type Item = u8;
    fn next(&mut self) -> Option<u8> {
        assert!(self.produced != 2, "iterator boom");
        self.produced += 1;
        Some(0)
    }
}

impl ExactSizeIterator for PanicAfterTwo {
    fn len(&self) -> usize {
        5
    }
}

#[make_dst_factory(pub)]
struct HeaderHolder {
    first: TracksHeaderDrop,
    second: TracksHeaderDrop,
    tail: [u8],
}

fn assert_header_dropped_on_panic<F: FnOnce() + core::panic::UnwindSafe>(build: F) {
    HEADER_DROP_COUNT.store(0, Ordering::Relaxed);
    let result = std::panic::catch_unwind(build);
    assert!(result.is_err(), "build should have panicked");
    // Both header fields must have been dropped exactly once.
    assert_eq!(HEADER_DROP_COUNT.load(Ordering::Relaxed), 2);
}

#[test]
fn iterator_panic_drops_header_fields() {
    // Run the Box/Arc/Rc cases sequentially in a single test: they share the
    // `HEADER_DROP_COUNT` static, so they must not run concurrently.
    assert_header_dropped_on_panic(|| {
        let _: Box<HeaderHolder> = HeaderHolder::build(TracksHeaderDrop(1), TracksHeaderDrop(2), PanicAfterTwo { produced: 0 });
    });
    assert_header_dropped_on_panic(|| {
        let _: std::sync::Arc<HeaderHolder> =
            HeaderHolder::build_arc(TracksHeaderDrop(1), TracksHeaderDrop(2), PanicAfterTwo { produced: 0 });
    });
    assert_header_dropped_on_panic(|| {
        let _: std::rc::Rc<HeaderHolder> = HeaderHolder::build_rc(TracksHeaderDrop(1), TracksHeaderDrop(2), PanicAfterTwo { produced: 0 });
    });
}

// --- Regression: zero-sized DST fields/tails with `Drop` must be dropped
//     exactly once. The zero-sized Box factories must move header fields and
//     the (trait) tail value into the allocation instead of leaving them owned
//     by the local argument tuple, which would drop them a second time. ---

// Trait-object tail whose concrete type is zero-sized and has a `Drop` impl.
static TRAIT_DD: AtomicUsize = AtomicUsize::new(0);

trait Marker {
    fn id(&self) -> u32;
}

struct ZstMarker;
impl Marker for ZstMarker {
    fn id(&self) -> u32 {
        7
    }
}
impl Drop for ZstMarker {
    fn drop(&mut self) {
        let _ = TRAIT_DD.fetch_add(1, Ordering::Relaxed);
    }
}

#[make_dst_factory(pub)]
struct TraitNode {
    tail: dyn Marker,
}

#[test]
fn zero_sized_trait_object_dropped_once() {
    TRAIT_DD.store(0, Ordering::Relaxed);
    {
        let n: Box<TraitNode> = TraitNode::build(ZstMarker);
        assert_eq!(n.tail.id(), 7);
    }
    assert_eq!(TRAIT_DD.load(Ordering::Relaxed), 1);

    // Arc/Rc always allocate, but verify they also drop exactly once.
    TRAIT_DD.store(0, Ordering::Relaxed);
    {
        let _n: std::sync::Arc<TraitNode> = TraitNode::build_arc(ZstMarker);
    }
    assert_eq!(TRAIT_DD.load(Ordering::Relaxed), 1);

    TRAIT_DD.store(0, Ordering::Relaxed);
    {
        let _n: std::rc::Rc<TraitNode> = TraitNode::build_rc(ZstMarker);
    }
    assert_eq!(TRAIT_DD.load(Ordering::Relaxed), 1);
}

// Zero-sized header field with a `Drop` impl, slice tail.
static SLICE_HDR_DD: AtomicUsize = AtomicUsize::new(0);

struct SliceHdrDrop;
impl Drop for SliceHdrDrop {
    fn drop(&mut self) {
        let _ = SLICE_HDR_DD.fetch_add(1, Ordering::Relaxed);
    }
}

#[make_dst_factory(pub)]
struct SliceHdrNode {
    hdr: SliceHdrDrop,
    data: [()],
}

#[test]
fn zero_sized_slice_header_dropped_once() {
    SLICE_HDR_DD.store(0, Ordering::Relaxed);
    {
        let s: Box<SliceHdrNode> = SliceHdrNode::build_from_slice(SliceHdrDrop, &[(), ()]);
        assert_eq!(s.data.len(), 2);
    }
    assert_eq!(SLICE_HDR_DD.load(Ordering::Relaxed), 1);

    SLICE_HDR_DD.store(0, Ordering::Relaxed);
    {
        let s: Box<SliceHdrNode> = SliceHdrNode::build(SliceHdrDrop, [(), (), ()]);
        assert_eq!(s.data.len(), 3);
    }
    assert_eq!(SLICE_HDR_DD.load(Ordering::Relaxed), 1);
}

// Zero-sized header field with a `Drop` impl, `str` tail (empty string -> zero-sized).
static STR_HDR_DD: AtomicUsize = AtomicUsize::new(0);

struct StrHdrDrop;
impl Drop for StrHdrDrop {
    fn drop(&mut self) {
        let _ = STR_HDR_DD.fetch_add(1, Ordering::Relaxed);
    }
}

#[make_dst_factory(pub)]
struct StrHdrNode {
    hdr: StrHdrDrop,
    data: str,
}

#[test]
fn zero_sized_str_header_dropped_once() {
    STR_HDR_DD.store(0, Ordering::Relaxed);
    {
        let s: Box<StrHdrNode> = StrHdrNode::build(StrHdrDrop, "");
        assert_eq!(&s.data, "");
    }
    assert_eq!(STR_HDR_DD.load(Ordering::Relaxed), 1);
}

// Zero-sized header field with a `Drop` impl, zeroable slice tail.
static ZERO_HDR_DD: AtomicUsize = AtomicUsize::new(0);

struct ZeroHdrDrop;
impl Drop for ZeroHdrDrop {
    fn drop(&mut self) {
        let _ = ZERO_HDR_DD.fetch_add(1, Ordering::Relaxed);
    }
}

#[make_dst_factory(pub, zeroable)]
struct ZeroHdrNode {
    hdr: ZeroHdrDrop,
    data: [u8],
}

#[test]
fn zeroable_header_dropped_once() {
    // Non-zero-sized: `[u8]` of length 4 keeps the struct non-zero-sized, so this
    // exercises the regular zeroed path with a `Drop` header.
    ZERO_HDR_DD.store(0, Ordering::Relaxed);
    {
        let z: Box<ZeroHdrNode> = ZeroHdrNode::build_zeroed(ZeroHdrDrop, 4);
        assert_eq!(z.data.len(), 4);
        assert!(z.data.iter().all(|&b| b == 0));
    }
    assert_eq!(ZERO_HDR_DD.load(Ordering::Relaxed), 1);

    // Zero-sized: length 0 makes the whole struct zero-sized, exercising the
    // zeroed ZST fast-path with a `Drop` header.
    ZERO_HDR_DD.store(0, Ordering::Relaxed);
    {
        let z: Box<ZeroHdrNode> = ZeroHdrNode::build_zeroed(ZeroHdrDrop, 0);
        assert_eq!(z.data.len(), 0);
    }
    assert_eq!(ZERO_HDR_DD.load(Ordering::Relaxed), 1);
}

// --- Regression: `#[repr(packed)]` structs with an over-aligned tail element ---
//
// The tail elements (and under-aligned header fields) live at offsets that are
// not aligned to their natural alignment. The generated code must compute the
// packed layout, use unaligned reads/writes, and avoid forming references to
// packed fields.

#[make_dst_factory(pub)]
#[repr(Rust, packed(1))]
struct PackedU32 {
    a: u8,
    b: u32,      // under-aligned header field
    tail: [u32], // under-aligned tail elements
}

#[test]
fn packed_over_aligned_slice_tail() {
    let s: Box<PackedU32> = PackedU32::build_from_slice(7, 0xAABB_CCDD, &[1u32, 2, 3]);
    assert_eq!({ s.a }, 7);
    assert_eq!({ s.b }, 0xAABB_CCDD);
    assert_eq!((&raw const s.tail).len(), 3);

    let it: Box<PackedU32> = PackedU32::build(8, 0x1234_5678, [10u32, 20, 30, 40]);
    assert_eq!((&raw const it.tail).len(), 4);

    let arc: std::sync::Arc<PackedU32> = PackedU32::build_arc_from_slice(9, 2, &[100u32, 200]);
    let rc: std::rc::Rc<PackedU32> = PackedU32::build_rc_from_slice(9, 2, &[5u32]);
    assert_eq!((&raw const arc.tail).len(), 2);
    assert_eq!((&raw const rc.tail).len(), 1);

    // destructure returns header fields and tail elements by value.
    let (a, b, iter) = PackedU32::destructure(s);
    assert_eq!(a, 7);
    assert_eq!(b, 0xAABB_CCDD);
    assert_eq!(iter.collect::<Vec<u32>>(), vec![1, 2, 3]);
}
