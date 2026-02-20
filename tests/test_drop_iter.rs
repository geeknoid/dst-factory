use core::sync::atomic::{AtomicUsize, Ordering};
use dst_factory::make_dst_factory;

static DROP_COUNT: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone)]
struct DropCounter(#[expect(dead_code, reason = "exists to test Drop")] u32);

impl Drop for DropCounter {
    fn drop(&mut self) {
        let _ = DROP_COUNT.fetch_add(1, Ordering::Relaxed);
    }
}

#[make_dst_factory]
struct DropSliceStruct<T> {
    id: u32,
    items: [T],
}

#[test]
fn iterator_drop_cleans_up_remaining_elements() {
    DROP_COUNT.store(0, Ordering::Relaxed);

    let instance: Box<DropSliceStruct<DropCounter>> = DropSliceStruct::build(1, vec![DropCounter(1), DropCounter(2), DropCounter(3)]);

    // Destructure but only consume one element, then drop the iterator
    let (_, mut iter) = DropSliceStruct::destructure(instance);
    let _ = iter.next(); // consume one, drops that element
    drop(iter); // should drop remaining 2 elements

    // vec elements are moved into the DST (no drop), then:
    // - next() reads one out and it gets dropped when _ binding drops = 1
    // - drop(iter) drops remaining 2 = 2
    // Total = 3
    assert_eq!(DROP_COUNT.load(Ordering::Relaxed), 3);
}
