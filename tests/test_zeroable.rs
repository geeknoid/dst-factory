use std::rc::Rc;
use std::sync::Arc;

use dst_factory::make_dst_factory;

#[make_dst_factory(zeroable)]
struct ZeroableBuffer {
    current_index: usize,
    payload: [u8],
}

#[test]
fn zeroed_box_basic() {
    let buf: Box<ZeroableBuffer> = ZeroableBuffer::build_zeroed(0, 1024);
    assert_eq!(buf.current_index, 0);
    assert_eq!(buf.payload.len(), 1024);
    assert!(buf.payload.iter().all(|&b| b == 0));
}

#[test]
fn zeroed_box_empty() {
    let buf: Box<ZeroableBuffer> = ZeroableBuffer::build_zeroed(42, 0);
    assert_eq!(buf.current_index, 42);
    assert!(buf.payload.is_empty());
}

#[test]
fn zeroed_arc() {
    let buf: Arc<ZeroableBuffer> = ZeroableBuffer::build_arc_zeroed(0, 512);
    assert_eq!(buf.payload.len(), 512);
    assert!(buf.payload.iter().all(|&b| b == 0));
    assert_eq!(Arc::strong_count(&buf), 1);
}

#[test]
fn zeroed_rc() {
    let buf: Rc<ZeroableBuffer> = ZeroableBuffer::build_rc_zeroed(0, 256);
    assert_eq!(buf.payload.len(), 256);
    assert!(buf.payload.iter().all(|&b| b == 0));
    assert_eq!(Rc::strong_count(&buf), 1);
}
