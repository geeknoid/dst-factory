
#![allow(dead_code)]

extern crate alloc;

use dst_factory::make_dst_factory;

#[make_dst_factory(no_std, zeroable)]
struct NoStdZeroable {
    id: i32,
    data: [u8],
}

fn main() {
    let buf = NoStdZeroable::build_zeroed(0, 64);
    assert_eq!(buf.id, 0);
    assert_eq!(buf.data.len(), 64);
    assert!(buf.data.iter().all(|&b| b == 0));
}
