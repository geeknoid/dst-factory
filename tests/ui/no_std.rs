
extern crate alloc;

use dst_factory::make_dst_factory;

#[make_dst_factory(no_std)]
struct NoStdStr {
    id: i32,
    data: str,
}

#[make_dst_factory(no_std)]
struct NoStdArray {
    id: i32,
    data: [u32],
}

fn main() {
}