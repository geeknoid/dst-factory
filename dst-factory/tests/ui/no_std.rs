
extern crate alloc;

use dst_factory::make_dst_factory;

#[make_dst_factory(no_std)]
struct NoStd {
    id: i32,
    data: str,
}

fn main() {
}