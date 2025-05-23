
extern crate alloc;

use tail_extend::make_dst_builder;

#[make_dst_builder(no_std)]
struct NoStd {
    id: i32,
    data: str,
}

fn main() {
}