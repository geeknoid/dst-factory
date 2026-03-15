use dst_factory::make_dst_factory;

#[make_dst_factory(create, zeroable no_std)]
struct NoCommaAfterZeroable {
    id: i32,
    data: [u8],
}

fn main() {
}
