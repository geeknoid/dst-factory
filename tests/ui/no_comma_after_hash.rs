use dst_factory::make_dst_factory;

#[make_dst_factory(create, debug, eq, hash no_std)]
struct NoCommaAfterHash {
    id: i32,
    data: str,
}

fn main() {
}
