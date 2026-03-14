use dst_factory::make_dst_factory;

#[make_dst_factory(create, debug, eq no_std)]
struct NoCommaAfterEq {
    id: i32,
    data: str,
}

fn main() {
}
