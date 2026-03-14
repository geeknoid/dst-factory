use dst_factory::make_dst_factory;

#[make_dst_factory(create, debug no_std)]
struct NoCommaAfterDebug {
    id: i32,
    data: str,
}

fn main() {
}
