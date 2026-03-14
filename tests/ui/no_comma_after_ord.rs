use dst_factory::make_dst_factory;

#[make_dst_factory(create, eq, ord no_std)]
struct NoCommaAfterOrd {
    id: i32,
    data: str,
}

fn main() {
}
