use dst_factory::make_dst_factory;

#[make_dst_factory(create, pub no_std)]
struct NoCommaAfterVisibility {
    id: i32,
    data: str,
}

fn main() {
}
