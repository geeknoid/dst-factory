use dst_factory::make_dst_factory;

#[make_dst_factory(create, clone no_std)]
struct NoCommaAfterClone {
    id: i32,
    data: str,
}

fn main() {
}
