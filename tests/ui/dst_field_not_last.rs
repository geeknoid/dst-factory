use dst_factory::make_dst_factory;

#[make_dst_factory]
struct DstFieldNotLast {
    id: str,
    data: i32,
}

fn main() {
}
