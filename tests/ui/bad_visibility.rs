use dst_factory::make_dst_factory;

#[make_dst_factory(create, XYZ)]
struct BadVisibility {
    id: i32,
    data: str,
}

fn main() {
}
