use tail_extend::make_dst_builder;

#[make_dst_builder(create, XYZ)]
struct BadVisibility {
    id: i32,
    data: str,
}

fn main() {
}
