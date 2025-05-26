use dst_factory::make_dst_factory;

#[make_dst_factory("A String Literal")]
struct StringInAttribute {
    id: i32,
    data: str,
}

fn main() {
}
