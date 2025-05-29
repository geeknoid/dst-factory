use dst_factory::make_dst_factory;

#[allow(non_camel_case_types)]
type str = String;

#[make_dst_factory]
struct BadStr {
    id: i32,
    data: str,
}

fn main() {
}
