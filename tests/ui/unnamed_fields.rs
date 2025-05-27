use dst_factory::make_dst_factory;

#[make_dst_factory]
struct UnnamedFields(i32, str);

fn main() {
}
