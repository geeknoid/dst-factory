use dst_factory::make_dst_factory;

#[make_dst_factory(new, pub(crate))]
struct User {
    age: u8,
    name: str,
}

#[make_dst_factory]
struct Credential {
    ttl: u32,
    key: [u16],
}

trait MyTrait {
    fn get_42(&self) -> u32;
}
struct MyStruct {}
impl MyTrait for MyStruct {
    fn get_42(&self) -> u32 {
        42
    }
}

#[make_dst_factory]
struct Traity {
    ttl: u32,
    data: dyn MyTrait,
}

#[make_dst_factory]
struct Tuply(u32, str);

fn main() {
    // Create a user instance.
    let user: Box<User> = User::new(33, "A string!");
    println!("User name: {}", &user.name);
    println!("User age : {}", user.age);

    // Create a Credential instance.
    let cred: Box<Credential> = Credential::build_from_slice(60, &[0, 1, 2, 3]);
    println!("TTL: {}", &cred.ttl);
    println!("Key: {:?}", &cred.key);

    // Create a Traity instance.
    let my_struct = MyStruct {};
    let traity: Box<Traity> = Traity::build(100, my_struct);
    println!("Traity TTL: {}", &traity.ttl);
    println!("Traity data: {}", traity.data.get_42());

    // Create a Tuply instance.
    let tuply: Box<Tuply> = Tuply::build(100, "Hello");
    println!("Tuply.0: {}", tuply.0);
    println!("Tuply.1: {}", &tuply.1);
}
