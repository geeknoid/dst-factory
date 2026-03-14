use std::rc::Rc;
use std::sync::Arc;

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
struct MyStruct;
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
    // Create a user instance in a Box.
    let user: Box<User> = User::new(33, "Alice");
    println!("User name: {}", &user.name);
    println!("User age : {}", user.age);

    // The same struct can also live in an Arc or Rc.
    let shared_user: Arc<User> = User::new_arc(25, "Bob");
    println!("Shared user: {}, age {}", &shared_user.name, shared_user.age);

    let local_user: Rc<User> = User::new_rc(40, "Carol");
    println!("Local user: {}, age {}", &local_user.name, local_user.age);

    // Create a Credential instance.
    let cred: Box<Credential> = Credential::build_from_slice(60, &[0, 1, 2, 3]);
    println!("TTL: {}", &cred.ttl);
    println!("Key: {:?}", &cred.key);

    // Tail data is mutable when you have unique access.
    let mut cred: Box<Credential> = Credential::build_from_slice(60, &[10, 20, 30]);
    assert!(cred.key.len() > 2);
    cred.key[0] = 99;
    cred.key[2] = 0;
    println!("Mutated key: {:?}", &cred.key); // [99, 20, 0]

    // Arc and Rc give shared (immutable) access; use get_mut when you
    // have the only strong reference.
    let mut shared_cred: Arc<Credential> = Credential::build_arc_from_slice(120, &[1, 2, 3]);
    Arc::get_mut(&mut shared_cred).expect("unique").key[1] = 42;
    println!("Arc key: {:?}", &shared_cred.key); // [1, 42, 3]

    let mut local_cred: Rc<Credential> = Credential::build_rc_from_slice(90, &[4, 5, 6]);
    Rc::get_mut(&mut local_cred).expect("unique").key[0] = 0;
    println!("Rc key: {:?}", &local_cred.key); // [0, 5, 6]

    // Create a Traity instance.
    let my_struct = MyStruct {};
    let traity: Box<Traity> = Traity::build(100, my_struct);
    println!("Traity TTL: {}", &traity.ttl);
    println!("Traity data: {}", traity.data.get_42());

    // Trait objects work with Arc and Rc too.
    let shared_traity: Arc<Traity> = Traity::build_arc(200, MyStruct {});
    println!("Arc Traity: {}", shared_traity.data.get_42());

    // Create a Tuply instance.
    let tuply: Box<Tuply> = Tuply::build(100, "Hello");
    println!("Tuply.0: {}", tuply.0);
    println!("Tuply.1: {}", &tuply.1);
}
