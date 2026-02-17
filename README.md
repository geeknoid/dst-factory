# dst-factory

[![crate.io](https://img.shields.io/crates/v/dst-factory.svg)](https://crates.io/crates/dst-factory)
[![docs.rs](https://docs.rs/dst-factory/badge.svg)](https://docs.rs/dst-factory)
[![CI](https://github.com/geeknoid/dst-factory/workflows/main/badge.svg)](https://github.com/geeknoid/dst-factory/actions)
[![Coverage](https://codecov.io/gh/geeknoid/dst-factory/graph/badge.svg?token=FCUG0EL5TI)](https://codecov.io/gh/geeknoid/dst-factory)
[![Minimum Supported Rust Version 1.90](https://img.shields.io/badge/MSRV-1.90-blue.svg)]()
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

* [Summary](#summary)
* [Why Should You Care?](#why-should-you-care)
* [Examples](#examples)
* [Attribute Features](#attribute-features)
* [Other Features](#other-features)
* [Error Conditions](#error-conditions)
* [Acknowledgments](#acknowledgements)

## Summary

<!-- cargo-rdme start -->

C-like [flexible array members](https://en.wikipedia.org/wiki/Flexible_array_member) for Rust.

This crate lets you allocate variable data inline at the end of a struct. If you have a
struct that gets allocated on the heap and has some variable-length data associated with it
(like a string or an array), then you can allocate this data directly inline with the struct.
This saves memory by avoiding the need for a pointer and a separate allocation, and saves CPU
cycles by eliminating the need for indirection when accessing the data.

Rust supports the notion of [Dynamically Sized Types](https://doc.rust-lang.org/reference/dynamically-sized-types.html), known as DSTs,
which are types that have a size not known at compile time. DSTs are perfect to implement
flexible array members. But unfortunately, Rust doesn't provide an out-of-the-box way to allocate
instances of such types. This is where this crate comes in.

You can apply the #[[`macro@make_dst_factory`]] attribute to your DST structs, which causes factory
functions to be produced that let you easily and safely create instances of your DSTs.

## Why Should You Care?

Dynamically sized types aren't for everyone. You can't use them as local variables
or put them in arrays or vectors, so they can be inconvenient to use. However, their value
lies in situations where you have a lot of heap-allocated objects, as they can substantially
reduce the memory footprint of your application. If you're building graphs, trees, or other
dynamic data structures, you can often leverage DSTs to keep your individual nodes smaller
and more efficient.

## Examples

Here's an example using an array as the last field of a struct:

```rust
use dst_factory::make_dst_factory;

#[make_dst_factory]
struct User {
    age: u32,
    signing_key: [u8],
}

// allocate one user with a 4-byte key
let a = User::build(33, [0, 1, 2, 3]);

// allocate another user with a 5-byte key
let b = User::build_from_slice(33, &[0, 1, 2, 3, 4]);

// allocate another user, this time using an iterator
let v = vec![0, 1, 2, 3, 4];
let c = User::build(33, v.iter().copied());

// destructure this user and compare its key to the vector
// this has the advantage of iterating over u8, not &u8 or &mut u8.
let (_age, signing_key) = User::destructure(c);
assert!(signing_key.eq(v.into_iter()));
```
Here's another example, this time using a string as the last field of a struct:

```rust
use dst_factory::make_dst_factory;

#[make_dst_factory]
struct User {
    age: u32,
    name: str,
}

// allocate one user with a 5-character string
let a = User::build(33, "Alice");

// allocate another user with a 3-character string
let b = User::build(33, "Bob");
```
And finally, here's an example using a trait object as the last field of a struct:
```rust
use dst_factory::make_dst_factory;

// a trait we'll use in our DST
trait NumberProducer {
    fn get_number(&self) -> u32;
}

// an implementation of the trait we're going to use
struct FortyTwoProducer;
impl NumberProducer for FortyTwoProducer {
    fn get_number(&self) -> u32 {
        42
    }
}

// another implementation of the trait we're going to use
struct TenProducer;
impl NumberProducer for TenProducer {
    fn get_number(&self) -> u32 {
        10
    }
}

#[make_dst_factory]
struct Node {
    count: u32,
    producer: dyn NumberProducer,
}

// allocate an instance with one implementation of the trait
let a = Node::build(33, FortyTwoProducer{});
assert_eq!(42, a.producer.get_number());

// allocate an instance with another implementation of the trait
let b = Node::build(33, TenProducer{});
assert_eq!(10, b.producer.get_number());
```

Because DSTs don't have a known size at compile time, you can't store them on the stack,
and you can't pass them by value. As a result of these constraints, the factory functions
always return boxed instances of the structs.

## Attribute Features

The common use case for the #[[`macro@make_dst_factory`]] attribute is to not pass any arguments.
This results in a function called `build` when using a string or dynamic trait as the
last field of the struct, and the functions `build`, `build_from_slice`, and `destructure` when using an array as the last
field of the struct.

The generated functions are private by default and have the following signatures:

```rust
// for arrays
fn build<G>(field1, field2, ..., last_field: G) -> Box<Self>
where
    G: IntoIterator<Item = last_field_type>,
    <G as IntoIterator>::IntoIter: ExactSizeIterator,

fn build_from_slice(field1, field2, ..., last_field: &[last_field_type]) -> Box<Self>
where
    last_field_type: Copy + Sized;

fn destructure(this: Box<Self>) -> (Type1, Type2, ..., SelfIter);

// for strings
fn build(field1, field2, ..., last_field: impl AsRef<str>) -> Box<Self>;

// for traits
fn build(field1, field2, ..., last_field: G) -> Box<Self>
where
    G: TraitName + Sized;
```

The attribute lets you control the name of the generated functions, their
visibility, and whether to generate code for the `no_std` environment. The general
grammar is:

```rust
#[make_dst_factory(<base_factory_name> [, destructurer=<destructurer_name>] [, iterator=<iterator_name>] [, <visibility>] [, no_std] [, generic=<generic_name>])]
```

Some examples:

```rust
// The generated functions will be public and called `build`, `build_from_slice`, and `destructure`.
#[make_dst_factory(pub)]

// The generated functions will be private and called `create`, `create_from_slice`, and `destructure`.
#[make_dst_factory(create)]

// The generated functions will be private and called `create`, `create_from_slice`, and `destroy`.
#[make_dst_factory(create, destructurer = destroy)]

// The generated functions will be public and called `create`, `create_from_slice`, and `destructure`
#[make_dst_factory(create, pub)]

// The generated functions will be private, called `create`, `create_from_slice`, and `destructure`, and support the `no_std` environment
#[make_dst_factory(create, no_std)]

// The generated functions will be private, called `create`, `create_from_slice`, and `destructure`,
// support the `no_std` environment, and will have generic types called `X`.
#[make_dst_factory(create, no_std, generic=X)]
```

## Other Features

You can use the #[[`macro@make_dst_factory`]] attribute on structs with the normal Rust
representation or C representation (`#[repr(C)]`), with any padding and alignment
specification. See the Rust reference on [Type Layout](https://doc.rust-lang.org/reference/type-layout.html)
for more details.

## Error Conditions

The #[[`macro@make_dst_factory`]] attribute produces a compile-time error if:

- It's applied to anything other than a regular struct or a tuple struct.
- Its arguments are malformed (e.g., incorrect visibility keyword, too many arguments).
- The struct has no fields.
- The last field of the struct is not a slice (`[T]`), a string (`str`), or a trait object (`dyn Trait`).
- The resulting struct exceeds the maximum size allowed of `isize::MAX`.

## Acknowledgements

Many thanks to <https://github.com/scottmcm> for his invaluable help getting the factory methods
in top shape.

<!-- cargo-rdme end -->

