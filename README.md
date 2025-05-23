# C-like [flexible array members](https://en.wikipedia.org/wiki/Flexible_array_member) for Rust

[![crate.io](https://img.shields.io/crates/v/tail-extend.svg)](https://crates.io/crates/tail-extend)
[![docs.rs](https://docs.rs/tail-extend/badge.svg)](https://docs.rs/tail-extend)
[![CI](https://github.com/geeknoid/tail-extend/workflows/main/badge.svg)](https://github.com/geeknoid/tail-extend/actions)
[![Coverage](https://codecov.io/gh/geeknoid/tail-extend/graph/badge.svg?token=FCUG0EL5TI)](https://codecov.io/gh/geeknoid/tail-extend)
[![Minimum Supported Rust Version 1.85](https://img.shields.io/badge/MSRV-1.85-blue.svg)]()
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

* [Summary](#summary)
* [Example](#example)
* [Attribute Features](#attribute-features)
* [Error Conditions](#error-conditions)

## Summary

This crate lets you allocate variable data inline at the end of a struct. If you have a
struct that gets allocated on the heap and has some variable-length data associated with it
(like a string or an array), then you can allocate this data directly inline with the struct.
This saves memory by avoiding the need for a pointer and a separate allocation, and saves CPU
cycles by eliminating the need for an indirection when accessing the data.

Rust supports the notion of [Dynamically Sized Types](https://doc.rust-lang.org/reference/dynamically-sized-types.html), known as DSTs,
which are types that have a size
not known at compile time. DSTs are perfect to implement flexible array members. But
unfortunately, Rust doesn't provide an out-of-the-box way to allocate instances of such types.
This is where this crate comes in.

You can apply the `#[make_dst_builder]` attribute to your DST struct which causes factory
methods to be produced that let you easily and safely create instances of your DST.

## Example

Here's an example using an array as the last field of a struct:

```rust
use tail_extend::make_dst_builder;

#[make_dst_builder]
struct User {
    age: u32,
    signing_key: [u8],
}

// allocate one user with a 4 byte key
let a = User::build(33, &[0, 1, 2, 3]);

// allocate another user with a 5 byte key
let b = User::build(33, &[0, 1, 2, 3, 4]);

// allocate another user, this time using an iterator
let v = vec![0u8, 1, 2, 3, 4];
let iter = v.into_iter();
let c = User::build_from_iter(33, iter);
```
Here's another example, this time using a string as the last field of a struct:

```rust
use tail_extend::make_dst_builder;

#[make_dst_builder]
struct User {
    age: u32,
    name: str,
}

// allocate one user with a 5-character string
let a = User::build(33, "Alice");

// allocate another user with a 3-character string
let b = User::build(33, "Bob");
```
Because DSTs don't have a known size at compile time, you can't store them on the stack,
and you can't pass them by value. As a result of these constraints, the `build` and
`build_from_iter` functions always return boxed instances of the structs.

## Attribute Features

The common use case for the `#[make_dst_builder]` attribute is to not pass any arguments.
This results in factory methods called `build` when using a string as the last field of the
struct, and `build` and `build_from_iter` when using an array as the last field of the struct.

The generated methods are private by default and have the following signatures:

```ignore
// for arrays
fn build(field1, field2, ..., last_field: &[last_field_type]) -> Box<Self>;
fn build_from_iter<I>(field1, field2, ..., last_field:: I) -> Box<Self>
where
    I: IntoIterator<Item = last_field_type>,
    <I as IntoIterator>::IntoIter: ExactSizeIterator,

// for strings
fn build(field1, field2, ..., last_field: &str) -> Box<Self>;
```

The attribute lets you control the name of the generated factory methods, their
visibility, and whether to generate for the `no_std` environment. The general
grammar is:

```ignore
#[make_dst_builder(<base_method_name>[, <visibility>] [, no_std])]
```

Some examples:

```ignore
// The factory methods will be private and called `create` and `create_from_iter`
#[make_dst_builder(create)]

// The factory methods will be public and called `create` and `create_from_iter`
#[make_dst_builder(create, pub)]

// The factory methods will be private, called `create` and `create_from_iter`, and support the `no_std` environment
#[make_dst_builder(create, no_std)]
```

## Error Conditions

The `#[make_dst_builder]` macro produces a compile-time error if:

- It's applied to anything other than a struct with named fields.
- The struct has no fields (a tail field is essential for a DST).
- The last field of the struct is not a slice (`[T]`) or a string (`str`).
- The arguments are malformed (e.g., incorrect visibility keyword, too many arguments).
