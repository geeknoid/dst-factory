# Changelog

## 0.5.0 - 2026-02-18

- Fixed soundness bug: trait object factory now correctly `mem::forget`s the source value after copying into the DST, preventing double-free when the concrete type owns heap data
- Fixed destructurer iterator `Drop` to drop remaining un-yielded elements before deallocating, preventing resource leaks when the iterator is not fully consumed
- Fixed `std::mem::forget` to `core::mem::forget` for `no_std` compatibility
- Replaced `addr_of_mut!` macro with `&raw mut` syntax for consistency with Rust 2024 edition
- Added `ExactSizeIterator` implementation for the destructurer iterator
- Added `Debug` implementation for the destructurer iterator
- Added opt-in `clone` flag that generates a `Clone` implementation for `Box<T>` (supported for `str` and `[T]` tails, not `dyn Trait`)

## 0.4.0 - 2026-02-17

- Added support for serde deserialization
- Update MSRV and use latest dependencies
- Revamp CI stuff to be more thorough

## 0.3.0 - 2025-09-14

- Add dst slice destructurer

## 0.2.0 - 2025-06-06

- Added change log - ([9320c80](https://github.com/geeknoid/dst-factory/commit/9320c80ba0059e7522e18ba220947ff4d285bfd3))
- Simplify MacroArgs code a bit - ([04203f2](https://github.com/geeknoid/dst-factory/commit/04203f22b76bd14247ca1dfc287cd57b82c306c3))
- Improve some error diagnostics - ([2ef5d34](https://github.com/geeknoid/dst-factory/commit/2ef5d3449fd6d156fe870f708c9d6413f1577704))
- Add support for tuple structs - ([0683ef3](https://github.com/geeknoid/dst-factory/commit/0683ef38152b36c70510380e8ab6dce481cf5897))
- Enable and fix more lints - ([20c3fbf](https://github.com/geeknoid/dst-factory/commit/20c3fbfb169628e6bb32f71f8ff5998f1f7311ea))

## 0.1.0 - 2025-06-03

Initial release
