# `helper_fn`

A macro to create closure-like helper functions.

## Usage

This attribute can be applied to a function item to allow it to capture
variables from scope. The helper function must be declared before its usage and
after the variables it captures.

You can supply no arguments to the macro, and it will implicitly capture
variables from the surrounding scope, like a closure:

```rust
use helper_fn::helper_fn;

let mut num = 5;

#[helper_fn]
fn get_num() -> i32 {
  num // implicitly captured
}

assert_eq!(get_num!(), 5); // note that this is a macro invocation, not a function call
```

Or, you can specify which variables to capture, which enables recursion and
greatly improves IDE support:

```rust
use helper_fn::helper_fn;

let mut num = 5;

#[helper_fn(num: i32)] // explicitly captured
fn get_num() -> i32 {
  num
}

assert_eq!(get_num!(), 5);
```

Variables can be captured by value (using move or copy semantics), by reference,
or by mutable reference:

```text
#[helper_fn(copied: i32, moved: Vec<i32>, &by_ref: Foo, &mut by_mut_ref: Bar)]
```

## Rationale

Closures are often used as helper functions that require the surrounding
scope; here's a simple example:
```rust
let mut num = 5;

let get_num = || {
  // Lots of complex stuff
  num
};

assert_eq!(get_num(), 5);
```
This works just fine, but if you mutate `num` in between calls, it won't compile:
```rust compile_fail
let mut num = 5;

let get_num = || {
  // Lots of complex stuff
  num
};

assert_eq!(get_num(), 5);
num += 1; // cannot assign to `num` because it is borrowed
assert_eq!(get_num(), 6);
```
One workaround is to use a local `fn` item and pass variables in:
```rust
let mut num = 5;

fn get_num(num: i32) -> i32 {
  // Lots of complex stuff
  num
}

assert_eq!(get_num(num), 5);
num += 1; // this is ok
assert_eq!(get_num(num), 6);
```
This can work, but it can become quite verbose with multiple variables.

With `helper_fn`, you can get the best of both worlds:
```rust
use helper_fn::helper_fn;

let mut num = 5;

#[helper_fn]
fn get_num() -> i32 {
  num // implicitly captured
}

assert_eq!(get_num!(), 5); // note that this is a macro invocation
num += 1; // this is ok
assert_eq!(get_num!(), 6);
```
You can even capture mutable references:
```rust
use helper_fn::helper_fn;

let mut num = 5;

#[helper_fn]
fn get_num() -> i32 {
  num
}

#[helper_fn]
fn increment_num() {
  num += 1; // implicitly mutably captured
}

assert_eq!(get_num!(), 5);
increment_num!();
assert_eq!(get_num!(), 6);
```
