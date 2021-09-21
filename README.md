# `helper_fn`

A macro to create closure-like helper functions.

## Usage

This attribute can be applied to a function item to allow it to inherit
variables from the parent scope. The helper function must be declared before its
usage and after the variables it inherits.
```rust
use helper_fn::helper_fn;

let mut num = 5;

#[helper_fn(num: i32)] // captures num from the surrounding scope
fn get_num() -> i32 {
  num
}

assert_eq!(get_num!(), 5); // note that this is a macro invocation, not a function call
```

Variables can be captured by value (using move or copy semantics), by reference,
or by mutable reference:
```text
#[helper_fn(copied: i32, moved: Vec<i32>, &by_ref: Foo, &mut by_mut_ref: Bar)]
```

If there are scoping issues, you can use the `use_helper_fn` macro:
```rust
use helper_fn::{helper_fn, use_helper_fn};

let mut num = 5;

// hoist the definitions
use_helper_fn! {
  get_num(num),
  get_num_times_two(num) as get_num_times_2,
  increment_num(&mut num),
};

assert_eq!(get_num!(), 5);
assert_eq!(get_num_times_2!(), 10);
increment_num!();
assert_eq!(get_num!(), 6);
assert_eq!(get_num_times_2!(), 12);

#[helper_fn(num: i32)]
fn get_num() -> i32 {
  num
}

#[helper_fn(num: i32)]
fn get_num_times_two() -> i32 {
  // reuse the definition from the parent scope
  // has to be a different name to avoid conflict
  use_helper_fn!(get_num(num) as get_num_);
  get_num_!() * 2
}

#[helper_fn(&mut num: i32)]
fn increment_num() {
  *num += 1;
}
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

#[helper_fn(num: i32)]
fn get_num() -> i32 {
  num
}

assert_eq!(get_num!(), 5); // you don't need to pass in `num`
num += 1; // this is ok
assert_eq!(get_num!(), 6); // note that these calls are macro invocations
```