use helper_fn::{helper_fn, use_helper_fn};

#[test]
fn foo() {
  let mut num = 5;

  use_helper_fn! {
    get_num(&num),
    make_num_zero(&mut num),
  }

  assert_eq!(get_num!(), 5);
  make_num_zero!();
  assert_eq!(get_num!(), 0);
  num = -5;
  assert_eq!(get_num!(), -5);
  make_num_zero!();
  assert_eq!(get_num!(), 0);

  #[helper_fn(&num: i32)]
  fn get_num() -> i32 {
    *num
  }

  #[helper_fn(&mut num: i32)]
  fn _decrement_num() {
    *num -= 1;
  }

  #[helper_fn(&mut num: i32)]
  fn make_num_zero() {
    use_helper_fn!(_decrement_num(&mut num) as decrement_num);
    #[helper_fn(&mut num: i32)]
    fn increment_num() {
      *num += 1;
    }
    if *num > 0 {
      decrement_num!();
      make_num_zero!();
    } else if *num < 0 {
      increment_num!();
      make_num_zero!();
    }
  }
}
