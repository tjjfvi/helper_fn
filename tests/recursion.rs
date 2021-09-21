use helper_fn::helper_fn;

#[test]
fn foo() {
  let mut num = 5;

  #[helper_fn(&num: i32)]
  fn get_num() -> i32 {
    *num
  }

  #[helper_fn(&mut num: i32)]
  fn make_num_zero() {
    if *num > 0 {
      *num -= 1;
      make_num_zero!();
    } else if *num < 0 {
      *num += 1;
      make_num_zero!();
    }
  }

  assert_eq!(get_num!(), 5);
  make_num_zero!();
  assert_eq!(get_num!(), 0);
  num = -5;
  assert_eq!(get_num!(), -5);
  make_num_zero!();
  assert_eq!(get_num!(), 0);
}
