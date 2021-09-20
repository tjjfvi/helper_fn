use helper_fn::helper_fn;

#[test]
fn foo() {
  let mut num = 5;

  #[helper_fn]
  fn increment_num() {
    num += 1;
  }

  #[helper_fn]
  fn get_num() {
    num
  }

  assert_eq!(get_num!(), 5);
  increment_num!();
  assert_eq!(get_num!(), 6);
}
