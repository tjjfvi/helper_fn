#![doc=include_str!("../README.md")]

use proc_macro::{self, TokenStream};
use proc_macro_error::{abort, proc_macro_error};
use quote::quote;
use syn::{parse_macro_input, ItemFn, Visibility};

/// See the [crate level documentation](index.html) for more details.
#[proc_macro_attribute]
#[proc_macro_error]
pub fn helper_fn(_attr: TokenStream, item: TokenStream) -> TokenStream {
  let item = parse_macro_input!(item as ItemFn);
  if item.vis != Visibility::Inherited {
    abort!(item.vis, "unexpected visibility specifier");
  }
  if let Some(x) = item.sig.constness {
    abort!(x, "helper function cannot be marked const")
  }
  if let Some(x) = item.sig.asyncness {
    abort!(x, "helper function cannot be marked async")
  }
  if let Some(x) = item.sig.unsafety {
    abort!(x, "helper function cannot be marked unsafe")
  }
  if let Some(x) = item.sig.abi {
    abort!(x, "helper function cannot be marked extern")
  }
  if let Some(x) = item.sig.variadic {
    abort!(x, "helper function cannot be variadic")
  }
  if item.sig.generics.params.len() != 0 || item.sig.generics.where_clause.is_some() {
    abort!(item.sig.generics, "helper function cannot have generics")
  }
  let inputs = item.sig.inputs.iter();
  let block = &item.block;
  let output = &item.sig.output;
  let name = &item.sig.ident;
  (quote! {
    macro_rules! #name {
      ($($arg:expr),* $(,)?) => {
        (|#(#inputs),*| #output #block)($($arg),*)
      }
    }
  })
  .into()
}
