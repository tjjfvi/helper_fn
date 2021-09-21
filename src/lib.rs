#![doc=include_str!("../README.md")]

use proc_macro::{self, TokenStream};
use proc_macro_error::{abort, proc_macro_error};
use quote::quote;
use syn::{
  parse::Parse, parse_macro_input, punctuated::Punctuated, Ident, ItemFn, Token, Type, Visibility,
};

/// See the [crate level documentation](index.html) for more details.
#[proc_macro_attribute]
#[proc_macro_error]
pub fn helper_fn(attr: TokenStream, item: TokenStream) -> TokenStream {
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
  if attr.is_empty() {
    (quote! {
      macro_rules! #name {
        ($($arg:expr),* $(,)?) => {
          (|#(#inputs),*| #output #block)($($arg),*)
        }
      }
    })
    .into()
  } else {
    let attr_args = parse_macro_input!(attr as CaptureArgs);
    let capture_params = attr_args.0.iter().map(|arg| {
      let param = &arg.ident;
      let ref_ty = match &arg.kind {
        CaptureArgKind::Value => quote! {},
        CaptureArgKind::Ref(..) => quote! { & },
        CaptureArgKind::RefMut(..) => quote! { &mut },
      };
      let ty = &arg.ty;
      quote! { #param: #ref_ty (#ty) }
    });
    let capture_args = attr_args.0.iter().map(|arg| {
      let ident = &arg.ident;
      match &arg.kind {
        CaptureArgKind::Value => quote! { #ident },
        CaptureArgKind::Ref(..) => quote! {{
          trait AutoRef {
            fn auto_ref(&self) -> &Self {
              self
            }
          }
          impl<T> AutoRef for T {}
          #ident.auto_ref()
        }},
        CaptureArgKind::RefMut(..) => quote! {{
          trait AutoRefMut {
            fn auto_ref_mut(&mut self) -> &mut Self {
              self
            }
          }
          impl<T> AutoRefMut for T {}
          #ident.auto_ref_mut()
        }},
      }
    });
    let macro_def = quote! {
      macro_rules! #name {
        ($($arg:expr),* $(,)?) => {
          #name(#(#capture_args),*, $($arg),*)
        }
      }
    };
    (quote! {
      fn #name(#(#capture_params),*, #(#inputs),*) #output {
        #macro_def
        #block
      }
      #macro_def
    })
    .into()
  }
}

#[derive(Debug)]
struct CaptureArgs(Punctuated<CaptureArg, Token![,]>);

impl Parse for CaptureArgs {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    Ok(CaptureArgs(input.parse_terminated(<_>::parse)?))
  }
}

#[derive(Debug)]
struct CaptureArg {
  kind: CaptureArgKind,
  ident: Ident,
  colon: Token![:],
  ty: Type,
}

impl Parse for CaptureArg {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    Ok(CaptureArg {
      kind: input.parse()?,
      ident: input.parse()?,
      colon: input.parse()?,
      ty: input.parse()?,
    })
  }
}

#[derive(Debug)]
enum CaptureArgKind {
  Value,
  Ref(Token![&]),
  RefMut(Token![&], Token![mut]),
}

impl Parse for CaptureArgKind {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    let lookahead = input.lookahead1();
    if lookahead.peek(Token![&]) {
      let ref_token: Token![&] = input.parse()?;
      let lookahead = input.lookahead1();
      if lookahead.peek(Token![mut]) {
        Ok(CaptureArgKind::RefMut(ref_token, input.parse()?))
      } else {
        Ok(CaptureArgKind::Ref(ref_token))
      }
    } else {
      Ok(CaptureArgKind::Value)
    }
  }
}
