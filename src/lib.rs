#![doc=include_str!("../README.md")]

use proc_macro::{self, TokenStream};
use proc_macro2::{Ident as Ident2, Span as Span2, TokenStream as TokenStream2};
use proc_macro_error::{abort, proc_macro_error};
use quote::{quote, ToTokens};
use syn::{
  parenthesized, parse::Parse, parse_macro_input, punctuated::Punctuated, token, ItemFn, Token,
  Type, Visibility,
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
  let fn_name = create_fn_name(name);
  if !attr.is_empty() {
    let captures = parse_macro_input!(attr as CaptureArgsWithTypes);
    let capture_params = captures.0.iter().map(|x| {
      let ident = &x.capture.ident;
      let kind = &x.capture.kind;
      let ty = &x.ty;
      quote! { #ident: #kind #ty }
    });
    let macro_def = create_macro_def(name, &fn_name, captures.0.iter().map(|x| &x.capture));
    (quote! {
      fn #fn_name(#(#capture_params),*, #(#inputs),*) #output {
        #macro_def
        #block
      }
      #macro_def
    })
    .into()
  } else {
    (quote! {
      macro_rules! #name {
        ($($arg:expr),* $(,)?) => {
          (|#(#inputs),*| #output #block)($($arg),*)
        }
      }
    })
    .into()
  }
}

#[proc_macro]
pub fn use_helper_fn(args: TokenStream) -> TokenStream {
  let args = parse_macro_input!(args as UseHelperFnArgs);

  return args
    .0
    .into_iter()
    .map(|arg| -> TokenStream {
      create_macro_def(
        arg
          .alias
          .as_ref()
          .map(|x| &x.alias)
          .unwrap_or_else(|| &arg.name),
        &create_fn_name(&arg.name),
        arg.captures.0.iter(),
      )
      .into()
    })
    .flatten()
    .collect();

  struct UseHelperFnArgs(Punctuated<UseHelperFnArg, Token![,]>);

  impl Parse for UseHelperFnArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
      Ok(UseHelperFnArgs(input.parse_terminated(<_>::parse)?))
    }
  }

  struct UseHelperFnArg {
    name: syn::Ident,
    _paren: token::Paren,
    captures: CaptureArgs,
    alias: Option<UseHelperFnAlias>,
  }

  struct UseHelperFnAlias {
    _as_token: Token![as],
    alias: syn::Ident,
  }

  impl Parse for UseHelperFnArg {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
      let captures;
      Ok(UseHelperFnArg {
        name: input.parse()?,
        _paren: parenthesized!(captures in input),
        captures: captures.parse()?,
        alias: input.parse().ok(),
      })
    }
  }
  impl Parse for UseHelperFnAlias {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
      Ok(UseHelperFnAlias {
        _as_token: input.parse()?,
        alias: input.parse()?,
      })
    }
  }
}

#[derive(Debug)]
struct CaptureArgsWithTypes(Punctuated<CaptureArgWithType, Token![,]>);

impl Parse for CaptureArgsWithTypes {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    Ok(CaptureArgsWithTypes(input.parse_terminated(<_>::parse)?))
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
  ident: syn::Ident,
}

impl Parse for CaptureArg {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    Ok(CaptureArg {
      kind: input.parse()?,
      ident: input.parse()?,
    })
  }
}

#[derive(Debug)]
struct CaptureArgWithType {
  capture: CaptureArg,
  colon: Token![:],
  ty: Type,
}

impl Parse for CaptureArgWithType {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    Ok(CaptureArgWithType {
      capture: input.parse()?,
      colon: input.parse()?,
      ty: input.parse()?,
    })
  }
}

#[derive(Debug, Clone)]
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

impl ToTokens for CaptureArgKind {
  fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
    match self {
      CaptureArgKind::Value => {}
      CaptureArgKind::Ref(..) => tokens.extend(quote!(&)),
      CaptureArgKind::RefMut(..) => tokens.extend(quote!(&mut)),
    }
  }
}

fn create_fn_name(name: &Ident2) -> Ident2 {
  Ident2::new(&format!("__helper_fn_{}", name)[..], Span2::call_site())
}

fn create_macro_def<'a>(
  name: &Ident2,
  fn_name: &Ident2,
  args: impl Iterator<Item = &'a CaptureArg>,
) -> TokenStream2 {
  let capture_args = args.map(|arg| {
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
  quote! {
    macro_rules! #name {
      ($($arg:expr),* $(,)?) => {
        #fn_name(#(#capture_args),*, $($arg),*)
      }
    }
  }
}
