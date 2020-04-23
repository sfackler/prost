#![doc(html_root_url = "https://docs.rs/prost-derive/0.6.1")]
// The `quote!` macro requires deep recursion.
#![recursion_limit = "4096"]

extern crate proc_macro;

use anyhow::bail;
use quote::quote;

use anyhow::Error;
use itertools::Itertools;
use proc_macro::TokenStream;
use proc_macro2::Span;
use syn::{
    punctuated::Punctuated, Data, DataEnum, DataStruct, DeriveInput, Fields, FieldsNamed,
    FieldsUnnamed, Ident, ImplItem, ItemImpl, Variant,
};

mod field;
use crate::field::Field;

fn try_message(input: TokenStream) -> Result<TokenStream, Error> {
    let input: DeriveInput = syn::parse(input)?;

    let ident = input.ident;

    let variant_data = match input.data {
        Data::Struct(variant_data) => variant_data,
        Data::Enum(..) => bail!("Message can not be derived for an enum"),
        Data::Union(..) => bail!("Message can not be derived for a union"),
    };

    if !input.generics.params.is_empty() || input.generics.where_clause.is_some() {
        bail!("Message may not be derived for generic type");
    }

    let fields = match variant_data {
        DataStruct {
            fields: Fields::Named(FieldsNamed { named: fields, .. }),
            ..
        }
        | DataStruct {
            fields:
                Fields::Unnamed(FieldsUnnamed {
                    unnamed: fields, ..
                }),
            ..
        } => fields.into_iter().collect(),
        DataStruct {
            fields: Fields::Unit,
            ..
        } => Vec::new(),
    };

    let mut next_tag: u32 = 1;
    let mut fields = fields
        .into_iter()
        .enumerate()
        .flat_map(|(idx, field)| {
            let field_ident = field
                .ident
                .unwrap_or_else(|| Ident::new(&idx.to_string(), Span::call_site()));
            match Field::new(field.attrs, Some(next_tag)) {
                Ok(Some(field)) => {
                    next_tag = field.tags().iter().max().map(|t| t + 1).unwrap_or(next_tag);
                    Some(Ok((field_ident, field)))
                }
                Ok(None) => None,
                Err(err) => Some(Err(
                    err.context(format!("invalid message field {}.{}", ident, field_ident))
                )),
            }
        })
        .collect::<Result<Vec<_>, _>>()?;

    // We want Debug to be in declaration order
    let unsorted_fields = fields.clone();

    // Sort the fields by tag number so that fields will be encoded in tag order.
    // TODO: This encodes oneof fields in the position of their lowest tag,
    // regardless of the currently occupied variant, is that consequential?
    // See: https://developers.google.com/protocol-buffers/docs/encoding#order
    fields.sort_by_key(|&(_, ref field)| field.tags().into_iter().min().unwrap());
    let fields = fields;

    let mut tags = fields
        .iter()
        .flat_map(|&(_, ref field)| field.tags())
        .collect::<Vec<_>>();
    let num_tags = tags.len();
    tags.sort();
    tags.dedup();
    if tags.len() != num_tags {
        bail!("message {} has fields with duplicate tags", ident);
    }

    let encoded_len = fields
        .iter()
        .map(|&(ref field_ident, ref field)| field.encoded_len(quote!(self.#field_ident)));

    let encode = fields
        .iter()
        .map(|&(ref field_ident, ref field)| field.encode(quote!(self.#field_ident)));

    let merge = fields.iter().map(|&(ref field_ident, ref field)| {
        let merge = field.merge(quote!(value));
        let tags = field
            .tags()
            .into_iter()
            .map(|tag| quote!(#tag))
            .intersperse(quote!(|));
        quote! {
            #(#tags)* => {
                let mut value = &mut self.#field_ident;
                #merge.map_err(|mut error| {
                    error.push(STRUCT_NAME, stringify!(#field_ident));
                    error
                })
            },
        }
    });

    let struct_name = if fields.is_empty() {
        quote!()
    } else {
        quote!(
            const STRUCT_NAME: &'static str = stringify!(#ident);
        )
    };

    // TODO
    let is_struct = true;

    let clear = fields
        .iter()
        .map(|&(ref field_ident, ref field)| field.clear(quote!(self.#field_ident)));

    let default = fields.iter().map(|&(ref field_ident, ref field)| {
        let value = field.default();
        quote!(#field_ident: #value,)
    });

    let methods = fields
        .iter()
        .flat_map(|&(ref field_ident, ref field)| field.methods(field_ident))
        .collect::<Vec<_>>();
    let methods = if methods.is_empty() {
        quote!()
    } else {
        quote! {
            #[allow(dead_code)]
            impl #ident {
                #(#methods)*
            }
        }
    };

    let debugs = unsorted_fields.iter().map(|&(ref field_ident, ref field)| {
        let wrapper = field.debug(quote!(self.#field_ident));
        let call = if is_struct {
            quote!(builder.field(stringify!(#field_ident), &wrapper))
        } else {
            quote!(builder.field(&wrapper))
        };
        quote! {
             let builder = {
                 let wrapper = #wrapper;
                 #call
             };
        }
    });
    let debug_builder = if is_struct {
        quote!(f.debug_struct(stringify!(#ident)))
    } else {
        quote!(f.debug_tuple(stringify!(#ident)))
    };

    let expanded = quote! {
        impl ::prost::Message for #ident {
            #[allow(unused_variables)]
            fn encode_raw<B>(&self, buf: &mut B) where B: ::prost::bytes::BufMut {
                #(#encode)*
            }

            #[allow(unused_variables)]
            fn merge_field<B>(
                &mut self,
                tag: u32,
                wire_type: ::prost::encoding::WireType,
                buf: &mut B,
                ctx: ::prost::encoding::DecodeContext,
            ) -> ::std::result::Result<(), ::prost::DecodeError>
            where B: ::prost::bytes::Buf {
                #struct_name
                match tag {
                    #(#merge)*
                    _ => ::prost::encoding::skip_field(wire_type, tag, buf, ctx),
                }
            }

            #[inline]
            fn encoded_len(&self) -> usize {
                0 #(+ #encoded_len)*
            }

            fn clear(&mut self) {
                #(#clear;)*
            }
        }

        impl Default for #ident {
            fn default() -> #ident {
                #ident {
                    #(#default)*
                }
            }
        }

        impl ::std::fmt::Debug for #ident {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                let mut builder = #debug_builder;
                #(#debugs;)*
                builder.finish()
            }
        }

        #methods
    };

    Ok(expanded.into())
}

fn try_enumeration(_attr: TokenStream, input: TokenStream) -> Result<TokenStream, Error> {
    let mut impl_: ItemImpl = syn::parse(input)?;

    if !impl_.generics.params.is_empty() || impl_.generics.where_clause.is_some() {
        bail!("enumeration may not be applied to generic types");
    }

    if impl_.trait_.is_some() {
        bail!("enumeration may not be applied to trait impls");
    }

    let mut variants = Vec::new();
    for item in &impl_.items {
        let const_ = match item {
            ImplItem::Const(const_) => const_.ident.clone(),
            _ => bail!("enumeration may only be applied to impls with only consts"),
        };
        variants.push(const_);
    }

    if variants.is_empty() {
        bail!("enumeration must be applied to impls with consts");
    }

    let ty = &impl_.self_ty;
    let is_valid = quote! {
        /// Returns true if the enum's value corresponds to a known variant.
        #[inline]
        pub fn is_valid(&self) -> bool {
            match self {
                #(#ty::#variants)|* => true,
                _ => false,
            }
        }
    };
    impl_
        .items
        .push(ImplItem::Method(syn::parse(is_valid.into()).unwrap()));

    let default = &variants[0];
    let ty = &impl_.self_ty;

    let expanded = quote! {
        #impl_

        impl ::std::default::Default for #ty {
            #[inline]
            fn default() -> #ty {
                #ty::#default
            }
        }

        impl ::std::convert::From<i32> for #ty {
            #[inline]
            fn from(value: i32) -> #ty {
                #ty(value)
            }
        }

        impl ::std::convert::From<#ty> for i32 {
            #[inline]
            fn from(value: #ty) -> i32 {
                value.0
            }
        }
    };

    Ok(expanded.into())
}

#[proc_macro_attribute]
pub fn enumeration(attr: TokenStream, input: TokenStream) -> TokenStream {
    try_enumeration(attr, input).unwrap()
}

fn try_oneof(input: TokenStream) -> Result<TokenStream, Error> {
    let input: DeriveInput = syn::parse(input)?;

    let ident = input.ident;

    let variants = match input.data {
        Data::Enum(DataEnum { variants, .. }) => variants,
        Data::Struct(..) => bail!("Oneof can not be derived for a struct"),
        Data::Union(..) => bail!("Oneof can not be derived for a union"),
    };

    if !input.generics.params.is_empty() || input.generics.where_clause.is_some() {
        bail!("Message may not be derived for generic type");
    }

    // Map the variants into 'fields'.
    let mut fields: Vec<(Ident, Field)> = Vec::new();
    for Variant {
        attrs,
        ident: variant_ident,
        fields: variant_fields,
        ..
    } in variants
    {
        let variant_fields = match variant_fields {
            Fields::Unit => Punctuated::new(),
            Fields::Named(FieldsNamed { named: fields, .. })
            | Fields::Unnamed(FieldsUnnamed {
                unnamed: fields, ..
            }) => fields,
        };
        if variant_fields.len() != 1 {
            bail!("Oneof enum variants must have a single field");
        }
        match Field::new_oneof(attrs)? {
            Some(field) => fields.push((variant_ident, field)),
            None => bail!("invalid oneof variant: oneof variants may not be ignored"),
        }
    }

    let mut tags = fields
        .iter()
        .flat_map(|&(ref variant_ident, ref field)| -> Result<u32, Error> {
            if field.tags().len() > 1 {
                bail!(
                    "invalid oneof variant {}::{}: oneof variants may only have a single tag",
                    ident,
                    variant_ident
                );
            }
            Ok(field.tags()[0])
        })
        .collect::<Vec<_>>();
    tags.sort();
    tags.dedup();
    if tags.len() != fields.len() {
        panic!("invalid oneof {}: variants have duplicate tags", ident);
    }

    let encode = fields.iter().map(|&(ref variant_ident, ref field)| {
        let encode = field.encode(quote!(*value));
        quote!(#ident::#variant_ident(ref value) => { #encode })
    });

    let merge = fields.iter().map(|&(ref variant_ident, ref field)| {
        let tag = field.tags()[0];
        let merge = field.merge(quote!(value));
        quote! {
            #tag => {
                match field {
                    ::std::option::Option::Some(#ident::#variant_ident(ref mut value)) => {
                        #merge
                    },
                    _ => {
                        let mut owned_value = ::std::default::Default::default();
                        let value = &mut owned_value;
                        #merge.map(|_| *field = ::std::option::Option::Some(#ident::#variant_ident(owned_value)))
                    },
                }
            }
        }
    });

    let encoded_len = fields.iter().map(|&(ref variant_ident, ref field)| {
        let encoded_len = field.encoded_len(quote!(*value));
        quote!(#ident::#variant_ident(ref value) => #encoded_len)
    });

    let debug = fields.iter().map(|&(ref variant_ident, ref field)| {
        let wrapper = field.debug(quote!(*value));
        quote!(#ident::#variant_ident(ref value) => {
            let wrapper = #wrapper;
            f.debug_tuple(stringify!(#variant_ident))
                .field(&wrapper)
                .finish()
        })
    });

    let expanded = quote! {
        impl #ident {
            pub fn encode<B>(&self, buf: &mut B) where B: ::prost::bytes::BufMut {
                match *self {
                    #(#encode,)*
                }
            }

            pub fn merge<B>(
                field: &mut ::std::option::Option<#ident>,
                tag: u32,
                wire_type: ::prost::encoding::WireType,
                buf: &mut B,
                ctx: ::prost::encoding::DecodeContext,
            ) -> ::std::result::Result<(), ::prost::DecodeError>
            where B: ::prost::bytes::Buf {
                match tag {
                    #(#merge,)*
                    _ => unreachable!(concat!("invalid ", stringify!(#ident), " tag: {}"), tag),
                }
            }

            #[inline]
            pub fn encoded_len(&self) -> usize {
                match *self {
                    #(#encoded_len,)*
                }
            }
        }

        impl ::std::fmt::Debug for #ident {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                match *self {
                    #(#debug,)*
                }
            }
        }
    };

    Ok(expanded.into())
}

#[proc_macro_derive(Oneof, attributes(prost))]
pub fn oneof(input: TokenStream) -> TokenStream {
    try_oneof(input).unwrap()
}
