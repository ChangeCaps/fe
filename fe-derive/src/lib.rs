use proc_macro2::TokenStream;
use quote::{quote, quote_spanned};
use syn::{parse_macro_input, spanned::Spanned, Data, DeriveInput, Fields};

#[proc_macro_derive(Class)]
pub fn derive_class(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;

    let (fields, field, to_variant) = fields(&input.data);

    let expanded = quote! {
        impl fe::class::Class for #name {
            fn ident() -> &'static str where Self: Sized {
                stringify!(#name)
            }

            fn fields() -> Vec<fe::compilation::Field> where Self: Sized {
                #fields
            }

            fn field_mut(&mut self, idx: usize) -> &mut fe::variant::Ref {
                #field
            }

            fn to_variant(self) -> fe::variant::Variant {
                #to_variant
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}

#[inline]
fn fields(data: &Data) -> (TokenStream, TokenStream, TokenStream) {
    match data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(fields) => {
                let field = fields.named.iter().enumerate().map(|(i, f)| {
                    let name = f.ident.clone().unwrap();

                    quote_spanned! {f.span()=>
                        #i => &mut self.#name.inner,
                    }
                });

                let to_variant = fields.named.iter().map(|f| {
                    let name = f.ident.clone().unwrap();

                    quote_spanned! {f.span()=>
                        self.#name.inner
                    }
                });

                let fields = fields.named.iter().map(|f| {
                    let name = f.ident.clone().unwrap();
                    let ty = f.ty.clone();

                    quote_spanned! {f.span()=> 
                        fe::compilation::Field {
                            ident: fe::ast::Ident::new(String::from(stringify!(#name))),
                            ty: <#ty>::ty(),
                            variant: None,
                        }
                    }
                }); 

                (
                    quote! {
                        vec![#(#fields),*]
                    },
                    quote! {
                        match idx {
                            #(#field)*
                            _ => unreachable!(),
                        }
                    },
                    quote! {
                        fe::variant::Variant::Class(vec![#(#to_variant),*])
                    }
                )
            }
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}
