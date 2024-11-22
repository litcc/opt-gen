use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use quote::ToTokens;
use syn::parse_macro_input;
use syn::parse_quote;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::Attribute;
use syn::Data;
use syn::DeriveInput;
use syn::Error;
use syn::Expr;
use syn::Fields;
use syn::Meta;
use syn::Result;
use syn::Token;

use crate::utils::SaltStrGenerator;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum OptType {
    Raw,
    Cow,
}

pub struct OptGenStructParse {
    /// 生成类型
    pub opt_type: OptType,
    /// 原始结构体名称
    pub struct_name: Ident,
    /// 结构体宏 (排除 opt_gen 以及相关的宏)
    pub attrs: Vec<Attribute>,
    /// 结构体字段
    pub fields: Vec<FieldParse>,
    /// 生命周期
    pub lifetime: Vec<FieldParse>,
}

pub struct FieldParse {
    /// 生成类型
    pub opt_type: OptType,
    /// 字段名称
    pub field_name: Ident,
    /// 字段类型
    pub field_type: syn::Type,
    /// 字段存在的属性宏 (排除与当前库相关的属性宏)
    pub attrs: Vec<Attribute>,
}

impl OptGenStructParse {
    pub fn parse(input: DeriveInput) -> Result<Self> {
        todo!()
    }
}

pub(crate) fn opt_gen(p0: TokenStream, opt_type: OptType) -> TokenStream {
    let input = parse_macro_input!(p0 as DeriveInput);
    let struct_name = &input.ident;
    let mut has_lifetime = opt_type == OptType::Cow;

    let token = match input.data {
        Data::Struct(ref data_struct) => {
            if let Fields::Named(ref fields_named) = data_struct.fields {
                let (field_name_list, _field_type_list): (Vec<_>, Vec<_>) =
                    fields_named
                        .named
                        .iter()
                        .map(|field| (field.ident.as_ref().unwrap(), &field.ty))
                        .unzip();

                let mut create_fn_list = Vec::new();
                let change_fields = fields_named.named.iter()
                    .map(|field| {
                        let field_name = field.ident.as_ref().unwrap();
                        let field_type = &field.ty;
                        let data = get_opt_type(&field.attrs, opt_type);
                        let change_type = match data {
                            OptType::Raw => quote! { Option<#field_type> },
                            OptType::Cow => {
                                has_lifetime = true;
                                quote! { Option<std::borrow::Cow<'__opt, #field_type>> }
                            }
                        };

                        if !field.attrs.is_empty() {
                            let attrs = &field.attrs;
                            if let Some((new_attrs, fn_list)) = auto_wrap_serde_with_fn(data, attrs, &change_type)? {
                                create_fn_list.extend(fn_list);
                                Ok((data, quote! { #( #new_attrs )* pub #field_name: #change_type }))
                            } else {
                                Ok((data, quote! { #( #attrs )* pub #field_name: #change_type }))
                            }
                        } else {
                            Ok((data, quote! { pub #field_name: #change_type }))
                        }
                    })
                    .collect::<Result<Vec<(_, _)>>>();

                if change_fields.is_err() {
                    return change_fields.err().unwrap().to_compile_error().into();
                }

                let change_fields = change_fields.unwrap();
                let change_struct = quote::format_ident!("{}Ref", struct_name);
                let struct_attrs = &input.attrs;
                let change_struct_all = if has_lifetime {
                    quote! { #change_struct <'__opt> }
                } else {
                    quote! { #change_struct }
                };

                let change_fields2 =
                    change_fields.iter().map(|i| &i.1).collect::<Vec<_>>();
                let fields_info = change_fields
                    .iter()
                    .map(|i| &i.0)
                    .zip(field_name_list.iter())
                    .collect::<Vec<(_, _)>>();
                let other_code = impl_struct_gen(
                    opt_type,
                    has_lifetime,
                    struct_name,
                    &change_struct,
                    &fields_info,
                );

                // let struct_attrs = quote! { #[derive(Debug, Clone,
                // serde::Serialize, serde::Deserialize)] };
                let struct_attrs = quote! { #( #struct_attrs )* };

                // eprintln!("struct_attrs: {}", struct_attrs.to_string());

                let ref_body = quote! {
                    #( #create_fn_list )*

                    #struct_attrs
                    pub struct #change_struct_all {
                        #( #change_fields2, )*
                    }

                    #other_code
                };
                Ok(ref_body)
            } else {
                Err(vec![Error::new(
                    struct_name.span(),
                    "Sorry, RefModel does not support complex structures",
                )])
            }
        }
        _ => Err(vec![Error::new(
            struct_name.span(),
            "Sorry, RefModel only supports Struct.",
        )]),
    };

    token.expect("Invalid Rust code provided").to_token_stream().into()
}

fn get_opt_type(attrs: &[Attribute], default: OptType) -> OptType {
    attrs
        .iter()
        .filter_map(|i| {
            if i.path().is_ident("opt_gen") {
                let mut opt = None;
                let _ = i.parse_nested_meta(|meta| {
                    if meta.path.is_ident("Raw") {
                        opt = Some(OptType::Raw);
                        return Err(Error::new(meta.path.span(), "is Ok"));
                    }
                    if meta.path.is_ident("Cow") {
                        opt = Some(OptType::Cow);
                        return Err(Error::new(meta.path.span(), "is Ok"));
                    }
                    Ok(())
                });
                opt
            } else {
                None
            }
        })
        .next()
        .unwrap_or(default)
}

pub type AttributeChange = Option<(Vec<Attribute>, Vec<proc_macro2::TokenStream>)>;

pub(crate) fn auto_wrap_serde_with_fn(
    opt_type: OptType,
    list: &[Attribute],
    new_type: &proc_macro2::TokenStream,
) -> Result<AttributeChange> {
    let mut code_all = Vec::new();
    let mut new_attr_list = Vec::new();

    for mut attr in list.to_vec() {
        if attr.path().is_ident("opt_gen") {
            continue;
        }

        if attr.path().is_ident("serde") {
            let mut meta_list = Vec::new();
            let nested = attr
                .parse_args_with(Punctuated::<Meta, Token![,]>::parse_terminated)?;

            for meta in nested {
                match meta {
                    Meta::NameValue(meta)
                        if meta.path.is_ident("deserialize_with") =>
                    {
                        let random_string = rand::thread_rng().gen_salt(10);
                        let new_fn_name = format!(
                            "__serde_deserialize_with_wrap_{}",
                            random_string
                        );
                        let new_fn_ident = syn::Ident::new(
                            &new_fn_name,
                            proc_macro2::Span::call_site(),
                        );

                        let fn_name = if let Expr::Lit(ref base) = meta.value {
                            if let syn::Lit::Str(ref s) = base.lit {
                                Some(syn::Ident::new(
                                    &s.value(),
                                    proc_macro2::Span::call_site(),
                                ))
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                        .ok_or(Error::new(
                            meta.value.span(),
                            "deserialize_with value must be a string",
                        ))?;

                        let g = match opt_type {
                            OptType::Raw => quote! { <'de, D> },
                            OptType::Cow => quote! { <'de, '__opt, D> },
                        };

                        let return_data = match opt_type {
                            OptType::Raw => quote! { Ok(Some(value)) },
                            OptType::Cow => {
                                quote! { Ok(Some(std::borrow::Cow::<'__opt>::Owned(value))) }
                            }
                        };

                        let code = quote! {
                            #[inline(always)]
                            #[automatically_derived]
                            fn #new_fn_ident #g (deserializer: D) -> Result<#new_type, D::Error>
                            where
                                D: serde::Deserializer<'de>,
                            {
                                let value = #fn_name ::<'de,D>(deserializer)?;
                                #return_data
                            }
                        };

                        meta_list
                            .push(parse_quote!(deserialize_with = #new_fn_name));
                        code_all.push(code);
                    }
                    _ => meta_list.push(meta),
                }
            }
            attr.meta = syn::parse_quote!(serde( #( #meta_list ),* ));
            new_attr_list.push(attr);
            continue;
        }

        new_attr_list.push(attr);
    }

    if code_all.is_empty() {
        return Ok(None);
    }

    Ok(Some((new_attr_list, code_all)))
}

pub(crate) fn impl_struct_gen(
    _opt_type: OptType,
    has_lifetime: bool,
    struct_name: &Ident,
    change_struct: &Ident,
    field_info: &Vec<(&OptType, &&Ident)>,
) -> proc_macro2::TokenStream {
    let field_list = field_info.iter().map(|i| i.1).collect::<Vec<_>>();

    let generics = if has_lifetime {
        quote! { <'__opt> }
    } else {
        quote! {}
    };

    let field_item_from = field_info.iter().map(|(opt_type_field, field_name)| {
        match opt_type_field {
            OptType::Raw => quote! { #field_name: Some(value.#field_name) },
            OptType::Cow => quote! { #field_name: Some(std::borrow::Cow::Owned(value.#field_name)) },
        }
    }).collect::<Vec<_>>();

    let field_item_from = quote! { #( #field_item_from ),* };

    let from_gen = quote! {

        impl #generics From<#struct_name> for #change_struct #generics {
            fn from(value: #struct_name) -> Self {
                #change_struct {
                    #field_item_from
                }
            }
        }
    };

    quote! {

        impl #struct_name {
            pub fn all_fields() -> &'static [&'static str] {
                static FIELDS: &'static [&'static str] = &[
                    #( stringify!(#field_list) ),*
                ];
                FIELDS
            }

            pub fn has_field(field_name: &str) -> bool {
                Self::all_fields().contains(&field_name)
            }
        }


        impl #generics #change_struct #generics {
            pub fn all_empty(&self) -> bool {
                return !(#( self.#field_list.is_some() )||*)
            }

            pub fn has_field(field_name: &str) -> bool {
                #struct_name::all_fields().contains(&field_name)
            }

            pub fn all_fields() -> &'static [&'static str] {
                #struct_name::all_fields()
            }
        }

        #from_gen


        impl #generics Default for #change_struct #generics {
            fn default() -> Self {
                #change_struct {
                    #( #field_list: None ),*
                }
            }
        }
    }
}
