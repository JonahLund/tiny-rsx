use syn::{
    braced,
    ext::IdentExt as _,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token, Ident, LitStr, Result, Token,
};

use crate::ast;

mod kw {
    use syn::custom_keyword;

    custom_keyword!(DOCTYPE);
    custom_keyword!(html);
}

/// Parses the input tokens
pub fn parse(input: ParseStream, strict: bool) -> Result<Box<[ast::Node]>> {
    let mut nodes = Vec::new();
    let mut stack = Vec::new();

    while !input.is_empty() {
        let curr: ast::Node = input.parse()?;

        match (&curr, stack.pop()) {
            // current and stack node are both tags
            (ast::Node::Tag(curr), Some(other)) => {
                match (curr, &other) {
                    // invalid, inverse order
                    (ast::Tag::Opening { .. }, ast::Tag::Closing { .. }) => {
                        panic!("opening closing")
                    }
                    // invalid
                    (ast::Tag::Closing { .. }, ast::Tag::Closing { .. }) => {
                        panic!("closing closing")
                    }
                    // valid
                    (ast::Tag::Opening { .. }, ast::Tag::Opening { .. }) => {
                        panic!("inverse order")
                    }
                    // valid
                    (
                        ast::Tag::Closing { name },
                        ast::Tag::Opening {
                            name: other_name, ..
                        },
                    ) => {
                        if name != other_name {
                            stack.push(ast::Node::Tag(other));
                        } else {
                        }
                    }
                }
            }
            _ => {}
        }
    }

    Ok(nodes.into_boxed_slice())
}

impl Parse for ast::DashIdent {
    fn parse(input: ParseStream) -> Result<Self> {
        // Parse a non-empty sequence of identifiers separated by dashes.
        let inner =
            Punctuated::<Ident, Token![-]>::parse_separated_nonempty_with(
                input,
                Ident::parse_any,
            )?;

        Ok(ast::DashIdent(inner))
    }
}

impl Parse for ast::Doctype {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![<]>()?;
        input.parse::<Token![!]>()?;
        input.parse::<kw::DOCTYPE>()?;
        input.parse::<kw::html>()?;
        input.parse::<Token![>]>()?;

        Ok(ast::Doctype)
    }
}

impl Parse for ast::Value {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(LitStr) {
            Ok(ast::Value::LitStr(input.parse()?))
        } else if lookahead.peek(token::Brace) {
            let content;
            braced!(content in input);
            Ok(ast::Value::Expr(content.parse()?))
        } else {
            Err(lookahead.error())
        }
    }
}

impl Parse for ast::Attr {
    fn parse(input: ParseStream) -> Result<Self> {
        let key = input.parse()?;
        input.parse::<Token![=]>()?;
        let value = input.parse()?;

        Ok(ast::Attr { key, value })
    }
}

impl Parse for ast::Tag {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![<]>()?;

        if input.parse::<Option<Token![/]>>()?.is_some() {
            let name = input.parse()?;
            input.parse::<Token![>]>()?;

            return Ok(ast::Tag::Closing { name });
        }

        let name = input.parse()?;

        let mut attrs = Vec::new();
        while !(input.peek(Token![>])
            || (input.peek(Token![/]) && input.peek2(Token![>])))
        {
            attrs.push(input.parse()?);
        }

        let void_slash = input.parse()?;
        input.parse::<Token![>]>()?;

        Ok(ast::Tag::Opening {
            name,
            attrs,
            void_slash,
        })
    }
}

impl Parse for ast::Node {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![<])
            && input.peek2(Token![!])
            && input.peek3(kw::DOCTYPE)
        {
            Ok(ast::Node::Doctype(input.parse()?))
        } else if lookahead.peek(Token![<]) {
            Ok(ast::Node::Tag(input.parse()?))
        } else if lookahead.peek(LitStr) || lookahead.peek(token::Brace) {
            Ok(ast::Node::Value(input.parse()?))
        } else {
            Err(lookahead.error())
        }
    }
}

#[cfg(test)]
mod tests {
    use proc_macro2::Span;
    use quote::quote;

    use super::*;

    macro_rules! dash_ident {
        ($($tt:tt)*) => {
            syn::parse2::<ast::DashIdent>(quote!($($tt)*)).unwrap()
        };
    }

    macro_rules! lit_str {
        ($lit:literal) => {
            syn::LitStr::new($lit, Span::call_site())
        };
    }

    macro_rules! lit_bool {
        ($lit:literal) => {
            syn::LitBool::new($lit, Span::call_site())
        };
    }

    #[test]
    fn parses_to_doctype() {
        assert_eq!(
            syn::parse2::<ast::Doctype>(quote!(<!DOCTYPE html>)).unwrap(),
            ast::Doctype,
        )
    }

    #[test]
    fn parses_to_string_value() {
        assert_eq!(
            syn::parse2::<ast::Value>(quote!("foo")).unwrap(),
            ast::Value::LitStr(lit_str!("foo")),
        );
        assert_eq!(
            syn::parse2::<ast::Value>(quote!("")).unwrap(),
            ast::Value::LitStr(lit_str!("")),
        );
    }

    #[test]
    fn parses_to_expr_value() {
        assert_eq!(
            syn::parse2::<ast::Value>(quote!({ true })).unwrap(),
            ast::Value::Expr(syn::Expr::Lit(syn::ExprLit {
                attrs: vec![],
                lit: syn::Lit::Bool(lit_bool!(true))
            })),
        );
    }

    #[test]
    fn parses_to_opening_tag() {
        assert_eq!(
            syn::parse2::<ast::Tag>(quote! ( <foo> )).unwrap(),
            ast::Tag::Opening {
                name: dash_ident!(foo),
                attrs: vec![],
                void_slash: None
            }
        )
    }

    #[test]
    fn parses_to_opening_tag_with_attrs() {
        assert_eq!(
            syn::parse2::<ast::Tag>(quote!(<foo bar="baz" qux={false}>))
                .unwrap(),
            ast::Tag::Opening {
                name: dash_ident!(foo),
                attrs: vec![
                    ast::Attr {
                        key: dash_ident!(bar),
                        value: ast::Value::LitStr(lit_str!("baz"))
                    },
                    ast::Attr {
                        key: dash_ident!(qux),
                        value: ast::Value::Expr(syn::Expr::Lit(syn::ExprLit {
                            attrs: vec![],
                            lit: syn::Lit::Bool(lit_bool!(false))
                        }))
                    },
                ],
                void_slash: None
            }
        )
    }

    #[test]
    fn parses_to_void_tag() {
        assert_eq!(
            syn::parse2::<ast::Tag>(quote!(<foo />)).unwrap(),
            ast::Tag::Opening {
                name: dash_ident!(foo),
                attrs: vec![],
                void_slash: Some(syn::token::Slash(Span::call_site()))
            }
        )
    }
}
