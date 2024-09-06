use syn::{
    ext::IdentExt as _,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
};

use crate::ast;

mod kw {
    syn::custom_keyword!(DOCTYPE);
    syn::custom_keyword!(html);
}

pub struct Parser {}

impl Parser {
    pub fn new() -> Self {
        Self {}
    }

    pub fn parse_ident(
        &self,
        input: ParseStream,
    ) -> syn::Result<ast::DashIdent> {
        // Parse a non-empty sequence of identifiers separated by dashes.
        let inner = Punctuated::<syn::Ident, syn::Token![-]>::parse_separated_nonempty_with(
            input,
            syn::Ident::parse_any,
        )?;

        Ok(ast::DashIdent(inner))
    }

    pub fn parse_doctype(
        &self,
        input: ParseStream,
    ) -> syn::Result<ast::Doctype> {
        input.parse::<syn::Token![<]>()?;
        input.parse::<syn::Token![!]>()?;
        input.parse::<kw::DOCTYPE>()?;
        input.parse::<kw::html>()?;
        input.parse::<syn::Token![>]>()?;

        Ok(ast::Doctype)
    }

    pub fn parse_value(&self, input: ParseStream) -> syn::Result<ast::Value> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::LitStr) {
            Ok(ast::Value::LitStr(input.parse()?))
        } else if lookahead.peek(syn::token::Brace) {
            let content;
            syn::braced!(content in input);
            Ok(ast::Value::Expr(content.parse()?))
        } else {
            Err(lookahead.error())
        }
    }

    pub fn parse_attr(&self, input: ParseStream) -> syn::Result<ast::Attr> {
        let key = self.parse_ident(input)?;
        input.parse::<syn::Token![=]>()?;
        let value = self.parse_value(input)?;

        Ok(ast::Attr { key, value })
    }

    pub fn parse_tag(&self, input: ParseStream) -> syn::Result<ast::Tag> {
        input.parse::<syn::Token![<]>()?;

        if input.parse::<Option<syn::Token![/]>>()?.is_some() {
            let name = self.parse_ident(input)?;
            input.parse::<syn::Token![>]>()?;

            return Ok(ast::Tag::Closing { name });
        }

        let name = self.parse_ident(input)?;

        let mut attrs = Vec::new();
        while !(input.peek(syn::Token![>])
            || (input.peek(syn::Token![/]) && input.peek2(syn::Token![>])))
        {
            attrs.push(self.parse_attr(input)?);
        }

        let void_slash = input.parse()?;
        input.parse::<syn::Token![>]>()?;

        Ok(ast::Tag::Opening {
            name,
            attrs,
            void_slash,
        })
    }

    pub fn parse_node(&self, input: ParseStream) -> syn::Result<ast::Node> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::Token![<])
            && input.peek2(syn::Token![!])
            && input.peek3(kw::DOCTYPE)
        {
            Ok(ast::Node::Doctype(self.parse_doctype(input)?))
        } else if lookahead.peek(syn::Token![<]) {
            Ok(ast::Node::Tag(self.parse_tag(input)?))
        } else if lookahead.peek(syn::LitStr)
            || lookahead.peek(syn::token::Brace)
        {
            Ok(ast::Node::Value(self.parse_value(input)?))
        } else {
            Err(lookahead.error())
        }
    }
}

impl Parse for ast::DashIdent {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Parser::new().parse_ident(input)
    }
}

impl Parse for ast::Doctype {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Parser::new().parse_doctype(input)
    }
}

impl Parse for ast::Value {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Parser::new().parse_value(input)
    }
}

impl Parse for ast::Attr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Parser::new().parse_attr(input)
    }
}

impl Parse for ast::Tag {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Parser::new().parse_tag(input)
    }
}

impl Parse for ast::Node {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Parser::new().parse_node(input)
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
