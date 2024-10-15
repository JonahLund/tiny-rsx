use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use syn::{
    braced,
    ext::IdentExt as _,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token, Ident, LitStr, Result, Token,
};

use crate::ast::{kw, Attr, DashIdent, Doctype, Node, NodeTree, Tag, Value};

pub fn parse(input: TokenStream) -> Result<Box<[Node]>> {
    Ok(syn::parse::<NodeTree>(input)?.nodes)
}

pub fn parse2(input: TokenStream2) -> Result<Box<[Node]>> {
    Ok(syn::parse2::<NodeTree>(input)?.nodes)
}

impl Parse for DashIdent {
    fn parse(input: ParseStream) -> Result<Self> {
        // Parse a non-empty sequence of identifiers separated by dashes.
        let inner =
            Punctuated::<Ident, Token![-]>::parse_separated_nonempty_with(
                input,
                Ident::parse_any,
            )?;

        Ok(DashIdent(inner))
    }
}

impl Parse for Doctype {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Doctype {
            lt_sign: input.parse()?,
            excl_mark: input.parse()?,
            doctype: input.parse()?,
            html: input.parse()?,
            gt_sign: input.parse()?,
        })
    }
}

impl Parse for Value {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(LitStr) {
            Ok(Value::LitStr(input.parse()?))
        } else if lookahead.peek(token::Brace) {
            let content;
            braced!(content in input);
            Ok(Value::Expr(content.parse()?))
        } else {
            Err(lookahead.error())
        }
    }
}

impl Parse for Attr {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Attr {
            key: input.parse()?,
            eq_sign: input.parse()?,
            value: input.parse()?,
        })
    }
}

impl Parse for Tag {
    fn parse(input: ParseStream) -> Result<Self> {
        let lt_sign = input.parse()?;

        if input.parse::<Option<Token![/]>>()?.is_some() {
            let name = input.parse()?;
            let gt_sign = input.parse()?;

            return Ok(Tag::Closing {
                lt_sign,
                name,
                gt_sign,
            });
        }

        let name = input.parse()?;

        let mut attrs = Vec::new();
        while !(input.peek(Token![>])
            || (input.peek(Token![/]) && input.peek2(Token![>])))
        {
            attrs.push(input.parse()?);
        }

        let void_slash = input.parse()?;
        let gt_sign = input.parse()?;

        Ok(Tag::Opening {
            lt_sign,
            name,
            attrs,
            void_slash,
            gt_sign,
        })
    }
}

impl Parse for Node {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![<])
            && input.peek2(Token![!])
            && input.peek3(kw::DOCTYPE)
        {
            Ok(Node::Doctype(input.parse()?))
        } else if lookahead.peek(Token![<]) {
            Ok(Node::Tag(input.parse()?))
        } else if lookahead.peek(LitStr) || lookahead.peek(token::Brace) {
            Ok(Node::Value(input.parse()?))
        } else {
            Err(lookahead.error())
        }
    }
}

impl Parse for NodeTree {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut nodes = Vec::new();
        let mut stack = Vec::new();

        while !input.is_empty() {
            let curr = input.parse()?;
            let other = stack.last().and_then(|i| nodes.get(*i));

            match (&curr, other) {
                (
                    Node::Tag(Tag::Opening {
                        void_slash: None, ..
                    }),
                    _,
                ) => {
                    nodes.push(curr);
                    stack.push(nodes.len() - 1);
                }
                (
                    Node::Tag(Tag::Closing { name, .. }),
                    Some(Node::Tag(Tag::Opening {
                        name: other_name,
                        void_slash: None,
                        ..
                    })),
                ) => {
                    if name != other_name {
                        return Err(syn::Error::new_spanned(
                            &curr,
                            format_args!(
                                "closing tag mismatch, expected \
                                 </{other_name}> found </{name}>"
                            ),
                        ));
                    }

                    nodes.push(curr);
                    stack.pop();
                }
                (Node::Tag(Tag::Closing { .. }), None) => {
                    return Err(syn::Error::new_spanned(
                        &curr,
                        "missing opening tag",
                    ));
                }
                _ => {
                    nodes.push(curr);
                }
            }
        }

        if let Some(node) = stack.last().and_then(|i| nodes.get(*i)) {
            return Err(syn::Error::new_spanned(node, "missing closing tag"));
        }

        Ok(Self {
            nodes: nodes.into_boxed_slice(),
        })
    }
}

#[cfg(test)]
mod tests {
    use proc_macro2::Span;
    use quote::quote;

    use super::*;

    macro_rules! dash_ident {
        ($($tt:tt)*) => {
            syn::parse2::<DashIdent>(quote!($($tt)*)).unwrap()
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
            syn::parse2::<Doctype>(quote!(<!DOCTYPE html>)).unwrap(),
            Doctype {
                lt_sign: Token![<](Span::call_site()),
                excl_mark: Token![!](Span::call_site()),
                doctype: kw::DOCTYPE(Span::call_site()),
                html: kw::html(Span::call_site()),
                gt_sign: Token![>](Span::call_site()),
            }
        )
    }

    #[test]
    fn parses_to_string_value() {
        assert_eq!(
            syn::parse2::<Value>(quote!("foo")).unwrap(),
            Value::LitStr(lit_str!("foo")),
        );
        assert_eq!(
            syn::parse2::<Value>(quote!("")).unwrap(),
            Value::LitStr(lit_str!("")),
        );
    }

    #[test]
    fn parses_to_expr_value() {
        assert_eq!(
            syn::parse2::<Value>(quote!({ true })).unwrap(),
            Value::Expr(syn::Expr::Lit(syn::ExprLit {
                attrs: vec![],
                lit: syn::Lit::Bool(lit_bool!(true))
            })),
        );
    }

    #[test]
    fn parses_to_opening_tag() {
        assert_eq!(
            syn::parse2::<Tag>(quote! ( <foo> )).unwrap(),
            Tag::Opening {
                lt_sign: token::Lt(Span::call_site()),
                name: dash_ident!(foo),
                attrs: vec![],
                void_slash: None,
                gt_sign: token::Gt(Span::call_site()),
            }
        )
    }

    #[test]
    fn parses_to_opening_tag_with_attrs() {
        assert_eq!(
            syn::parse2::<Tag>(quote!(<foo bar="baz" qux={false}>)).unwrap(),
            Tag::Opening {
                lt_sign: token::Lt(Span::call_site()),
                name: dash_ident!(foo),
                attrs: vec![
                    Attr {
                        key: dash_ident!(bar),
                        eq_sign: Token![=](Span::call_site()),
                        value: Value::LitStr(lit_str!("baz"))
                    },
                    Attr {
                        key: dash_ident!(qux),
                        eq_sign: Token![=](Span::call_site()),
                        value: Value::Expr(syn::Expr::Lit(syn::ExprLit {
                            attrs: vec![],
                            lit: syn::Lit::Bool(lit_bool!(false))
                        }))
                    },
                ],
                void_slash: None,
                gt_sign: token::Gt(Span::call_site()),
            }
        )
    }

    #[test]
    fn parses_to_void_tag() {
        assert_eq!(
            syn::parse2::<Tag>(quote!(<foo />)).unwrap(),
            Tag::Opening {
                lt_sign: token::Lt(Span::call_site()),
                name: dash_ident!(foo),
                attrs: vec![],
                void_slash: Some(syn::token::Slash(Span::call_site())),
                gt_sign: token::Gt(Span::call_site()),
            }
        )
    }
}
