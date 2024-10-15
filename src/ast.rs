use quote::ToTokens;
use syn::{punctuated::Punctuated, Expr, Ident, LitStr, Token};

pub(crate) mod kw {
    use syn::custom_keyword;

    custom_keyword!(DOCTYPE);
    custom_keyword!(html);
}

/// An identifier seperated by dashes: `foo-bar-baz`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DashIdent(pub Punctuated<Ident, Token![-]>);

/// An HTML doctype declaration: <!DOCTYPE html>.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Doctype {
    pub lt_sign: Token![<],
    pub excl_mark: Token![!],
    pub doctype: kw::DOCTYPE,
    pub html: kw::html,
    pub gt_sign: Token![>],
}

/// A value, either a string literal or a braced expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    /// A string literal: `"foo bar"`.
    LitStr(LitStr),
    /// A braced expression: `{1 + 2}`.
    Expr(Expr),
}

/// An HTML attribute consisting of a key and a value: `foo="bar"`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Attr {
    pub key: DashIdent,
    pub eq_sign: Token![=],
    pub value: Value,
}

/// An HTML opening or closing tag: `<foo>`, `</foo>`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tag {
    Opening {
        lt_sign: Token![<],
        name: DashIdent,
        attrs: Vec<Attr>,
        void_slash: Option<Token![/]>,
        gt_sign: Token![>],
    },
    Closing {
        lt_sign: Token![<],
        name: DashIdent,
        gt_sign: Token![>],
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
    Doctype(Doctype),
    Tag(Tag),
    Value(Value),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NodeTree {
    pub nodes: Box<[Node]>,
}

impl Tag {
    pub fn is_opening_tag(&self) -> bool {
        matches!(self, Self::Opening { .. })
    }

    pub fn is_closing_tag(&self) -> bool {
        matches!(self, Self::Closing { .. })
    }

    pub fn is_self_closing(&self) -> bool {
        matches!(
            self,
            Self::Opening {
                void_slash: Some(_),
                ..
            }
        )
    }
}

impl ToTokens for DashIdent {
    #[inline]
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.0.to_tokens(tokens)
    }
}

impl ToTokens for Doctype {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.lt_sign.to_tokens(tokens);
        self.excl_mark.to_tokens(tokens);
        self.doctype.to_tokens(tokens);
        self.html.to_tokens(tokens);
        self.gt_sign.to_tokens(tokens);
    }
}

impl ToTokens for Value {
    #[inline]
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Value::LitStr(lit_str) => lit_str.to_tokens(tokens),
            Value::Expr(expr) => expr.to_tokens(tokens),
        }
    }
}

impl ToTokens for Attr {
    #[inline]
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.key.to_tokens(tokens);
        self.value.to_tokens(tokens);
    }
}

impl ToTokens for Tag {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Tag::Opening {
                lt_sign,
                name,
                attrs,
                void_slash,
                gt_sign,
            } => {
                lt_sign.to_tokens(tokens);
                name.to_tokens(tokens);
                for attr in attrs {
                    attr.to_tokens(tokens);
                }
                void_slash.to_tokens(tokens);
                gt_sign.to_tokens(tokens);
            }
            Tag::Closing {
                lt_sign,
                name,
                gt_sign,
            } => {
                lt_sign.to_tokens(tokens);
                name.to_tokens(tokens);
                gt_sign.to_tokens(tokens);
            }
        }
    }
}

impl ToTokens for Node {
    #[inline]
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Node::Doctype(doctype) => doctype.to_tokens(tokens),
            Node::Tag(tag) => tag.to_tokens(tokens),
            Node::Value(value) => value.to_tokens(tokens),
        }
    }
}

impl ToTokens for NodeTree {
    #[inline]
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        for node in &self.nodes {
            node.to_tokens(tokens);
        }
    }
}
