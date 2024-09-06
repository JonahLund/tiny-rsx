use syn::punctuated::Punctuated;

/// An identifier seperated by dashes: `foo-bar-baz`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DashIdent(pub Punctuated<syn::Ident, syn::Token![-]>);

/// An HTML doctype declaration: <!DOCTYPE html>.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Doctype;

/// A value, either a string literal or a braced expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    /// A string literal: `"foo bar"`.
    LitStr(syn::LitStr),
    /// A braced expression: `{1 + 2}`.
    Expr(syn::Expr),
}

/// An HTML attribute consisting of a key and a value: `foo="bar"`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Attr {
    pub key: DashIdent,
    pub value: Value,
}

/// An HTML opening or closing tag: `<foo>`, `</foo>`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tag {
    Opening {
        name: DashIdent,
        attrs: Vec<Attr>,
        void_slash: Option<syn::Token![/]>,
    },
    Closing {
        name: DashIdent,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
    Doctype(Doctype),
    Tag(Tag),
    Value(Value),
}
