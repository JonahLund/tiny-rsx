use core::fmt::{self, Write as _};

use quote::ToTokens;

use crate::ast;

pub struct Formatter<'a, 'b> {
    buf: &'a mut String,
    args: &'a mut Vec<(usize, &'b dyn ToTokens)>,
}

impl fmt::Write for Formatter<'_, '_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.buf.write_str(s)
    }
}

impl<'a, 'b> Formatter<'a, 'b> {
    #[inline]
    pub fn new(
        buf: &'a mut String,
        args: &'a mut Vec<(usize, &'b dyn ToTokens)>,
    ) -> Self {
        Self { buf, args }
    }

    #[inline]
    pub fn write_ident(&mut self, i: &ast::DashIdent) -> fmt::Result {
        write!(self, "{}", i)
    }

    #[inline]
    pub fn write_doctype(&mut self, _: &ast::Doctype) -> fmt::Result {
        self.write_str("<!DOCTYPE html>")
    }

    #[inline]
    pub fn write_expr(&mut self, e: &'b syn::Expr) -> fmt::Result {
        self.args.push((self.buf.len(), e));
        Ok(())
    }

    #[inline]
    pub fn write_value(&mut self, v: &'b ast::Value) -> fmt::Result {
        match v {
            ast::Value::LitStr(lit_str) => self.write_str(&lit_str.value()),
            ast::Value::Expr(expr) => self.write_expr(expr),
        }
    }

    #[inline]
    pub fn write_attr(&mut self, a: &'b ast::Attr) -> fmt::Result {
        self.write_ident(&a.key)?;
        self.write_char('=')?;
        self.write_char('"')?;
        self.write_value(&a.value)?;
        self.write_char('"')
    }

    #[inline]
    pub fn write_tag(&mut self, t: &'b ast::Tag) -> fmt::Result {
        match t {
            ast::Tag::Opening { name, attrs, .. } => {
                self.write_char('<')?;
                self.write_ident(name)?;
                for attr in attrs {
                    self.write_char(' ')?;
                    self.write_attr(attr)?;
                }
                self.write_char('>')
            }
            ast::Tag::Closing { name } => {
                self.write_char('<')?;
                self.write_char('/')?;
                self.write_ident(name)?;
                self.write_char('>')
            }
        }
    }

    #[inline]
    pub fn write_node(&mut self, n: &'b ast::Node) -> fmt::Result {
        match n {
            ast::Node::Doctype(d) => self.write_doctype(d),
            ast::Node::Tag(t) => self.write_tag(t),
            ast::Node::Value(v) => self.write_value(v),
        }
    }
}

impl fmt::Display for ast::DashIdent {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> fmt::Result {
        for (i, ident) in self.0.iter().enumerate() {
            if i != 0 {
                f.write_char('-')?;
            }
            ident.fmt(f)?;
        }
        Ok(())
    }
}
