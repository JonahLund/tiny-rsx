use core::fmt::{Display, Formatter, Result, Write as _};

use crate::ast::DashIdent;

impl Display for DashIdent {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for (i, ident) in self.0.iter().enumerate() {
            if i != 0 {
                f.write_char('-')?;
            }
            ident.fmt(f)?;
        }
        Ok(())
    }
}
