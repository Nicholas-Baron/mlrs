use std::fmt::{self, Display, Formatter};

use crate::syntax::Identifier;

#[derive(Debug)]
pub enum LoweringError {
    MissingIdentifier { name: Identifier },
}

impl Display for LoweringError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LoweringError::MissingIdentifier { name } => f.write_fmt(format_args!(
                "Could not find declaration of identifier `{}`",
                name
            )),
        }
    }
}
