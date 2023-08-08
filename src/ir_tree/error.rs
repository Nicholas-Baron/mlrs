use std::{
    collections::HashSet,
    fmt::{self, Display, Formatter},
};

use crate::syntax::Identifier;

#[derive(Debug)]
pub enum LoweringError {
    MissingIdentifier {
        name: Identifier,
    },
    DifferentParamCounts {
        name: Identifier,
        counts: HashSet<usize>,
    },
}

impl Display for LoweringError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LoweringError::MissingIdentifier { name } => f.write_fmt(format_args!(
                "Could not find declaration of identifier `{}`",
                name
            )),
            LoweringError::DifferentParamCounts { name, counts } => f.write_fmt(format_args!(
                "Function {} has differing counts of parameters, namely {:?}",
                name, counts
            )),
        }
    }
}
