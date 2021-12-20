use crate::syntax::{self, BinaryOperation, Identifier, Literal};

use std::collections::HashMap;

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct IRId(u32);

impl IRId {
    fn inc(&mut self) {
        self.0 += 1;
    }
}

#[derive(Debug)]
enum IRItem {
    Literal(Literal),
    Identifier(Identifier),
    Lambda {
        parameter: IRId,
        body: IRId,
    },
    Binary {
        lhs: IRId,
        rhs: IRId,
        op: BinaryOperation,
    },
}

#[derive(Default, Debug)]
pub struct Module {
    ir_items: HashMap<IRId, IRItem>,
    name_scope: Vec<HashMap<Identifier, IRId>>,
    next_ir_id: IRId,
    root_id: Option<IRId>,
}

impl Module {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_expr(expr: &syntax::Expr) -> Self {
        let mut module = Self::new();
        let root_id = module.add_expr(expr);
        module.root_id = Some(root_id);
        module
    }

    /// Add a fresh name scope, returning it for convenience.
    fn add_new_name_scope(&mut self) -> &mut HashMap<Identifier, IRId> {
        self.name_scope.push(Default::default());
        self.name_scope.last_mut().unwrap()
    }

    /// Walk the name scopes in reverse order to find the `IRId` for a particular identifier.
    fn find_identifier(&self, identifier: &Identifier) -> Option<&IRId> {
        for scope in self.name_scope.iter().rev() {
            if let Some(id) = scope.get(identifier) {
                return Some(id);
            }
        }
        None
    }

    fn add_expr(&mut self, expr: &syntax::Expr) -> IRId {
        use syntax::Expr;

        let (expr_id, ir_expr) = match &*expr {
            Expr::Literal(x) => (self.next_ir_id(), IRItem::Literal(x.clone())),
            Expr::Identifier(ident) => {
                return match self.find_identifier(ident).cloned() {
                    Some(id) => id,
                    None => panic!("Could not find identifier '{}' in module {:?}", ident, self),
                }
            }
            Expr::Lambda { body, parameter } => (
                self.next_ir_id(),
                // NOTE: The initalization order here matters.
                // The parameter needs to be added *before* the body is processed.
                IRItem::Lambda {
                    parameter: {
                        let param_id = self.next_ir_id();
                        let name_scope = self.add_new_name_scope();
                        name_scope.insert(parameter.clone(), param_id.clone());
                        param_id
                    },
                    body: self.add_expr(body),
                },
            ),
            Expr::Binary { lhs, rhs, ref op } => (
                self.next_ir_id(),
                IRItem::Binary {
                    lhs: self.add_expr(lhs),
                    rhs: self.add_expr(rhs),
                    op: op.clone(),
                },
            ),
        };
        self.ir_items.insert(expr_id.clone(), ir_expr);
        expr_id
    }

    fn next_ir_id(&mut self) -> IRId {
        let id = self.next_ir_id.clone();
        self.next_ir_id.inc();
        id
    }
}