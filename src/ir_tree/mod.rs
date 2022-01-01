use crate::syntax::{self, BinaryOperation, Identifier, Literal};

use std::collections::HashMap;

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct IRId(u32);

impl IRId {
    fn inc(&mut self) {
        self.0 += 1;
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum IRItem {
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
        self.name_scope
            .iter()
            .rev()
            .find_map(|scope| scope.get(identifier))
    }

    pub fn root_id(&self) -> Option<&IRId> {
        self.root_id.as_ref()
    }

    pub fn get_item(&self, id: &IRId) -> Option<&IRItem> {
        self.ir_items.get(id)
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
            Expr::Binding { name, expr } => {
                let ir_id = self.add_expr(expr);
                self.name_scope
                    .last_mut()
                    .map(|scope| scope.insert(name.clone(), ir_id.clone()))
                    .flatten()
                    .map(|old_id| {
                        eprintln!("Identifier {} was bound to {:?}, but is now bound to {:?} in the same name scope",
                            name,old_id,ir_id
                            );
                        eprintln!("{:?}",self);
                    });
                return ir_id;
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
                        self.ir_items
                            .insert(param_id.clone(), IRItem::Identifier(parameter.clone()));
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lowers_lambda() {
        let lambda = syntax::Expr::Lambda {
            parameter: "x".to_string(),
            body: Box::new(syntax::Expr::Lambda {
                parameter: "y".to_string(),
                body: Box::new(syntax::Expr::Identifier("x".to_string())),
            }),
        };

        let module = Module::from_expr(&lambda);
        assert_eq!(module.name_scope.len(), 2);
        assert_ne!(module.root_id, None);

        let x_id = module.find_identifier(&"x".to_string());
        assert_ne!(x_id, None);
        let y_id = module.find_identifier(&"y".to_string());
        assert_ne!(y_id, None);
        let z_id = module.find_identifier(&"z".to_string());
        assert_eq!(z_id, None);

        println!("{:?}", module);

        assert_eq!(
            module.ir_items.get(x_id.unwrap()),
            Some(&IRItem::Identifier("x".to_string()))
        );
        assert_eq!(
            module.ir_items.get(y_id.unwrap()),
            Some(&IRItem::Identifier("y".to_string()))
        );
    }

    #[test]
    fn lowers_binding() {
        let lambda = syntax::Expr::Binding {
            name: "f".to_string(),
            expr: Box::new(syntax::Expr::Lambda {
                parameter: "x".to_string(),
                body: Box::new(syntax::Expr::Identifier("x".to_string())),
            }),
        };

        let module = Module::from_expr(&lambda);
        assert_eq!(module.name_scope.len(), 1);
        assert_ne!(module.root_id, None);

        let f_id = module.find_identifier(&"f".to_string());
        assert_ne!(f_id, None);
        let x_id = module.find_identifier(&"x".to_string());
        assert_ne!(x_id, None);
        let x_id = x_id.unwrap();

        println!("{:?}", module);

        assert_eq!(
            module.ir_items.get(f_id.unwrap()),
            Some(&IRItem::Lambda {
                parameter: x_id.clone(),
                body: x_id.clone()
            })
        );
        assert_eq!(
            module.ir_items.get(x_id),
            Some(&IRItem::Identifier("x".to_string()))
        );
    }
}
