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
    If {
        condition: IRId,
        true_value: IRId,
        false_value: IRId,
    },
}

impl IRItem {
    fn rewrite_id_to(&mut self, dest_id: &IRId, src_id: &IRId) {
        match self {
            Self::Lambda { body, .. } => {
                if body == src_id {
                    *body = dest_id.clone();
                }
            }
            Self::Binary { lhs, rhs, .. } => {
                if lhs == src_id {
                    *lhs = dest_id.clone();
                }
                if rhs == src_id {
                    *rhs = dest_id.clone();
                }
            }
            Self::If {
                condition,
                true_value,
                false_value,
            } => {
                if condition == src_id {
                    *condition = dest_id.clone();
                }
                if true_value == src_id {
                    *true_value = dest_id.clone();
                }
                if false_value == src_id {
                    *false_value = dest_id.clone();
                }
            }
            Self::Literal(_) | Self::Identifier(_) => {}
        }
    }
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

    /// Gets the current name scope. If none exists, then one will be created.
    fn current_name_scope(&mut self) -> &mut HashMap<Identifier, IRId> {
        if self.name_scope.is_empty() {
            self.add_new_name_scope()
        } else {
            self.name_scope.last_mut().unwrap()
        }
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

    pub fn set_root(&mut self, id: IRId) {
        self.root_id = Some(id)
    }

    fn rewrite_id_to(&mut self, dest_id: &IRId, src_id: &IRId) {
        // rewriting means that we need to

        // 1. "rebind" any names bound to `src_id` instead to `dest_id`
        for scope in &mut self.name_scope {
            for (_, id) in scope.iter_mut().filter(|(_, id)| id == &src_id) {
                *id = dest_id.clone();
            }
        }

        // 2. change all references of `src_id` to `dest_id` in ir_items
        // 2a. reassign the item with the `src_id` to `dest_id`
        if let Some(item) = self.ir_items.remove(src_id) {
            self.ir_items.insert(dest_id.clone(), item);
        }

        // 2b. change all items from referencing `src_id` to `dest_id`
        for item in self.ir_items.values_mut() {
            item.rewrite_id_to(dest_id, src_id);
        }
    }

    pub fn add_expr(&mut self, expr: &syntax::Expr) -> IRId {
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
                // We need to allow recursive bindings.
                // To do so, we can rewrite an id after it has been added.
                let temp_ir_id = self.next_ir_id();
                let scope = self.current_name_scope();
                if let Some(old_id) = scope.insert(name.clone(), temp_ir_id.clone()) {
                    eprintln!("Identifier {} was bound to {:?}, but is now bound to {:?} in the same name scope",
                            name,old_id,temp_ir_id
                            );
                    eprintln!("{:?}", self);
                }
                let new_ir_id = self.add_expr(expr);
                self.rewrite_id_to(&new_ir_id, &temp_ir_id);
                return new_ir_id;
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
            Expr::If {
                condition,
                true_value,
                false_value,
            } => (
                self.next_ir_id(),
                IRItem::If {
                    condition: self.add_expr(condition),
                    true_value: self.add_expr(true_value),
                    false_value: self.add_expr(false_value),
                },
            ),
            Expr::Let {
                bound_values,
                inner_expr,
            } => {
                let bound_names: HashMap<String, _> = bound_values
                    .iter()
                    .map(|(id, expr)| (id.clone(), self.add_expr(expr)))
                    .collect();
                let name_scope = self.add_new_name_scope();
                *name_scope = bound_names;
                return self.add_expr(inner_expr);
            }
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
        assert_eq!(module.name_scope.len(), 2);
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

    #[test]
    fn lowers_multiple() {
        let x_bind = syntax::Expr::Binding {
            name: "x".to_string(),
            expr: Box::new(syntax::Expr::Literal(Literal::Integer(5))),
        };

        let mut module = Module::from_expr(&x_bind);
        assert_eq!(module.name_scope.len(), 1);
        assert_ne!(module.root_id, None);

        let f_id = module.find_identifier(&"f".to_string());
        assert_eq!(f_id, None);
        let x_id = module.find_identifier(&"x".to_string());
        assert_ne!(x_id, None);
        let x_id = x_id.cloned().unwrap();

        println!("{:?}", module);

        let sum_id = module.add_expr(&syntax::Expr::Binary {
            op: BinaryOperation::Plus,
            lhs: Box::new(syntax::Expr::Identifier("x".to_string())),
            rhs: Box::new(syntax::Expr::Literal(Literal::Integer(10))),
        });

        let sum_ir = module.get_item(&sum_id).cloned().unwrap();
        if let IRItem::Binary { lhs, .. } = sum_ir {
            assert_eq!(lhs, x_id);
        } else {
            panic!();
        }
    }

    #[test]
    fn lowers_recursion() {
        let x_bind = syntax::Expr::Binding {
            name: "x".to_string(),
            expr: Box::new(syntax::Expr::Identifier("x".to_string())),
        };

        let module = Module::from_expr(&x_bind);
        assert_eq!(module.name_scope.len(), 1);
        assert_ne!(module.root_id, None);

        let f_id = module.find_identifier(&"f".to_string());
        assert_eq!(f_id, None);
        let x_id = module.find_identifier(&"x".to_string());
        assert_ne!(x_id, None);

        println!("{:?}", module);
    }
}
