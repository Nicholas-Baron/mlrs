use crate::syntax::{self, BinaryOperation, Identifier, Literal, Pattern};

use std::collections::HashMap;

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct IRId(u32);

impl IRId {
    fn inc(&mut self) {
        self.0 += 1;
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
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
}

impl Module {
    pub fn new() -> Self {
        Self::default()
    }

    #[cfg(test)]
    pub fn from_expr(expr: &syntax::Expr) -> (Self, IRId) {
        let mut module = Self::new();
        let expr_id = module.add_expr(expr);
        (module, expr_id)
    }

    #[cfg(test)]
    pub fn from_decls(decls: &[syntax::Declaration]) -> Self {
        let mut module = Self::new();
        decls.iter().for_each(|decl| {
            module.add_decl(decl);
        });
        module
    }

    /// Add a fresh name scope, returning it for convenience.
    fn add_new_name_scope(&mut self) -> &mut HashMap<Identifier, IRId> {
        self.name_scope.push(Default::default());
        self.name_scope.last_mut().unwrap()
    }

    /// Remove the most recent name scope from the list of scopes.
    fn hide_top_name_scope(&mut self) {
        self.name_scope.pop();
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

    pub fn get_item(&self, id: &IRId) -> Option<&IRItem> {
        self.ir_items.get(id)
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

    pub fn add_decl(&mut self, decl: &syntax::Declaration) -> IRId {
        let syntax::Declaration { name, expr } = decl;

        // We need to allow recursive bindings.
        // To do so, we can rewrite an id after it has been added.
        let temp_ir_id = self.next_ir_id();
        let scope = self.current_name_scope();
        scope.insert(name.clone(), temp_ir_id.clone());
        let new_ir_id = self.add_expr(expr);
        self.rewrite_id_to(&new_ir_id, &temp_ir_id);
        new_ir_id
    }

    pub fn add_expr(&mut self, expr: &syntax::Expr) -> IRId {
        use syntax::Expr;

        let (expr_id, ir_expr) = match expr {
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
                // We also need to remove the scope after we are done.
                {
                    let param_id = self.next_ir_id();
                    let name_scope = self.add_new_name_scope();
                    match parameter {
                        Pattern::Id(param) => {
                            name_scope.insert(param.clone(), param_id.clone());
                            self.ir_items
                                .insert(param_id.clone(), IRItem::Identifier(param.clone()));
                        }
                        Pattern::Ignore => {}
                    }
                    let body = self.add_expr(body);
                    self.hide_top_name_scope();

                    IRItem::Lambda {
                        parameter: param_id,
                        body,
                    }
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
                    .map(|decl| (decl.name.clone(), self.add_decl(decl)))
                    .collect();
                let name_scope = self.add_new_name_scope();
                *name_scope = bound_names;
                let inner_expr = self.add_expr(inner_expr);
                self.hide_top_name_scope();
                return inner_expr;
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
            parameter: Pattern::Id("x".to_string()),
            body: Box::new(syntax::Expr::Lambda {
                parameter: Pattern::Id("y".to_string()),
                body: Box::new(syntax::Expr::Identifier("x".to_string())),
            }),
        };

        let (module, lambda_id) = Module::from_expr(&lambda);
        assert_eq!(module.name_scope.len(), 0);

        let x_id = module.find_identifier(&"x".to_string());
        assert_eq!(x_id, None);
        let y_id = module.find_identifier(&"y".to_string());
        assert_eq!(y_id, None);
        let z_id = module.find_identifier(&"z".to_string());
        assert_eq!(z_id, None);

        println!("{:?}", module);

        assert!(match module.get_item(&lambda_id) {
            Some(&IRItem::Lambda { .. }) => true,
            _ => false,
        });
    }

    #[test]
    fn lowers_binding() {
        let lambda = syntax::Declaration {
            name: "f".to_string(),
            expr: Box::new(syntax::Expr::Lambda {
                parameter: Pattern::Id("x".to_string()),
                body: Box::new(syntax::Expr::Identifier("x".to_string())),
            }),
        };

        let module = Module::from_decls(&[lambda]);
        assert_eq!(module.name_scope.len(), 1);

        let f_id = module.find_identifier(&"f".to_string());
        assert_ne!(f_id, None);
        let x_id = module.find_identifier(&"x".to_string());
        assert_eq!(x_id, None);

        println!("{:?}", module);

        assert!(match module.ir_items.get(f_id.unwrap()) {
            Some(&IRItem::Lambda {
                ref parameter,
                ref body,
            }) => parameter == body,
            _ => false,
        });
    }

    #[test]
    fn lowers_multiple() {
        let x_bind = syntax::Declaration {
            name: "x".to_string(),
            expr: Box::new(syntax::Expr::Literal(Literal::Integer(5))),
        };

        let mut module = Module::from_decls(&[x_bind]);
        assert_eq!(module.name_scope.len(), 1);

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
        let x_bind = syntax::Declaration {
            name: "x".to_string(),
            expr: Box::new(syntax::Expr::Identifier("x".to_string())),
        };

        let module = Module::from_decls(&[x_bind]);
        assert_eq!(module.name_scope.len(), 1);

        let f_id = module.find_identifier(&"f".to_string());
        assert_eq!(f_id, None);
        let x_id = module.find_identifier(&"x".to_string());
        assert_ne!(x_id, None);

        println!("{:?}", module);
    }

    #[test]
    fn lowers_subfunction() {
        let inner_lambda = syntax::Declaration {
            name: "inner".to_string(),
            expr: Box::new(syntax::Expr::Lambda {
                parameter: Pattern::Id("x".to_string()),
                body: Box::new(syntax::Expr::Identifier("x".to_string())),
            }),
        };

        let outer_lambda = syntax::Declaration {
            name: "outer".to_string(),
            expr: Box::new(syntax::Expr::Lambda {
                parameter: Pattern::Id("x".to_string()),
                body: Box::new(syntax::Expr::Let {
                    inner_expr: Box::new(syntax::Expr::Binary {
                        op: syntax::BinaryOperation::Application,
                        rhs: Box::new(syntax::Expr::Identifier("x".to_string())),
                        lhs: Box::new(syntax::Expr::Identifier("inner".to_string())),
                    }),
                    bound_values: vec![inner_lambda],
                }),
            }),
        };

        let module = Module::from_decls(&[outer_lambda]);
        assert_eq!(module.name_scope.len(), 1);
        println!("{:?}", module);

        let x_id = module.find_identifier(&"x".to_string());
        assert_eq!(x_id, None);
        let inner_id = module.find_identifier(&"inner".to_string());
        assert_eq!(inner_id, None);
        let outer_id = module.find_identifier(&"outer".to_string());
        assert_ne!(outer_id, None);

        assert!(match module.get_item(outer_id.unwrap()) {
            Some(&IRItem::Lambda { .. }) => true,
            _ => false,
        });
    }

    #[test]
    fn lowers_ignore() {
        let const_func = syntax::Declaration {
            name: "const".to_string(),
            expr: Box::new(syntax::Expr::Lambda {
                parameter: syntax::Pattern::Id("x".to_string()),
                body: Box::new(syntax::Expr::Lambda {
                    parameter: syntax::Pattern::Ignore,
                    body: Box::new(syntax::Expr::Identifier("x".to_string())),
                }),
            }),
        };

        let module = Module::from_decls(&[const_func]);
        assert_eq!(module.name_scope.len(), 1);
        println!("{:?}", module);

        let x_id = module.find_identifier(&"x".to_string());
        assert_eq!(x_id, None);
    }
}
