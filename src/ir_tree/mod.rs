use crate::syntax::{self, BinaryOperation, Identifier, Literal, Pattern};

use std::collections::HashMap;

mod error;
use error::LoweringError;

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct IRId(u32);

impl IRId {
    fn inc(&mut self) {
        self.0 += 1;
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IRPattern {
    Ignore,
    Literal(Literal),
    Identifier(IRId),
    Tuple(Vec<IRId>),
    ListCons(Vec<IRId>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum IRItem {
    Literal(Literal),
    Identifier {
        name: Identifier,
        declaring_item: Option<IRId>,
    },
    Pattern(IRPattern),
    Tuple {
        elements: Vec<IRId>,
    },
    EmptyList,
    ListCons {
        item: IRId,
        rest_list: IRId,
    },
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
    Match {
        scrutinee: IRId,
        arms: Vec<(IRId, IRId)>,
    },
    Binding {
        pattern: IRId,
        value: IRId,
    },
    Let {
        binding_list: Vec<IRId>,
        inner_expr: IRId,
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
            Self::Identifier {
                declaring_item: Some(id),
                ..
            } => {
                if id == src_id {
                    *id = dest_id.clone();
                }
            }
            Self::Literal(_) | Self::EmptyList => {}
            Self::ListCons { item, rest_list } => {
                if item == src_id {
                    *item = dest_id.clone();
                }
                if rest_list == src_id {
                    *rest_list = dest_id.clone();
                }
            }
            Self::Tuple { elements } => {
                elements.iter_mut().for_each(|elem| {
                    if elem == src_id {
                        *elem = dest_id.clone();
                    }
                });
            }
            Self::Match { scrutinee, arms } => {
                if scrutinee == src_id {
                    *scrutinee = dest_id.clone();
                }
                arms.iter_mut().for_each(|(pattern, expr)| {
                    if pattern == src_id {
                        *pattern = dest_id.clone();
                    }

                    if expr == src_id {
                        *expr = dest_id.clone();
                    }
                });
            }
            Self::Identifier { .. } => todo!(),
            IRItem::Pattern(pattern) => match pattern {
                IRPattern::Ignore => {}
                IRPattern::Literal(_) => todo!(),
                IRPattern::Identifier(x) => {
                    if x == src_id {
                        *x = dest_id.clone();
                    }
                }
                IRPattern::Tuple(_) => todo!(),
                IRPattern::ListCons(elements) => {
                    for elem in elements {
                        if elem == src_id {
                            *elem = dest_id.clone();
                        }
                    }
                }
            },
            Self::Binding { pattern, value } => {
                if pattern == src_id {
                    *pattern = dest_id.clone();
                }
                if value == src_id {
                    *value = src_id.clone();
                }
            }
            Self::Let {
                binding_list,
                inner_expr,
            } => {
                if inner_expr == src_id {
                    *inner_expr = dest_id.clone();
                }
                for binding in binding_list {
                    if binding == src_id {
                        *binding = dest_id.clone();
                    }
                }
            }
        }
    }
}

type Scope = HashMap<Identifier, IRId>;

type LoweringResult<T> = Result<T, LoweringError>;

#[derive(Default, Debug)]
pub struct Module {
    ir_items: HashMap<IRId, IRItem>,
    name_scopes: Vec<Scope>,
    next_ir_id: IRId,
}

impl Module {
    pub fn new() -> Self {
        Self::default()
    }

    #[cfg(test)]
    pub fn from_expr(expr: &syntax::Expr) -> LoweringResult<(Self, IRId)> {
        let mut module = Self::new();
        let expr_id = module.add_expr(expr)?;
        Ok((module, expr_id))
    }

    #[cfg(test)]
    pub fn from_decls(decls: &[syntax::Declaration]) -> LoweringResult<Self> {
        let mut module = Self::new();
        for decl in decls {
            module.add_decl(decl)?;
        }
        Ok(module)
    }

    /// Add a fresh name scope, returning it for convenience.
    fn add_new_name_scope(&mut self) -> &mut Scope {
        self.name_scopes.push(Default::default());
        self.name_scopes.last_mut().unwrap()
    }

    /// Remove the most recent name scope from the list of scopes.
    fn hide_top_name_scope(&mut self) {
        self.name_scopes.pop();
    }

    /// Gets the current name scope. If none exists, then one will be created.
    fn current_name_scope(&mut self) -> &mut Scope {
        if self.name_scopes.is_empty() {
            self.add_new_name_scope()
        } else {
            self.name_scopes.last_mut().unwrap()
        }
    }

    /// Walk the name scopes in reverse order to find the `IRId` for a particular identifier.
    fn find_identifier(&self, identifier: &Identifier) -> Option<&IRId> {
        self.name_scopes
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
        for scope in &mut self.name_scopes {
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

    pub fn add_decl(&mut self, decl: &syntax::Declaration) -> LoweringResult<IRId> {
        let syntax::Declaration { pattern, expr } = decl;

        // We need to allow recursive bindings.
        // To do so, we can rewrite an id after it has been added.
        let temp_ir_id = self.next_ir_id();
        let (pattern_id, new_names) = self.add_pattern(pattern, &temp_ir_id);
        let scope = self.current_name_scope();

        if let Some(new_names) = new_names {
            for (key, value) in new_names {
                scope.insert(key, value);
            }

            let binding_id = self.next_ir_id();
            self.ir_items.insert(
                binding_id,
                IRItem::Binding {
                    pattern: pattern_id,
                    value: temp_ir_id.clone(),
                },
            );
        }

        let new_ir_id = self.add_expr(expr)?;
        self.rewrite_id_to(&new_ir_id, &temp_ir_id);

        Ok(new_ir_id)
    }

    pub fn add_expr(&mut self, expr: &syntax::Expr) -> Result<IRId, LoweringError> {
        use syntax::Expr;

        let (expr_id, ir_expr) = match expr {
            Expr::Literal(x) => (self.next_ir_id(), IRItem::Literal(x.clone())),
            Expr::Identifier(ident) => {
                return self.find_identifier(ident).cloned().ok_or_else(|| {
                    LoweringError::MissingIdentifier {
                        name: ident.clone(),
                    }
                });
            }
            Expr::List { elements } => {
                let mut list_so_far;
                let mut last_id = Some(self.empty_list_id());

                for item in elements.iter().rev() {
                    list_so_far = IRItem::ListCons {
                        item: self.add_expr(item)?,
                        rest_list: last_id.take().unwrap(),
                    };
                    last_id = Some(self.next_ir_id());
                    self.ir_items
                        .insert(last_id.as_ref().unwrap().clone(), list_so_far);
                }

                return Ok(last_id.unwrap());
            }
            Expr::Tuple { elements } => {
                let mut ir_elements = vec![];
                for elem in elements {
                    ir_elements.push(self.add_expr(elem)?);
                }

                (
                    self.next_ir_id(),
                    IRItem::Tuple {
                        elements: ir_elements,
                    },
                )
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
                            self.ir_items.insert(
                                param_id.clone(),
                                IRItem::Identifier {
                                    name: param.clone(),
                                    declaring_item: Some(param_id.clone()),
                                },
                            );
                        }
                        Pattern::Literal(_) => todo!(),
                        Pattern::Tuple(_) | Pattern::ListCons(_) => todo!(),
                        Pattern::Ignore => {}
                    }
                    let body = self.add_expr(body)?;
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
                    lhs: self.add_expr(lhs)?,
                    rhs: self.add_expr(rhs)?,
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
                    condition: self.add_expr(condition)?,
                    true_value: self.add_expr(true_value)?,
                    false_value: self.add_expr(false_value)?,
                },
            ),
            Expr::Let {
                bound_values,
                inner_expr,
            } => {
                let ir_id = self.next_ir_id();
                let mut bound_names = Scope::new();

                let binding_list = if !bound_values.is_empty() {
                    let mut bindings = vec![];
                    for syntax::Declaration { pattern, expr } in bound_values {
                        let (pattern_id, new_names) = self.add_pattern(pattern, &ir_id);
                        let expr_id = self.add_expr(expr);

                        if let Some(new_names) = new_names {
                            bound_names.extend(new_names);

                            let binding_id = self.next_ir_id();
                            self.ir_items.insert(
                                binding_id.clone(),
                                IRItem::Binding {
                                    pattern: pattern_id,
                                    value: expr_id?,
                                },
                            );
                            bindings.push(binding_id);
                        }
                    }
                    bindings
                } else {
                    Vec::default()
                };

                *self.add_new_name_scope() = bound_names;
                let inner_expr = self.add_expr(inner_expr)?;
                self.hide_top_name_scope();

                (
                    ir_id,
                    IRItem::Let {
                        inner_expr,
                        binding_list,
                    },
                )
            }
            Expr::Match { scrutinee, arms } => {
                let scrutinee = self.add_expr(scrutinee)?;

                let ir_id = self.next_ir_id();
                let mut lowered_arms = vec![];
                for (pattern, expr) in arms {
                    let (pattern, bound_names) = self.add_pattern(pattern, &ir_id);
                    let inner_expr = if let Some(bound_names) = bound_names {
                        let name_scope = self.add_new_name_scope();
                        *name_scope = bound_names;
                        let inner_expr = self.add_expr(expr)?;
                        self.hide_top_name_scope();
                        inner_expr
                    } else {
                        self.add_expr(expr)?
                    };

                    lowered_arms.push((pattern, inner_expr));
                }

                (
                    ir_id,
                    IRItem::Match {
                        scrutinee,
                        arms: lowered_arms,
                    },
                )
            }
            #[allow(unreachable_patterns)]
            _ => todo!("Unimplemented lowering for {expr:?}"),
        };
        self.ir_items.insert(expr_id.clone(), ir_expr);
        Ok(expr_id)
    }

    fn add_pattern(
        &mut self,
        pattern: &syntax::Pattern,
        declaring_item: &IRId,
    ) -> (IRId, Option<Scope>) {
        let mut insert_id_item_data: Option<(_, _)> = None;
        let (pattern, bound_names) = match pattern {
            Pattern::Id(id) => {
                let mut bound_names = HashMap::new();
                let ir_id = self.next_ir_id();
                bound_names.insert(id.clone(), ir_id.clone());
                insert_id_item_data = Some((ir_id.clone(), id));
                (IRPattern::Identifier(ir_id), Some(bound_names))
            }
            Pattern::Ignore => (IRPattern::Ignore, None),
            Pattern::Literal(lit) => (IRPattern::Literal(lit.clone()), None),
            Pattern::Tuple(elements) => {
                let (patterns, bound_names): (Vec<IRId>, Vec<_>) = elements
                    .iter()
                    .map(|elem| self.add_pattern(elem, declaring_item))
                    .unzip();

                (
                    IRPattern::Tuple(patterns),
                    Some(super::utils::join_hashmaps(
                        bound_names.into_iter().flatten().collect(),
                    )),
                )
            }
            Pattern::ListCons(elements) => {
                let (patterns, bound_names): (Vec<IRId>, Vec<_>) = elements
                    .iter()
                    .map(|elem| self.add_pattern(elem, declaring_item))
                    .unzip();

                (
                    IRPattern::ListCons(patterns),
                    Some(super::utils::join_hashmaps(
                        bound_names.into_iter().flatten().collect(),
                    )),
                )
            }
        };

        let ir_id = self.next_ir_id();
        self.ir_items
            .insert(ir_id.clone(), IRItem::Pattern(pattern));

        if let Some((bound_id, id)) = insert_id_item_data {
            self.ir_items.insert(
                bound_id,
                IRItem::Identifier {
                    name: id.to_string(),
                    declaring_item: Some(declaring_item.clone()),
                },
            );
        }

        (ir_id, bound_names)
    }

    fn empty_list_id(&mut self) -> IRId {
        if let Some(id) = self
            .ir_items
            .iter()
            .find_map(|(id, item)| (*item == IRItem::EmptyList).then_some(id.clone()))
        {
            id
        } else {
            let id = self.next_ir_id();
            self.ir_items.insert(id.clone(), IRItem::EmptyList);
            id
        }
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

        let (module, lambda_id) = Module::from_expr(&lambda).unwrap();
        assert_eq!(module.name_scopes.len(), 0);

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
        let lambda = syntax::Declaration::simple_name(
            "f".to_string(),
            syntax::Expr::Lambda {
                parameter: Pattern::Id("x".to_string()),
                body: Box::new(syntax::Expr::Identifier("x".to_string())),
            },
        );

        let module = Module::from_decls(&[lambda]).unwrap();
        assert_eq!(module.name_scopes.len(), 1);

        let f_id = module.find_identifier(&"f".to_string());
        assert_ne!(f_id, None);
        let x_id = module.find_identifier(&"x".to_string());
        assert_eq!(x_id, None);

        println!("{:?}", module);

        let f_ir_item = module.ir_items.get(f_id.unwrap());
        println!("{:?}", f_ir_item);

        let declaring_id: IRId = match f_ir_item {
            Some(IRItem::Identifier {
                name: _,
                declaring_item: Some(source_id),
            }) => source_id.clone(),
            _ => panic!(),
        };

        assert!(match module.ir_items.get(&declaring_id) {
            Some(&IRItem::Lambda {
                ref parameter,
                ref body,
            }) => parameter == body,
            _ => false,
        });
    }

    #[test]
    fn lowers_multiple() {
        let x_bind = syntax::Declaration::simple_name(
            "x".to_string(),
            syntax::Expr::Literal(Literal::Integer(5)),
        );

        let mut module = Module::from_decls(&[x_bind]).unwrap();
        assert_eq!(module.name_scopes.len(), 1);

        let f_id = module.find_identifier(&"f".to_string());
        assert_eq!(f_id, None);
        let x_id = module.find_identifier(&"x".to_string());
        assert_ne!(x_id, None);
        let x_id = x_id.cloned().unwrap();

        println!("{:?}", module);

        let sum_id = module
            .add_expr(&syntax::Expr::Binary {
                op: BinaryOperation::Plus,
                lhs: Box::new(syntax::Expr::Identifier("x".to_string())),
                rhs: Box::new(syntax::Expr::Literal(Literal::Integer(10))),
            })
            .unwrap();

        let sum_ir = module.get_item(&sum_id).cloned().unwrap();
        if let IRItem::Binary { lhs, .. } = sum_ir {
            assert_eq!(lhs, x_id);
        } else {
            panic!();
        }
    }

    #[test]
    fn lowers_recursion() {
        let x_bind = syntax::Declaration::simple_name(
            "x".to_string(),
            syntax::Expr::Identifier("x".to_string()),
        );

        let module = Module::from_decls(&[x_bind]).unwrap();
        assert_eq!(module.name_scopes.len(), 1);

        let f_id = module.find_identifier(&"f".to_string());
        assert_eq!(f_id, None);
        let x_id = module.find_identifier(&"x".to_string());
        assert_ne!(x_id, None);

        println!("{:?}", module);
    }

    #[test]
    fn lowers_subfunction() {
        let inner_lambda = syntax::Declaration::simple_name(
            "inner".to_string(),
            syntax::Expr::Lambda {
                parameter: Pattern::Id("x".to_string()),
                body: Box::new(syntax::Expr::Identifier("x".to_string())),
            },
        );

        let outer_lambda = syntax::Declaration::simple_name(
            "outer".to_string(),
            syntax::Expr::Lambda {
                parameter: Pattern::Id("x".to_string()),
                body: Box::new(syntax::Expr::Let {
                    inner_expr: Box::new(syntax::Expr::Binary {
                        op: syntax::BinaryOperation::Application,
                        rhs: Box::new(syntax::Expr::Identifier("x".to_string())),
                        lhs: Box::new(syntax::Expr::Identifier("inner".to_string())),
                    }),
                    bound_values: vec![inner_lambda],
                }),
            },
        );

        let module = Module::from_decls(&[outer_lambda]).unwrap();
        assert_eq!(module.name_scopes.len(), 1);
        println!("{:?}", module);

        let x_id = module.find_identifier(&"x".to_string());
        assert_eq!(x_id, None);
        let inner_id = module.find_identifier(&"inner".to_string());
        assert_eq!(inner_id, None);
        let outer_id = module.find_identifier(&"outer".to_string());
        assert_ne!(outer_id, None);

        let outer_ir_item = module.ir_items.get(outer_id.unwrap());
        println!("{:?}", outer_ir_item);

        let declaring_id: IRId = match outer_ir_item {
            Some(IRItem::Identifier {
                name: _,
                declaring_item: Some(source_id),
            }) => source_id.clone(),
            _ => panic!(),
        };

        assert!(match module.ir_items.get(&declaring_id) {
            Some(&IRItem::Lambda { .. }) => true,
            _ => false,
        });
    }

    #[test]
    fn lowers_ignore() {
        let const_func = syntax::Declaration::simple_name(
            "const".to_string(),
            syntax::Expr::Lambda {
                parameter: syntax::Pattern::Id("x".to_string()),
                body: Box::new(syntax::Expr::Lambda {
                    parameter: syntax::Pattern::Ignore,
                    body: Box::new(syntax::Expr::Identifier("x".to_string())),
                }),
            },
        );

        let module = Module::from_decls(&[const_func]).unwrap();
        assert_eq!(module.name_scopes.len(), 1);
        println!("{:?}", module);

        let x_id = module.find_identifier(&"x".to_string());
        assert_eq!(x_id, None);
    }

    #[test]
    fn lowers_two_declaractions() {
        let x_decl = syntax::Declaration::simple_name(
            "x".to_string(),
            syntax::Expr::Literal(Literal::Integer(5)),
        );

        let mut module = Module::new();
        let x_id = module.add_decl(&x_decl).unwrap();

        let y_decl = syntax::Declaration::simple_name(
            "y".to_string(),
            syntax::Expr::Literal(Literal::Integer(10)),
        );

        let y_id = module.add_decl(&y_decl).unwrap();

        let addition = syntax::Expr::Binary {
            lhs: Box::new(syntax::Expr::Identifier("x".to_string())),
            rhs: Box::new(syntax::Expr::Identifier("y".to_string())),
            op: BinaryOperation::Plus,
        };

        let add_id = module.add_expr(&addition).unwrap();

        dbg!(module.get_item(&add_id), &x_id, &y_id, &module);

        if let Some(IRItem::Binary {
            lhs,
            rhs,
            op: BinaryOperation::Plus,
        }) = module.get_item(&add_id)
        {
            if let (
                Some(IRItem::Identifier {
                    declaring_item: Some(lhs_decl),
                    ..
                }),
                Some(IRItem::Identifier {
                    declaring_item: Some(rhs_decl),
                    ..
                }),
            ) = (module.get_item(lhs), module.get_item(rhs))
            {
                assert_eq!(*lhs_decl, x_id);
                assert_eq!(*rhs_decl, y_id);
            } else {
                panic!()
            }
        } else {
            panic!()
        }
    }
}
