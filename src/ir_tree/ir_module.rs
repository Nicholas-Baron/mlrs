use super::*;

use crate::syntax::{self, Pattern};

#[derive(Default, Debug)]
pub struct Module {
    pub(super) ir_items: HashMap<IRId, IRItem>,
    pub(super) name_scopes: Vec<Scope>,
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
    pub(super) fn find_identifier(&self, identifier: &Identifier) -> Option<&IRId> {
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
                    Some(crate::utils::join_hashmaps(
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
                    Some(crate::utils::join_hashmaps(
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
