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

    pub fn add_decl(&mut self, decl: &syntax::Declaration) -> LoweringResult<IRId> {
        use syntax::Declaration;
        match decl {
            Declaration::PatternBinding { pattern, expr } => {
                // We need to allow recursive bindings.
                // To do so, we can rewrite an id after it has been added.
                let binding_id = self.next_ir_id();
                let (pattern_id, new_names) = self.add_pattern(pattern, &binding_id);

                self.current_name_scope()
                    .extend(new_names.unwrap_or_default());

                let new_ir_id = self.add_expr(expr)?;

                self.ir_items.insert(
                    binding_id.clone(),
                    IRItem::Binding {
                        pattern: pattern_id,
                        value: new_ir_id,
                    },
                );

                Ok(binding_id)
            }
            Declaration::Function { name, clauses } => {
                let binding_id = self.next_ir_id();

                self.current_name_scope()
                    .insert(name.clone(), binding_id.clone());

                let scope_count = self.name_scopes.len();

                let mut lambda_ids = vec![];
                let mut parameter_ids = vec![];

                if clauses.len() == 1 {
                    let (params, body) = clauses.first().unwrap();

                    for param in params {
                        let lambda_id = self.next_ir_id();
                        let (parameter, names) = self.add_pattern(param, &lambda_id);
                        self.add_new_name_scope().extend(names.unwrap_or_default());
                        parameter_ids.push(parameter);
                        lambda_ids.push(lambda_id);
                    }

                    let super_body = parameter_ids.iter().zip(lambda_ids).rfold(
                        self.add_expr(body)?,
                        |body, (parameter, lambda)| {
                            let inner_lambda = IRItem::Lambda {
                                parameter: parameter.clone(),
                                body,
                            };
                            self.ir_items.insert(lambda.clone(), inner_lambda);
                            self.hide_top_name_scope();
                            lambda
                        },
                    );

                    let (name_id, _) =
                        self.add_pattern(&syntax::Pattern::Id(name.clone()), &binding_id);

                    self.ir_items.insert(
                        binding_id.clone(),
                        IRItem::Binding {
                            pattern: name_id,
                            value: super_body,
                        },
                    );

                    assert_eq!(scope_count, self.name_scopes.len());

                    return Ok(binding_id);
                }

                let params_by_clause: Vec<_> = clauses.iter().map(|(params, _)| params).collect();

                let param_count: usize = {
                    use std::collections::HashSet;
                    let param_counts: HashSet<_> =
                        params_by_clause.iter().map(|x| x.len()).collect();

                    if param_counts.len() != 1 {
                        return Err(LoweringError::DifferentParamCounts {
                            name: name.clone(),
                            counts: param_counts,
                        });
                    } else {
                        param_counts.into_iter().next().unwrap()
                    }
                };

                for _ in 0..param_count {
                    parameter_ids.push(self.next_ir_id());
                    lambda_ids.push(self.next_ir_id());
                }

                let scrutinee = IRItem::Tuple {
                    elements: parameter_ids.clone(),
                };
                let scrutinee_id = self.next_ir_id();
                self.ir_items.insert(scrutinee_id.clone(), scrutinee);

                let match_id = self.next_ir_id();
                let mut arms = vec![];
                for (params, body) in clauses {
                    let (pattern_id, names) =
                        self.add_pattern(&syntax::Pattern::Tuple(params.clone()), &match_id);

                    self.add_new_name_scope().extend(names.unwrap_or_default());

                    let body_id = self.add_expr(body)?;
                    arms.push((pattern_id, body_id));
                    self.hide_top_name_scope();
                }

                let match_expr = IRItem::Match {
                    scrutinee: scrutinee_id,
                    arms,
                };
                self.ir_items.insert(match_id.clone(), match_expr);

                let super_body = parameter_ids.iter().zip(lambda_ids).rfold(
                    match_id,
                    |body, (parameter, lambda)| {
                        let inner_lambda = IRItem::Lambda {
                            parameter: parameter.clone(),
                            body,
                        };
                        self.ir_items.insert(
                            parameter.clone(),
                            IRItem::Pattern(IRPattern::Identifier(parameter.clone())),
                        );
                        self.ir_items.insert(lambda.clone(), inner_lambda);
                        lambda
                    },
                );

                let (name_id, _) =
                    self.add_pattern(&syntax::Pattern::Id(name.clone()), &binding_id);

                self.ir_items.insert(
                    binding_id.clone(),
                    IRItem::Binding {
                        pattern: name_id,
                        value: super_body,
                    },
                );

                assert_eq!(scope_count, self.name_scopes.len());

                Ok(binding_id)
            }
        }
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

            Expr::Lambda { body, parameter } => {
                let lambda_id = self.next_ir_id();

                // NOTE: The initalization order here matters.
                // The parameter needs to be added *before* the body is processed.
                // We also need to remove the scope after we are done.

                let (pattern_id, new_names) = self.add_pattern(parameter, &lambda_id);

                let name_scope = self.add_new_name_scope();
                if let Some(names) = new_names {
                    name_scope.extend(names);
                }

                let body = self.add_expr(body)?;
                self.hide_top_name_scope();

                (
                    lambda_id,
                    IRItem::Lambda {
                        parameter: pattern_id,
                        body,
                    },
                )
            }
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
                self.add_new_name_scope();
                let mut binding_list = vec![];

                for decl in bound_values {
                    // `add_decl` uses the current scope
                    binding_list.push(self.add_decl(decl)?);
                }

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
            Expr::Dictionary(entries) => {
                let id = self.next_ir_id();

                let mut map = HashMap::new();

                for (key, value) in entries {
                    map.insert(self.add_expr(key)?, self.add_expr(value)?);
                }

                (id, IRItem::Dictionary(map))
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
            Pattern::EmptyList => (IRPattern::EmptyList, None),
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

    pub fn write_graphviz_to<T: std::io::Write>(&self, dest: &mut T) -> std::io::Result<()> {
        writeln!(dest, "digraph {{")?;

        for (id, item) in self.ir_items.iter() {
            match item {
                IRItem::Literal(lit) => writeln!(dest, "{} [label = \"{}\"];", id.0, lit),
                IRItem::Identifier {
                    name,
                    declaring_item,
                } => {
                    if let Some(declarer) = declaring_item {
                        writeln!(
                            dest,
                            "{} [label = \"{}\"];\n{} -> {} [style = \"dashed\"];",
                            id.0, name, id.0, declarer.0
                        )
                    } else {
                        writeln!(dest, "{} [label = \"{}\"];", id.0, name)
                    }
                }
                IRItem::Pattern(pat) => {
                    writeln!(dest, "{} [label = \"{:?}\"];", id.0, pat)?;
                    match pat {
                        IRPattern::Literal(_) | IRPattern::EmptyList | IRPattern::Ignore => Ok(()),
                        IRPattern::Identifier(x) => writeln!(dest, "{} -> {};", id.0, x.0),
                        IRPattern::Tuple(elements) | IRPattern::ListCons(elements) => elements
                            .iter()
                            .try_for_each(|elem| writeln!(dest, "{} -> {};", id.0, elem.0)),
                    }
                }
                IRItem::Tuple { elements } => {
                    for elem in elements {
                        writeln!(dest, "{} -> {};", id.0, elem.0)?;
                    }
                    writeln!(dest, "{} [label = \"tuple\"]", id.0)
                }
                IRItem::EmptyList => writeln!(dest, "{} [label = \"[]\"];", id.0),
                IRItem::ListCons { item, rest_list } => {
                    writeln!(dest, "{} [label = \":\"];", id.0)?;
                    writeln!(dest, "{} -> {};", id.0, item.0)?;
                    writeln!(dest, "{} -> {};", id.0, rest_list.0)
                }
                IRItem::Lambda { parameter, body } => {
                    writeln!(dest, "{} [label = \"lambda\"];", id.0)?;
                    writeln!(dest, "{} -> {};", id.0, parameter.0)?;
                    writeln!(dest, "{} -> {};", id.0, body.0)
                }
                IRItem::Binary { lhs, rhs, op } => {
                    writeln!(dest, "{} [label = \"{:?}\"];", id.0, op)?;
                    writeln!(dest, "{} -> {};", id.0, lhs.0)?;
                    writeln!(dest, "{} -> {};", id.0, rhs.0)
                }
                IRItem::If {
                    condition,
                    true_value,
                    false_value,
                } => {
                    writeln!(dest, "{} [label = \"if\"];", id.0)?;
                    writeln!(dest, "{} -> {};", id.0, condition.0)?;
                    writeln!(dest, "{} -> {};", id.0, true_value.0)?;
                    writeln!(dest, "{} -> {};", id.0, false_value.0)
                }
                IRItem::Match { scrutinee, arms } => {
                    writeln!(dest, "{} [label = \"match\"];", id.0)?;
                    for (i, (pattern, value)) in arms.iter().enumerate() {
                        let arm = format!("\"{}_arm{}\"", id.0, i);
                        writeln!(dest, "{} -> {};", id.0, arm)?;
                        writeln!(dest, "{} -> {} [label = \"pattern\"];", arm, pattern.0)?;
                        writeln!(dest, "{} -> {} [label = \"value\"];", arm, value.0)?;
                    }
                    writeln!(dest, "{} -> {} [label = \"scrutinee\"];", id.0, scrutinee.0)
                }
                IRItem::Binding { pattern, value } => {
                    writeln!(dest, "{} [label = \"binding\"];", id.0)?;
                    writeln!(dest, "{} -> {} [label = \"pattern\"];", id.0, pattern.0)?;
                    writeln!(dest, "{} -> {} [label = \"value\"];", id.0, value.0)
                }
                IRItem::Let {
                    binding_list,
                    inner_expr,
                } => {
                    writeln!(dest, "{} [label = \"let\"];", id.0)?;
                    for binding in binding_list {
                        writeln!(dest, "{} -> {};", id.0, binding.0)?;
                    }
                    writeln!(dest, "{} -> {} [label = \"value\"];", id.0, inner_expr.0)
                }
                IRItem::Dictionary(map) => {
                    writeln!(dest, "{} [label = \"let\"];", id.0)?;
                    for (key, value) in map {
                        writeln!(dest, "{} -> {};", id.0, key.0)?;
                        writeln!(dest, "{} -> {};", id.0, value.0)?;
                    }
                    Ok(())
                }
            }?;
        }

        writeln!(dest, "}}")
    }
}
