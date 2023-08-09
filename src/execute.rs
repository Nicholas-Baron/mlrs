use crate::ir_tree::{IRId, IRItem, IRPattern, Module};
use crate::syntax::{BinaryOperation, Literal};

use std::collections::HashMap;
use std::fmt::Display;

type Environment = HashMap<IRId, Expr>;

#[derive(Debug, Clone)]
enum Expr {
    Literal(Literal),
    BindingSet(Environment),
    Suspend((IRId, Environment)),
    Closure {
        parameter: IRId,
        body: IRId,
        environment: Environment,
    },
    Tuple(Vec<Expr>),
    EmptyList,
    ListCons {
        item: Box<Expr>,
        rest: Box<Expr>,
    },
}

impl Expr {
    fn fully_evaluate(self, module: &Module, environment: &Environment) -> EvaluationResult {
        match self {
            Expr::Literal(l) => EvaluationResult::Literal(l),
            Expr::Suspend((id, env)) => eval(module, id, &env).fully_evaluate(module, &env),
            Expr::BindingSet(_) | Expr::Closure { .. } => EvaluationResult::NonLiteral,
            Expr::Tuple(items) => EvaluationResult::Tuple(
                items
                    .into_iter()
                    .map(|e| e.fully_evaluate(module, environment))
                    .collect(),
            ),
            Expr::EmptyList => EvaluationResult::List(vec![]),
            Expr::ListCons { item, rest } => {
                let mut tail = if let EvaluationResult::List(tail) =
                    rest.fully_evaluate(module, environment)
                {
                    tail
                } else {
                    panic!()
                };

                tail.reverse();
                tail.push(item.fully_evaluate(module, environment));
                tail.reverse();
                EvaluationResult::List(tail)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum EvaluationResult {
    NonLiteral,
    Literal(Literal),
    Tuple(Vec<EvaluationResult>),
    List(Vec<EvaluationResult>),
}

impl Display for EvaluationResult {
    fn fmt(&self, fmtr: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            EvaluationResult::NonLiteral => fmtr.write_str("_"),
            EvaluationResult::Literal(lit) => fmtr.write_fmt(format_args!("{}", lit)),
            EvaluationResult::Tuple(tup) => {
                fmtr.write_str("(")?;
                let mut first = true;
                for elem in tup {
                    if first {
                        first = false;
                    } else {
                        fmtr.write_str(", ")?;
                    }
                    fmtr.write_fmt(format_args!("{}", elem))?;
                }
                fmtr.write_str(")")
            }
            EvaluationResult::List(elements) => {
                fmtr.write_str("[")?;
                let mut first = true;
                for elem in elements {
                    if first {
                        first = false;
                    } else {
                        fmtr.write_str(", ")?;
                    }
                    fmtr.write_fmt(format_args!("{}", elem))?;
                }
                fmtr.write_str("]")
            }
        }
    }
}

pub fn evaluate_id(module: &Module, id: IRId) -> EvaluationResult {
    let environment = Environment::default();
    eval(module, id, &environment).fully_evaluate(module, &environment)
}

fn eval(module: &Module, id: IRId, environment: &Environment) -> Expr {
    if let Some(exp) = environment.get(&id).cloned() {
        return exp;
    }

    match module.get_item(&id).cloned().unwrap() {
        IRItem::Identifier {
            declaring_item: Some(ref decl_id),
            ..
        } => {
            if let Some(Expr::Suspend((ir_id, environment))) = environment.get(decl_id) {
                eval(module, ir_id.clone(), environment)
            } else {
                match eval(module, decl_id.clone(), environment) {
                    Expr::BindingSet(bindings) => bindings.get(&id).unwrap().clone(),
                    e => e,
                }
            }
        }
        ident @ IRItem::Identifier {
            declaring_item: None,
            ..
        } => {
            unreachable!("tried to evaluate {ident:?} that is not in {environment:?} and has no declaring_item")
        }
        pattern @ IRItem::Pattern(_) => {
            unreachable!("Tried to evaluate {pattern:?}")
        }
        IRItem::EmptyList => Expr::EmptyList,
        IRItem::ListCons { item, rest_list } => Expr::ListCons {
            item: Box::new(Expr::Suspend((item, environment.clone()))),
            rest: Box::new(Expr::Suspend((rest_list, environment.clone()))),
        },
        IRItem::Tuple { elements } => Expr::Tuple(
            elements
                .into_iter()
                .map(|e| eval(module, e, environment))
                .collect(),
        ),
        IRItem::Literal(lit) => Expr::Literal(lit),
        IRItem::Lambda { parameter, body } => Expr::Closure {
            parameter,
            body,
            environment: environment.clone(),
        },
        IRItem::If {
            condition,
            true_value,
            false_value,
        } => {
            let result = eval(module, condition, environment);
            if let Expr::Literal(Literal::Boolean(b)) = result {
                if b {
                    eval(module, true_value, environment)
                } else {
                    eval(module, false_value, environment)
                }
            } else {
                panic!("{result:?} should be a Literal(Boolean)");
            }
        }
        IRItem::Binary { op, lhs, rhs } => match op {
            BinaryOperation::Application => {
                let lhs = eval(module, lhs, environment);
                apply(module, lhs, Expr::Suspend((rhs, environment.clone())))
            }
            op => evaluate_prim(
                module,
                op,
                eval(module, lhs, environment),
                eval(module, rhs, environment),
            ),
        },
        IRItem::Match { scrutinee, arms } => {
            let scrutinee = eval(module, scrutinee, environment);
            let (selected_arm, env) = arms
                .into_iter()
                .find_map(|(pattern, expr)| {
                    let pattern = match module.get_item(&pattern) {
                        Some(IRItem::Pattern(pattern)) => pattern,
                        _ => panic!(),
                    };
                    match_pattern(module, &scrutinee, pattern).map(|env| (expr, env))
                })
                .expect("match expression was not exhaustive");

            eval(module, selected_arm, &env)
        }
        IRItem::Binding { pattern, value } => {
            let pattern = match module.get_item(&pattern) {
                Some(IRItem::Pattern(pattern)) => pattern,
                _ => panic!(),
            };

            match match_pattern(module, &eval(module, value, environment), pattern) {
                Some(new_env) => Expr::BindingSet(new_env),
                None => panic!(),
            }
        }
        IRItem::Let {
            binding_list,
            inner_expr,
        } => {
            let mut new_env = environment.clone();
            for binding_id in binding_list {
                let IRItem::Binding { pattern, value } = module.get_item(&binding_id).unwrap() else {panic!("let's binding_list contained a non-binding")};

                let IRItem::Pattern(pattern)= module.get_item(pattern).unwrap()else{panic!("binding does not have a pattern lhs")};

                new_env.extend(
                    match_pattern(module, &eval(module, value.clone(), environment), pattern)
                        .unwrap_or_default(),
                );
            }

            eval(module, inner_expr, &new_env)
        }
    }
}

/// `None` when no match occurs.
/// `Some(env)` when a match connects, with `env` containing the environment
fn match_pattern(module: &Module, scrutinee: &Expr, pattern: &IRPattern) -> Option<Environment> {
    match (scrutinee, pattern) {
        (Expr::Suspend((expr, env)), _) => {
            match_pattern(module, &eval(module, expr.clone(), env), pattern)
        }
        (Expr::EmptyList, IRPattern::EmptyList) | (_, IRPattern::Ignore) => {
            Some(Default::default())
        }
        // Pattern match a tuple
        (Expr::Tuple(lhs), IRPattern::Tuple(rhs)) => {
            if lhs.len() != rhs.len() {
                return None;
            }
            let bindings: Vec<_> = std::iter::zip(lhs, rhs)
                .map(|(l, r)| {
                    let pattern = match module.get_item(r) {
                        Some(IRItem::Pattern(pattern)) => pattern,
                        _ => panic!(),
                    };
                    match_pattern(module, l, pattern)
                })
                .collect();

            if bindings.iter().any(|binding| binding.is_none()) {
                None
            } else {
                Some(super::utils::join_hashmaps(
                    bindings.into_iter().flatten().collect(),
                ))
            }
        }
        (Expr::Literal(lhs), IRPattern::Literal(rhs)) => (lhs == rhs).then(Default::default),
        (lhs, IRPattern::Identifier(rhs)) => {
            let mut env = Environment::default();
            env.insert(rhs.clone(), lhs.clone());
            Some(env)
        }
        (whole_list @ Expr::ListCons { item, rest }, IRPattern::ListCons(pattern_items)) => {
            if pattern_items.is_empty() {
                todo!("match pattern {pattern_items:?} with {item:?}:{rest:?}");
            }

            let is_base_case = pattern_items.len() == 1;

            let first_item = match_pattern(
                module,
                if is_base_case { whole_list } else { item },
                match module.get_item(&pattern_items[0]) {
                    Some(IRItem::Pattern(pattern)) => pattern,
                    e => panic!("Found {e:?} in a pattern"),
                },
            );

            if is_base_case {
                return first_item;
            }

            // Known: items.len() is at least 2

            let rest_items = match_pattern(
                module,
                rest,
                &IRPattern::ListCons(pattern_items[1..].to_vec()),
            );

            match (first_item, rest_items) {
                (Some(first), Some(rest)) => {
                    Some(super::utils::join_hashmaps([first, rest].to_vec()))
                }
                _ => None,
            }
        }
        (Expr::EmptyList, IRPattern::ListCons(items)) => {
            if items.len() == 1 {
                match_pattern(
                    module,
                    scrutinee,
                    match module.get_item(&items[0]) {
                        Some(IRItem::Pattern(pattern)) => pattern,
                        _ => todo!(),
                    },
                )
            } else {
                None
            }
        }
        _ => None,
    }
}

fn apply(module: &Module, lhs: Expr, rhs: Expr) -> Expr {
    match lhs {
        Expr::Closure {
            parameter,
            body,
            mut environment,
        } => {
            let pattern = match module.get_item(&parameter) {
                Some(IRItem::Pattern(pattern)) => pattern,
                item => panic!("Expected pattern for {parameter:?}; found {item:?}"),
            };

            if let Some(sub_env) = match_pattern(module, &rhs, pattern) {
                environment.extend(sub_env);
            }

            eval(module, body, &environment)
        }
        Expr::Suspend((id, env)) => apply(module, eval(module, id, &env), rhs),
        Expr::BindingSet(bindings) if bindings.len() == 1 => {
            apply(module, bindings.into_iter().next().unwrap().1, rhs)
        }
        _ => todo!("cannot apply {rhs:?} to a lhs of {lhs:?}"),
    }
}

fn evaluate_prim(module: &Module, op: BinaryOperation, lhs: Expr, rhs: Expr) -> Expr {
    match (op, lhs, rhs) {
        (BinaryOperation::Application, _, _) => unreachable!(),
        (op, Expr::Suspend((lhs, env)), rhs) => {
            evaluate_prim(module, op, eval(module, lhs, &env), rhs)
        }
        (op, lhs, Expr::Suspend((rhs, env))) => {
            evaluate_prim(module, op, lhs, eval(module, rhs, &env))
        }
        (BinaryOperation::Equality, Expr::Literal(lhs), Expr::Literal(rhs)) => {
            Expr::Literal(Literal::Boolean(lhs == rhs))
        }
        (op, Expr::Literal(Literal::Integer(lhs)), Expr::Literal(Literal::Integer(rhs))) => {
            match op {
                BinaryOperation::Application => unreachable!(),
                BinaryOperation::Equality => Expr::Literal(Literal::Boolean(lhs == rhs)),
                BinaryOperation::Minus => Expr::Literal(Literal::Integer(lhs - rhs)),
                BinaryOperation::Mult => Expr::Literal(Literal::Integer(lhs * rhs)),
                BinaryOperation::Modulo => Expr::Literal(Literal::Integer(lhs % rhs)),
                BinaryOperation::Plus => Expr::Literal(Literal::Integer(lhs + rhs)),
                // TODO: Print panic message
                BinaryOperation::Prepend | BinaryOperation::Concat => panic!(),
            }
        }
        (BinaryOperation::Concat, Expr::EmptyList, Expr::EmptyList) => Expr::EmptyList,
        (BinaryOperation::Concat, Expr::EmptyList, list @ Expr::ListCons { .. }) => list,
        (
            BinaryOperation::Concat,
            Expr::ListCons {
                item: lhs_item,
                rest: lhs_rest,
            },
            rhs @ Expr::ListCons { .. },
        ) => Expr::ListCons {
            item: lhs_item,
            rest: Box::new(evaluate_prim(
                module,
                BinaryOperation::Concat,
                *lhs_rest,
                rhs,
            )),
        },
        (BinaryOperation::Prepend, lhs, rhs) => Expr::ListCons {
            item: Box::new(lhs),
            rest: Box::new(rhs),
        },
        (BinaryOperation::Equality, Expr::ListCons { .. }, Expr::EmptyList)
        | (BinaryOperation::Equality, Expr::EmptyList, Expr::ListCons { .. }) => {
            Expr::Literal(Literal::Boolean(false))
        }
        (BinaryOperation::Equality, Expr::EmptyList, Expr::EmptyList) => {
            Expr::Literal(Literal::Boolean(true))
        }
        prim_op => todo!("cannot evaluate_prim{prim_op:?}"),
    }
}
