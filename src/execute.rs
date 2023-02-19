use crate::ir_tree::{IRId, IRItem, IRPattern, Module};
use crate::syntax::{BinaryOperation, Literal};

use std::collections::HashMap;
use std::fmt::Display;

type Environment = HashMap<IRId, Expr>;

#[derive(Debug, Clone)]
enum Expr {
    Literal(Literal),
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
            Expr::Closure { .. } => EvaluationResult::NonLiteral,
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
        ident @ IRItem::Identifier(_) => {
            unreachable!("tried to evaluate {ident:?} that is not in {environment:?}")
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
                    match_pattern(module, &scrutinee, &pattern).map(|env| (expr, env))
                })
                .expect("match expression was not exhaustive");

            eval(module, selected_arm, &env)
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
        (_, IRPattern::Ignore) => Some(Default::default()),
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
            environment.insert(parameter, rhs);
            eval(module, body, &environment)
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
                BinaryOperation::Plus => Expr::Literal(Literal::Integer(lhs + rhs)),
                // TODO: Print panic message
                BinaryOperation::Prepend | BinaryOperation::Concat => panic!(),
            }
        }
        (BinaryOperation::Concat, Expr::EmptyList, Expr::EmptyList) => Expr::EmptyList,
        (BinaryOperation::Prepend, lhs, rhs) => Expr::ListCons {
            item: Box::new(lhs),
            rest: Box::new(rhs),
        },
        prim_op => todo!("cannot evaluate_prim{prim_op:?}"),
    }
}
