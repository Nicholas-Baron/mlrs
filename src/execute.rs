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
}

#[derive(Debug, PartialEq, Eq)]
pub enum EvaluationResult {
    NonLiteral,
    Literal(Literal),
    Tuple(Vec<EvaluationResult>),
}

impl From<Expr> for EvaluationResult {
    fn from(expr: Expr) -> Self {
        match expr {
            Expr::Literal(exp) => EvaluationResult::Literal(exp),
            Expr::Tuple(tuple) => {
                EvaluationResult::Tuple(tuple.into_iter().map(|e| e.into()).collect())
            }
            _ => EvaluationResult::NonLiteral,
        }
    }
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
        }
    }
}

pub fn evaluate_id(module: &Module, id: IRId) -> EvaluationResult {
    let environment = Environment::default();
    eval(module, id, &environment).into()
}

fn eval(module: &Module, id: IRId, environment: &Environment) -> Expr {
    if let Some(exp) = environment.get(&id).cloned() {
        return exp;
    }

    match module.get_item(&id).cloned().unwrap() {
        ident @ IRItem::Identifier(_) => {
            unreachable!("tried to evaluate {ident:?} that is not in {environment:?}")
        }
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
            let selected = arms
                .into_iter()
                .filter(|(pattern, _)| match_pattern(&scrutinee, pattern))
                .next();

            eval(module, selected.unwrap().1, environment)
        }
    }
}

fn match_pattern(scrutinee: &Expr, pattern: &IRPattern) -> bool {
    todo!()
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
            }
        }
        prim_op => todo!("cannot evaluate_prim{prim_op:?}"),
    }
}
