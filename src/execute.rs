use crate::ir_tree::{IRId, IRItem, Module};
use crate::syntax::{BinaryOperation, Identifier, Literal};

use std::collections::HashMap;

type Environment = HashMap<IRId, Expr>;

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Identifier(Identifier),
    Suspend((IRId, HashMap<IRId, Expr>)),
    Closure {
        parameter: IRId,
        body: IRId,
        environment: Environment,
    },
}

pub fn execute(module: &Module) -> Option<Literal> {
    let start_id = module.root_id().cloned().unwrap();
    let environment = Environment::default();
    match eval(module, start_id, &environment) {
        Expr::Literal(exp) => Some(exp),
        _ => None,
    }
}

fn eval(module: &Module, id: IRId, environment: &Environment) -> Expr {
    if let Some(exp) = environment.get(&id).cloned() {
        return exp;
    }

    match module.get_item(&id).cloned().unwrap() {
        IRItem::Identifier(name) => match environment.get(&id).cloned() {
            Some(exp) => exp,
            None => Expr::Identifier(name),
        },
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
                environment
                    .get(&lhs)
                    .cloned()
                    .unwrap_or_else(|| eval(module, lhs, environment)),
                environment
                    .get(&rhs)
                    .cloned()
                    .unwrap_or_else(|| eval(module, rhs, environment)),
            ),
        },
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
        _ => todo!(),
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
