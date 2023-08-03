use crate::syntax::{BinaryOperation, Identifier, Literal};

use std::collections::HashMap;

mod error;
use error::LoweringError;

mod ir_module;
pub use ir_module::Module;

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
    EmptyList,
    Literal(Literal),
    Identifier(IRId),
    Tuple(Vec<IRId>),
    ListCons(Vec<IRId>),
}

impl IRPattern {
    pub fn contains(&self, id: &IRId) -> bool {
        match self {
            IRPattern::Ignore | IRPattern::EmptyList | IRPattern::Literal(_) => false,
            IRPattern::Identifier(x) => x == id,
            IRPattern::Tuple(elements) | IRPattern::ListCons(elements) => elements.contains(id),
        }
    }
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
    pub fn contains(&self, id: &IRId) -> bool {
        match self {
            IRItem::EmptyList | IRItem::Literal(_) | IRItem::Identifier { .. } => false,
            IRItem::Pattern(pattern) => pattern.contains(id),
            IRItem::Tuple { elements } => elements.contains(id),
            IRItem::ListCons { item, rest_list } => item == id || rest_list == id,
            IRItem::Lambda { parameter, body } => parameter == id || body == id,
            IRItem::Binary { lhs, rhs, .. } => lhs == id || rhs == id,
            IRItem::If {
                condition,
                true_value,
                false_value,
            } => [condition, true_value, false_value].contains(&id),
            IRItem::Match { scrutinee, arms } => {
                scrutinee == id || arms.iter().any(|(lhs, rhs)| lhs == id || rhs == id)
            }
            IRItem::Binding { pattern, value } => pattern == id || value == id,
            IRItem::Let {
                binding_list,
                inner_expr,
            } => binding_list.contains(id) || inner_expr == id,
        }
    }

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
                IRPattern::Ignore | IRPattern::EmptyList => {}
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

#[cfg(test)]
mod tests {
    use super::*;

    use crate::syntax::{self, Pattern};

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
            }) => {
                module
                    .ir_items
                    .get(parameter)
                    .map(|pattern| pattern.contains(body))
                    .unwrap_or(false)
            }
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
