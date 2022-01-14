use crate::ir_tree::{IRId, IRItem, Module};
use crate::syntax::{BinaryOperation, Identifier, Literal};

use std::collections::HashMap;

#[derive(Debug, Clone)]
enum ExecValue {
    Literal(Literal),
    Identifier(Identifier),
    Closure {
        lambda: IRId,
        bound_values: HashMap<Identifier, ExecValue>,
    },
}

#[derive(Debug)]
pub struct ExecContext {
    named_values: Vec<HashMap<Identifier, ExecValue>>,
    evaluation_stack: Vec<ExecValue>,
}

impl ExecContext {
    // TODO: use `Default`
    pub fn new() -> Self {
        Self {
            named_values: vec![],
            evaluation_stack: vec![],
        }
    }

    fn current_bindings(&self) -> HashMap<Identifier, ExecValue> {
        let mut result = HashMap::default();

        for scope in &self.named_values {
            result.extend(scope.clone());
        }

        result
    }

    pub fn execute(&mut self, module: &Module) -> Option<Literal> {
        let start_id = module.root_id().cloned().unwrap();
        self.execute_id(module, &start_id);
        assert_eq!(self.evaluation_stack.len(), 1);
        self.evaluation_stack.pop().and_then(|opt| match opt {
            ExecValue::Literal(lit) => Some(lit),
            _ => None,
        })
    }

    fn find_value(&self, name: &Identifier) -> Option<&ExecValue> {
        self.named_values.iter().rev().find_map(|map| map.get(name))
    }

    fn execute_id(&mut self, module: &Module, id: &IRId) {
        let ir_item = module.get_item(id).cloned();
        match ir_item.unwrap() {
            IRItem::Literal(l) => self.evaluation_stack.push(ExecValue::Literal(l)),
            IRItem::Identifier(id) => self.evaluation_stack.push(ExecValue::Identifier(id)),
            IRItem::Binary { lhs, rhs, op } => match op {
                BinaryOperation::Mult
                | BinaryOperation::Plus
                | BinaryOperation::Equality
                | BinaryOperation::Minus => {
                    self.execute_id(module, &lhs);
                    let lhs_val = self.evaluation_stack.pop();

                    self.execute_id(module, &rhs);
                    let rhs_val = self.evaluation_stack.pop();

                    match (lhs_val, rhs_val) {
                        (Some(l), Some(r)) => self.execute_op(op, l, r),
                        _ => panic!(),
                    }
                }
                BinaryOperation::Application => {
                    self.execute_id(module, &rhs);
                    self.execute_id(module, &lhs);
                }
            },
            IRItem::Lambda { parameter, body } => {
                if self.evaluation_stack.is_empty() {
                    self.evaluation_stack.push(ExecValue::Closure {
                        lambda: id.clone(),
                        bound_values: self.current_bindings(),
                    });
                } else {
                    self.named_values.push(HashMap::default());
                    self.execute_id(module, &parameter);
                    let id = if let Some(ExecValue::Identifier(id)) = self.evaluation_stack.pop() {
                        id
                    } else {
                        panic!()
                    };
                    let value = self.evaluation_stack.pop().unwrap();
                    self.named_values
                        .last_mut()
                        .map(|map| map.insert(id, value));
                    self.execute_id(module, &body);
                    self.named_values.pop();
                }
            }
            IRItem::If {
                condition,
                true_value,
                false_value,
            } => {
                self.execute_id(module, &condition);
                let condition = self.evaluation_stack.pop().unwrap();
                if self.unpack_exec_bool(&condition) {
                    self.execute_id(module, &true_value)
                } else {
                    self.execute_id(module, &false_value)
                }
            }
        }
    }

    fn unpack_exec_bool(&self, val: &ExecValue) -> bool {
        match val {
            ExecValue::Literal(lit) => match lit {
                Literal::Integer(_) => panic!(),
                Literal::Boolean(val) => *val,
            },
            ExecValue::Identifier(id) => self.unpack_exec_bool(self.find_value(id).unwrap()),
            ExecValue::Closure { .. } => panic!(),
        }
    }

    fn unpack_exec_int(&self, val: &ExecValue) -> i64 {
        match val {
            ExecValue::Literal(lit) => match lit {
                Literal::Integer(val) => *val,
                Literal::Boolean(_) => panic!(),
            },
            ExecValue::Identifier(id) => self.unpack_exec_int(self.find_value(id).unwrap()),
            ExecValue::Closure { .. } => panic!(),
        }
    }

    fn execute_op(&mut self, op: BinaryOperation, l: ExecValue, r: ExecValue) {
        let lhs_val = self.unpack_exec_int(&l);
        let rhs_val = self.unpack_exec_int(&r);
        match op {
            BinaryOperation::Plus => self
                .evaluation_stack
                .push(ExecValue::Literal(Literal::Integer(lhs_val + rhs_val))),
            BinaryOperation::Minus => self
                .evaluation_stack
                .push(ExecValue::Literal(Literal::Integer(lhs_val - rhs_val))),
            BinaryOperation::Mult => self
                .evaluation_stack
                .push(ExecValue::Literal(Literal::Integer(lhs_val * rhs_val))),
            BinaryOperation::Equality => self
                .evaluation_stack
                .push(ExecValue::Literal(Literal::Boolean(lhs_val == rhs_val))),
            BinaryOperation::Application => {
                panic!("execute_op should never need to call a function")
            }
        }
    }
}
