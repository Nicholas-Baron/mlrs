use crate::ir_tree::{IRId, IRItem, Module};
use crate::syntax::{BinaryOperation, Identifier, Literal};

use std::collections::HashMap;

#[derive(Debug)]
enum ExecValue {
    Literal(Literal),
    Identifier(Identifier),
}

pub struct ExecContext {
    module: Module,
    named_values: Vec<HashMap<Identifier, ExecValue>>,
    evaluation_stack: Vec<ExecValue>,
}

impl ExecContext {
    pub fn new(module: Module) -> Self {
        Self {
            module,
            named_values: vec![],
            evaluation_stack: vec![],
        }
    }

    pub fn execute(&mut self) -> Literal {
        let start_id = self.module.root_id().cloned().unwrap();
        self.execute_id(&start_id);
        match self.evaluation_stack.pop() {
            Some(ExecValue::Literal(lit)) => lit,
            _ => panic!(),
        }
    }

    fn find_value(&self, name: &Identifier) -> Option<&ExecValue> {
        self.named_values.iter().rev().find_map(|map| map.get(name))
    }

    fn execute_id(&mut self, id: &IRId) {
        let ir_item = self.module.get_item(id).cloned();
        match ir_item.unwrap() {
            IRItem::Literal(l) => self.evaluation_stack.push(ExecValue::Literal(l.clone())),
            IRItem::Identifier(id) => self
                .evaluation_stack
                .push(ExecValue::Identifier(id.clone())),
            IRItem::Binary { lhs, rhs, op } => match op {
                BinaryOperation::Plus => {
                    self.execute_id(&lhs);
                    let lhs_val = self.evaluation_stack.pop();

                    self.execute_id(&rhs);
                    let rhs_val = self.evaluation_stack.pop();

                    match (lhs_val, rhs_val) {
                        (Some(l), Some(r)) => self.add(l, r),
                        _ => panic!(),
                    }
                }
                BinaryOperation::Application => {
                    self.execute_id(&rhs);
                    self.execute_id(&lhs);
                }
            },
            IRItem::Lambda { parameter, body } => {
                self.named_values.push(HashMap::default());
                self.execute_id(&parameter);
                let id = if let Some(ExecValue::Identifier(id)) = self.evaluation_stack.pop() {
                    id
                } else {
                    panic!()
                };
                self.named_values
                    .last_mut()
                    .map(|map| map.insert(id, self.evaluation_stack.pop().unwrap()));
                self.execute_id(&body);
            }
        }
    }
    fn unpack_exec(&self, val: &ExecValue) -> i64 {
        match val {
            ExecValue::Literal(lit) => match lit {
                Literal::Integer(val) => *val,
                Literal::Boolean(_) => panic!(),
            },
            ExecValue::Identifier(id) => self.unpack_exec(self.find_value(&id).unwrap()),
        }
    }

    fn add(&mut self, l: ExecValue, r: ExecValue) {
        let lhs_val = self.unpack_exec(&l);
        let rhs_val = self.unpack_exec(&r);
        self.evaluation_stack
            .push(ExecValue::Literal(Literal::Integer(lhs_val + rhs_val)));
    }
}
