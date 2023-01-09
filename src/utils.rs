use std::collections::HashMap;
use std::hash::Hash;

pub fn join_hashmaps<K, V>(bindings: Vec<Option<HashMap<K, V>>>) -> Option<HashMap<K, V>>
where
    K: Eq + Hash + std::fmt::Debug,
{
    bindings
        .into_iter()
        .fold(None, |acc, binding_set| match (acc, binding_set) {
            (None, None) => None,
            (None, binding_set @ Some(_)) => binding_set,
            (acc @ Some(_), None) => acc,
            (Some(mut acc), Some(mut binding_set)) => {
                for (name, id) in binding_set.drain() {
                    if acc.contains_key(&name) {
                        eprintln!("Found duplicate name binding in pattern: {name:?}");
                    } else {
                        acc.insert(name, id);
                    }
                }
                Some(acc)
            }
        })
}
