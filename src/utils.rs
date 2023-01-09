use std::collections::HashMap;
use std::hash::Hash;

pub fn join_hashmaps<K, V>(bindings: Vec<HashMap<K, V>>) -> HashMap<K, V>
where
    K: Eq + Hash + std::fmt::Debug,
{
    bindings
        .into_iter()
        .fold(Default::default(), |mut acc, mut binding_set| {
            for (name, id) in binding_set.drain() {
                if acc.contains_key(&name) {
                    eprintln!("Found duplicate name binding in pattern: {name:?}");
                } else {
                    acc.insert(name, id);
                }
            }
            acc
        })
}
