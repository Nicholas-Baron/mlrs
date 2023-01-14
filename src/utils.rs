use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::hash::Hash;

pub fn join_hashmaps<K, V>(bindings: Vec<HashMap<K, V>>) -> HashMap<K, V>
where
    K: Eq + Hash + std::fmt::Debug + Clone,
{
    bindings
        .into_iter()
        .fold(Default::default(), |mut acc, mut binding_set| {
            for (name, id) in binding_set.drain() {
                if let Entry::Vacant(entry) = acc.entry(name.clone()) {
                    entry.insert(id);
                } else {
                    eprintln!("Found duplicate name binding in pattern: {name:?}");
                }
            }
            acc
        })
}
