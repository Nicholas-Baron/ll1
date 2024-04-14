use std::collections::HashMap;

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Identifier(usize);

#[derive(Debug, Default)]
pub struct IdentifierMap(HashMap<Identifier, String>);

impl IdentifierMap {
    pub fn add_identifier(&mut self, word: String) -> Identifier {
        if let Some(k) = self.0.iter().find_map(|(k, v)| (*v == word).then_some(k)) {
            k.clone()
        } else {
            let new_id = (0..=self.0.len())
                .map(Identifier)
                .find(|k| !self.0.contains_key(k))
                .unwrap();
            self.0.insert(new_id.clone(), word);
            new_id
        }
    }
}
