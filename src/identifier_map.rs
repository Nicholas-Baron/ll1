#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Identifier(usize);

#[derive(Debug, Default, PartialEq, Eq)]
pub struct IdentifierMap(Vec<String>);

impl IdentifierMap {
    pub fn add_identifier(&mut self, word: String) -> Identifier {
        self.find_identifier(&word).unwrap_or_else(|| {
            let new_id = Identifier(self.0.len());
            self.0.push(word);
            new_id
        })
    }

    fn find_identifier(&self, word: &str) -> Option<Identifier> {
        self.0.iter().position(|id| id == word).map(Identifier)
    }
}
