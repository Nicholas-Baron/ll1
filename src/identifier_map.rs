#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Identifier(usize);

#[cfg(test)]
impl Identifier {
    pub fn mock_id(id: usize) -> Self {
        Self(id)
    }
}

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

    pub fn text_for(&self, id: Identifier) -> &str {
        self.0.get(id.0).unwrap()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}
