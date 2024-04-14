use std::collections::HashMap;

#[derive(PartialEq, Eq, Hash)]
pub struct Identifier(usize);

pub struct IdentifierMap(HashMap<Identifier, String>);
