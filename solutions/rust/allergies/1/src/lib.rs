pub struct Allergies {
    flags: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Allergen {
    Eggs,
    Peanuts,
    Shellfish,
    Strawberries,
    Tomatoes,
    Chocolate,
    Pollen,
    Cats,
}

impl Allergies {
    pub fn new(flags: u32) -> Self {
        Allergies { flags }
    }

    pub fn is_allergic_to(&self, allergen: &Allergen) -> bool {
        self.flags & (1 << *allergen as u32) != 0
    }

    pub fn allergies(&self) -> Vec<Allergen> {
        use Allergen::*;
        [
            Eggs,
            Peanuts,
            Shellfish,
            Strawberries,
            Tomatoes,
            Chocolate,
            Pollen,
            Cats,
        ]
        .into_iter()
        .enumerate()
        .filter_map(|(i, a)| (self.flags & (1 << i) != 0).then_some(a))
        .collect()
    }
}
