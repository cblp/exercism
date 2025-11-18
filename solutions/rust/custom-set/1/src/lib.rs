use std::cmp::Ordering::{Equal, Greater, Less};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CustomSet<T> {
    data: Vec<T>,
}

impl<T: Clone + Ord> CustomSet<T> {
    pub fn new(input: &[T]) -> Self {
        let mut data = input.to_vec();
        data.sort();
        data.dedup();
        Self { data }
    }

    pub fn contains(&self, element: &T) -> bool {
        self.data.contains(element)
    }

    pub fn add(&mut self, element: T) {
        self.data.push(element);
        self.data.sort();
        self.data.dedup();
    }

    pub fn is_subset(&self, other: &Self) -> bool {
        self.is_empty()
            || !other.is_empty() && {
                let mut i = 0;
                let mut j = 0;
                while i < self.data.len() && j < other.data.len() {
                    match self.data[i].cmp(&other.data[j]) {
                        Less => return false,
                        Equal => {
                            i += 1;
                            j += 1;
                        }
                        Greater => j += 1,
                    }
                }
                true
            }
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn is_disjoint(&self, other: &Self) -> bool {
        let mut i = 0;
        let mut j = 0;
        while i < self.data.len() && j < other.data.len() {
            match self.data[i].cmp(&other.data[j]) {
                Less => i += 1,
                Equal => return false,
                Greater => j += 1,
            }
        }
        true
    }

    #[must_use]
    pub fn intersection(&self, other: &Self) -> Self {
        let mut data = vec![];
        let mut i = 0;
        let mut j = 0;
        while i < self.data.len() && j < other.data.len() {
            match self.data[i].cmp(&other.data[j]) {
                Less => i += 1,
                Equal => {
                    data.push(self.data[i].clone());
                    i += 1;
                    j += 1;
                }
                Greater => j += 1,
            }
        }
        Self { data }
    }

    #[must_use]
    pub fn difference(&self, other: &Self) -> Self {
        if other.is_empty() {
            self.clone()
        } else {
            let mut data = vec![];
            let mut i = 0;
            let mut j = 0;
            while i < self.data.len() && j < other.data.len() {
                match self.data[i].cmp(&other.data[j]) {
                    Less => {
                        data.push(self.data[i].clone());
                        i += 1;
                    }
                    Equal => {
                        i += 1;
                        j += 1;
                    }
                    Greater => j += 1,
                }
            }
            Self { data }
        }
    }

    #[must_use]
    pub fn union(&self, other: &Self) -> Self {
        if self.is_empty() {
            other.clone()
        } else if other.is_empty() {
            self.clone()
        } else {
            let mut data = vec![];
            let mut i = 0;
            let mut j = 0;
            while i < self.data.len() && j < other.data.len() {
                match self.data[i].cmp(&other.data[j]) {
                    Less => {
                        data.push(self.data[i].clone());
                        i += 1;
                    }
                    Equal => {
                        data.push(self.data[i].clone());
                        i += 1;
                        j += 1;
                    }
                    Greater => {
                        data.push(other.data[j].clone());
                        j += 1;
                    }
                }
            }
            Self { data }
        }
    }
}
