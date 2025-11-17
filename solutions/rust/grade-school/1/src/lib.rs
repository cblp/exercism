use std::collections::{BTreeMap, BTreeSet};

pub struct School {
    grades: BTreeMap<u32, BTreeSet<String>>,
}

impl School {
    pub fn new() -> Self {
        Self {
            grades: BTreeMap::new(),
        }
    }

    pub fn add(&mut self, grade: u32, student: &str) {
        if !self
            .grades
            .values()
            .any(|students| students.contains(student))
        {
            self.grades
                .entry(grade)
                .and_modify(|students| {
                    students.insert(student.to_string());
                })
                .or_insert_with(|| BTreeSet::from([student.to_string()]));
        }
    }

    pub fn grades(&self) -> Vec<u32> {
        self.grades.keys().copied().collect()
    }

    pub fn grade(&self, grade: u32) -> Vec<String> {
        match self.grades.get(&grade) {
            None => vec![],
            Some(students) => students.iter().cloned().collect(),
        }
    }
}
