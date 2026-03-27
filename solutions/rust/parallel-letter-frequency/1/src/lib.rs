use {
    itertools::Itertools,
    rayon::{iter::ParallelIterator, slice::ParallelSlice},
    std::{collections::HashMap, hash::Hash, ops::AddAssign},
};

fn freq(inputs: &[&str]) -> HashMap<char, usize> {
    inputs
        .iter()
        .flat_map(|input| input.chars())
        .filter(|c| c.is_alphabetic())
        .map(|c| c.to_ascii_lowercase())
        .counts()
}

fn merge<K, V>(mut a: HashMap<K, V>, b: HashMap<K, V>) -> HashMap<K, V>
where
    K: Eq + Hash,
    V: AddAssign + Default,
{
    for (c, n) in b {
        *a.entry(c).or_default() += n;
    }
    a
}

pub fn frequency(inputs: &[&str], worker_count: usize) -> HashMap<char, usize> {
    if inputs.is_empty() {
        return HashMap::new();
    }
    if worker_count == 1 {
        return freq(inputs);
    }
    inputs
        .par_chunks(inputs.len() / worker_count + 1)
        .map(freq)
        .reduce_with(merge)
        .unwrap()
}
