use std::collections::HashSet;

fn sorted<A: Ord + Clone>(mut v: Vec<A>) -> Vec<A> {
    v.sort();
    v
}

fn to_lowercase(s: &str) -> Vec<char> {
    s.chars().flat_map(char::to_lowercase).collect()
}

pub fn anagrams_for<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    let word_lower = to_lowercase(word);
    let word_sorted = sorted(word_lower.clone());
    possible_anagrams
        .into_iter()
        .map(|a| *a)
        .filter(|a| {
            let a_lower = to_lowercase(a);
            sorted(a_lower.clone()) == word_sorted && a_lower != word_lower
        })
        .collect()
}
