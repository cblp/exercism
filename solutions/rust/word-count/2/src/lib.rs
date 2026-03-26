use std::collections::HashMap;

/// Count occurrences of words.
pub fn word_count(words: &str) -> HashMap<String, u32> {
    let mut r = HashMap::new();
    let mut count = |word: &mut String| {
        if !word.is_empty() {
            *r.entry(word.clone()).or_default() += 1;
            word.clear();
        }
    };
    let mut current_word = String::new();
    let mut seen_apostrophe = false;
    for c in words.chars() {
        if c.is_alphanumeric() {
            if seen_apostrophe {
                current_word.push('\'');
                seen_apostrophe = false;
            }
            current_word += &c.to_lowercase().to_string();
        } else if c == '\'' && !seen_apostrophe && !current_word.is_empty() {
            seen_apostrophe = true;
        } else {
            count(&mut current_word);
            seen_apostrophe = false;
        }
    }
    count(&mut current_word);
    r
}
