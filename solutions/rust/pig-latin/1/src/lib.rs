fn is_vowel_or_y(a: char) -> bool {
    matches!(a, 'a' | 'e' | 'i' | 'o' | 'u' | 'y')
}

fn is_ascii_vowel(a: u8) -> bool {
    matches!(a, b'a' | b'e' | b'i' | b'o' | b'u')
}

fn starts_with_vowel(input: &str) -> bool {
    !input.is_empty() && is_ascii_vowel(input.as_bytes()[0])
}

pub fn translate(input: &str) -> String {
    input
        .split_ascii_whitespace()
        .map(translate_word)
        .collect::<Vec<String>>()
        .join(" ")
}

fn translate_word(input: &str) -> String {
    // rule 1
    if starts_with_vowel(input)
        || input.starts_with("xr")
        || input.starts_with("yt")
    {
        return input.to_string() + "ay";
    }

    // rules 2 and 4
    let consonants_count = if let Some(input1) = input.strip_prefix("y") {
        1 + input1.find(is_vowel_or_y).expect("at least one vowel")
    } else {
        input.find(is_vowel_or_y).expect("at least one vowel")
    };
    if consonants_count > 0 {
        // rule 3
        let consonants_count = consonants_count
            + if &input[consonants_count - 1..consonants_count + 1] == "qu" {
                1
            } else {
                0
            };

        // back to rule 2
        return input[consonants_count..].to_string()
            + &input[0..consonants_count]
            + "ay";
    }

    // fallback
    input.to_string()
}
