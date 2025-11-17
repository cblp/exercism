trait ToPrettyString {
    fn to_pretty_string(&self) -> String;
}

impl ToPrettyString for u32 {
    fn to_pretty_string(&self) -> String {
        match self {
            0 => "no".to_string(),
            1 => "one".to_string(),
            2 => "two".to_string(),
            3 => "three".to_string(),
            4 => "four".to_string(),
            5 => "five".to_string(),
            6 => "six".to_string(),
            7 => "seven".to_string(),
            8 => "eight".to_string(),
            9 => "nine".to_string(),
            10 => "ten".to_string(),
            n => format!("{n}"),
        }
    }
}

trait ToTitleCase {
    fn to_title_case(&self) -> String;
}

impl ToTitleCase for String {
    fn to_title_case(&self) -> String {
        let mut c = self.chars();
        match c.next() {
            None => String::new(),
            Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
        }
    }
}

fn verse(bottles: u32) -> String {
    let bottles_str = bottles.to_pretty_string().to_title_case();
    let bottles_next_str = if bottles - 1 == 0 {
        "no".to_string()
    } else {
        (bottles - 1).to_pretty_string()
    };
    let bottle_word = if bottles == 1 { "bottle" } else { "bottles" };
    let bottle_next_word = if bottles - 1 == 1 {
        "bottle"
    } else {
        "bottles"
    };

    format!(
        "{bottles_str} green {bottle_word} hanging on the wall,\n\
        {bottles_str} green {bottle_word} hanging on the wall,\n\
        And if one green bottle should accidentally fall,\n\
        There'll be {bottles_next_str} green {bottle_next_word} \
        hanging on the wall."
    )
}

pub fn recite(start_bottles: u32, take_down: u32) -> String {
    (0..take_down)
        .map(|i| verse(start_bottles - i))
        .collect::<Vec<String>>()
        .join("\n\n")
}
