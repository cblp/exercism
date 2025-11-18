fn eval_number(word: &str) -> Option<i32> {
    word.parse::<i32>().ok()
}

fn eval(words: &[&str]) -> Option<i32> {
    let n = words.len();
    if let [word] = words {
        return eval_number(word);
    }
    if n < 2 {
        return None;
    }
    match words.split_at(n - 2) {
        (a, ["plus", b]) => return Some(eval(a)? + eval_number(b)?),
        (a, ["minus", b]) => return Some(eval(a)? - eval_number(b)?),
        _ => {}
    }
    if n < 3 {
        return None;
    }
    match words.split_at(n - 3) {
        (a, ["multiplied", "by", b]) => Some(eval(a)? * eval_number(b)?),
        (a, ["divided", "by", b]) => Some(eval(a)? / eval_number(b)?),
        _ => None,
    }
}

pub fn answer(command: &str) -> Option<i32> {
    if !command.starts_with("What is ") || !command.ends_with('?') {
        return None;
    }
    let words = command[8..command.len() - 1]
        .split_whitespace()
        .collect::<Vec<_>>();
    eval(&words)
}
