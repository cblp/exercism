fn opening(c: char) -> char {
    match c {
        ')' => '(',
        ']' => '[',
        '}' => '{',
        _ => todo!("opening({c:?})"),
    }
}

pub fn brackets_are_balanced(string: &str) -> bool {
    let mut bracket_stack: Vec<char> = vec![];
    for c in string.chars() {
        match c {
            '(' | '[' | '{' => bracket_stack.push(c),
            ')' | ']' | '}' => {
                if !bracket_stack.pop().is_some_and(|b| b == opening(c)) {
                    return false;
                }
            }
            _ => {}
        }
    }
    bracket_stack.is_empty()
}
