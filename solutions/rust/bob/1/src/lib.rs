pub fn reply(message: &str) -> &str {
    let message = message.trim();
    let letters: String =
        message.chars().filter(|c| c.is_alphabetic()).collect();
    let is_yell =
        !letters.is_empty() && letters.chars().all(char::is_uppercase);
    let is_question = message.ends_with("?");

    match message {
        "" => "Fine. Be that way!",
        _ if is_yell && is_question => "Calm down, I know what I'm doing!",
        _ if is_question => "Sure.",
        _ if is_yell => "Whoa, chill out!",
        _ => "Whatever.",
    }
}
