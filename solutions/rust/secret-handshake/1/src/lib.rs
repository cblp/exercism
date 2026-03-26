pub fn actions(n: u8) -> Vec<&'static str> {
    let mut actions = vec![];
    if n & (1 << 0) != 0 {
        actions.push("wink")
    }
    if n & (1 << 1) != 0 {
        actions.push("double blink")
    }
    if n & (1 << 2) != 0 {
        actions.push("close your eyes")
    }
    if n & (1 << 3) != 0 {
        actions.push("jump")
    }
    if n & (1 << 4) != 0 {
        actions.reverse()
    }
    actions
}
