pub struct RailFence {
    rails: u32,
}

fn subst(message: &str, rails: u32, is_decoding: bool) -> String {
    let message: Vec<char> = message.chars().collect();
    let max_step = 2 * (rails - 1) as usize;
    let mut out = vec![0 as char; message.len()];
    let mut cipher_ix = 0;
    for r in 0..rails as usize {
        let mut step = 2 * r;
        let mut plain_ix = r;
        while plain_ix < message.len() {
            if is_decoding {
                out[plain_ix] = message[cipher_ix]
            } else {
                out[cipher_ix] = message[plain_ix]
            }
            cipher_ix += 1;
            if step != max_step {
                step = max_step - step
            }
            plain_ix += step;
        }
    }
    String::from_iter(out)
}

impl RailFence {
    pub fn new(rails: u32) -> Self {
        Self { rails }
    }

    pub fn encode(&self, text: &str) -> String {
        subst(text, self.rails, false)
    }

    pub fn decode(&self, cipher: &str) -> String {
        subst(cipher, self.rails, true)
    }
}
