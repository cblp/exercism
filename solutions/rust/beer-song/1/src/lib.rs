use itertools::Itertools;

fn end(n: u32) -> String {
    match n {
        1 => "".to_string(),
        _ => "s".to_string(),
    }
}

pub fn verse(n: u32) -> String {
    match n {
        0 => "No more bottles of beer on the wall, no more bottles of beer.\n\
            Go to the store and buy some more, 99 bottles of beer on the wall."
            .to_string(),
        _ => {
            let e = end(n);
            let n_ = n - 1;
            let e_ = end(n_);
            let take = match n {
                1 => "it",
                _ => "one",
            };
            let left = match n_ {
                0 => "no more".to_string(),
                _ => n_.to_string(),
            };
            format!(
                "{n} bottle{e} of beer on the wall, {n} bottle{e} of beer.\n\
                Take {take} down and pass it around, \
                {left} bottle{e_} of beer on the wall.",
            )
        }
    }
}

pub fn sing(start: u32, end: u32) -> String {
    (end..=start).rev().map(verse).join("\n\n")
}
