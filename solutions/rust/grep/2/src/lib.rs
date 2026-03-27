use {
    anyhow::Error,
    std::{
        fs::File,
        io::{BufRead, BufReader},
    },
};

/// While using `&[&str]` to handle flags is convenient for exercise purposes,
/// and resembles the output of [`std::env::args`], in real-world projects it is
/// both more convenient and more idiomatic to contain runtime configuration in
/// a dedicated struct. Therefore, we suggest that you do so in this exercise.
///
/// [`std::env::args`]: https://doc.rust-lang.org/std/env/fn.args.html
#[derive(Debug)]
pub struct Flags {
    case_insensitive: bool,
    filenames_only: bool,
    invert_match: bool,
    match_entire_line: bool,
    prepend_line_numbers: bool,
}

impl Flags {
    pub fn new(flags: &[&str]) -> Self {
        Self {
            case_insensitive: flags.contains(&"-i"),
            filenames_only: flags.contains(&"-l"),
            invert_match: flags.contains(&"-v"),
            match_entire_line: flags.contains(&"-x"),
            prepend_line_numbers: flags.contains(&"-n"),
        }
    }
}

fn match_lines(flags: &Flags, line: &str, pattern: &str) -> bool {
    if flags.match_entire_line {
        if flags.case_insensitive {
            line.chars().zip(pattern.chars()).all(|(a, b)| {
                a.to_lowercase().to_string() == b.to_lowercase().to_string()
            })
        } else {
            line == pattern
        }
    } else if flags.case_insensitive {
        line.to_lowercase().contains(&pattern.to_lowercase())
    } else {
        line.contains(pattern)
    }
}

pub fn grep(
    pattern: &str,
    flags: &Flags,
    files: &[&str],
) -> Result<Vec<String>, Error> {
    let mut results = vec![];
    for filename in files {
        let file = File::open(filename)?;
        for (line_number, line) in BufReader::new(file).lines().enumerate() {
            let line_original = line?;
            if flags.invert_match != match_lines(flags, &line_original, pattern)
            {
                if flags.filenames_only {
                    results.push(filename.to_string());
                    break;
                }
                let mut result = String::new();
                if files.len() > 1 {
                    result += filename;
                    result += ":";
                }
                if flags.prepend_line_numbers {
                    result += &(line_number + 1).to_string();
                    result += ":";
                }
                result += &line_original;
                results.push(result);
            }
        }
    }
    Ok(results)
}
