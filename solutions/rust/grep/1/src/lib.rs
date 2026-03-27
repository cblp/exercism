use {
    anyhow::Error,
    clap::Parser,
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
#[derive(Debug, Parser)]
pub struct Flags {
    #[arg(short = 'n')]
    prepend_line_numbers: bool,

    #[arg(short = 'l')]
    filenames_only: bool,

    #[arg(short = 'i')]
    case_insensitive: bool,

    #[arg(short = 'v')]
    invert_match: bool,

    #[arg(short = 'x')]
    match_entire_line: bool,
}

impl Flags {
    pub fn new(flags: &[&str]) -> Self {
        let mut argv = vec![""];
        argv.append(&mut flags.to_vec());
        Self::parse_from(argv)
    }
}

pub fn grep(
    pattern: &str,
    flags: &Flags,
    files: &[&str],
) -> Result<Vec<String>, Error> {
    let pattern = if flags.case_insensitive {
        pattern.to_lowercase()
    } else {
        pattern.to_string()
    };
    let mut results = vec![];
    for filename in files {
        let file = File::open(filename)?;
        for (line_number, line) in BufReader::new(file).lines().enumerate() {
            let line_original = line?;
            let line_to_match = if flags.case_insensitive {
                line_original.to_lowercase()
            } else {
                line_original.clone()
            };
            if flags.invert_match
                != (if flags.match_entire_line {
                    line_to_match == pattern
                } else {
                    line_to_match.contains(&pattern)
                })
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
