use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug)]
struct Equation {
    summands: Vec<char>,
    result_var: char,
}

fn go(
    carry: u16,
    env: HashMap<char, u8>,
    equations: Vec<Equation>,
    leading_vars: &HashSet<char>,
) -> Vec<HashMap<char, u8>> {
    if let Some((eq, eqs)) = equations.split_first() {
        try_solve(eq.clone(), carry, env, leading_vars)
            .iter()
            .flat_map(|(carry_, env_)| {
                go(*carry_, env_.clone(), eqs.to_vec(), leading_vars)
            })
            .collect()
    } else {
        match carry {
            0 => vec![env],
            _ => panic!("Unexpected carry = {carry}"),
        }
    }
}

fn try_solve(
    eq: Equation,
    carry: u16,
    env: HashMap<char, u8>,
    leading_vars: &HashSet<char>,
) -> Vec<(u16, HashMap<char, u8>)> // returns new carry, modified env
{
    apply(carry, env, eq.summands, leading_vars)
        .into_iter()
        .flat_map(|(result, env1)| {
            let result_carry = result / 10;
            let result_digit_calculated = (result % 10) as u8;
            match env1.get(&eq.result_var) {
                Some(&result_digit_known)
                    if result_digit_calculated == result_digit_known =>
                {
                    vec![env1]
                }
                None if !env1
                    .values()
                    .collect::<HashSet<_>>()
                    .contains(&result_digit_calculated)
                    && (!leading_vars.contains(&eq.result_var)
                        || result_digit_calculated != 0) =>
                {
                    let mut env2 = env1;
                    env2.insert(eq.result_var, result_digit_calculated);
                    vec![env2]
                }
                _ => vec![],
            }
            .iter()
            .map(move |env2| (result_carry, env2.clone()))
            .collect::<Vec<_>>()
        })
        .collect()
}

fn apply(
    carry: u16,
    env: HashMap<char, u8>,
    summands: Vec<char>,
    leading_vars: &HashSet<char>,
) -> Vec<(u16, HashMap<char, u8>)> {
    if let Some((var, vars)) = summands.split_first() {
        if let Some(value) = env.get(var) {
            apply(*value as u16 + carry, env, vars.to_vec(), leading_vars)
        } else {
            let env_digits: HashSet<u8> = env.clone().into_values().collect();
            free_digits(*var, leading_vars.clone(), env_digits)
                .iter()
                .flat_map(|value| {
                    let mut env_ = env.clone();
                    env_.insert(*var, *value);
                    apply(
                        *value as u16 + carry,
                        env_,
                        vars.to_vec(),
                        leading_vars,
                    )
                })
                .collect()
        }
    } else {
        vec![(carry, env)]
    }
}

fn free_digits(
    var: char,
    leading_vars: HashSet<char>,
    env_digits: HashSet<u8>,
) -> Vec<u8> {
    if leading_vars.contains(&var) {
        1..=9
    } else {
        0..=9
    }
    .filter(|digit| !env_digits.contains(digit))
    .collect()
}

pub fn solve(input: &str) -> Option<HashMap<char, u8>> {
    let (summands, results) = parse(input);
    let leading_vars = {
        let mut all_parts = summands.clone();
        all_parts.push(results.clone());
        all_parts
            .iter()
            .filter_map(|x| x.first())
            .copied()
            .collect()
    };
    dedup(go(
        0,
        HashMap::new(),
        make_equations(summands, results),
        &leading_vars,
    ))
}

fn dedup<A: Clone>(v: Vec<A>) -> Option<A> {
    v.first().map(|x| x.clone())
    // TODO: a : b : _ ->
    //     error $ "too many solutions: " ++ show a ++ ", " ++ show b ++ "..."
}

fn parse(input: &str) -> (Vec<Vec<char>>, Vec<char>) {
    unsnoc(
        input
            .split(|c: char| !c.is_alphabetic())
            .filter(|s| !s.is_empty())
            .map(|s| s.chars().collect())
            .collect(),
    )
}

fn unsnoc<A: Clone>(v: Vec<A>) -> (Vec<A>, A) {
    if let Some(last) = v.last()
    // TODO split_last
    {
        (v[0..v.len() - 1].to_vec(), last.clone())
    } else {
        panic!("input must be non-empty")
    }
}

fn make_equations(
    summands: Vec<Vec<char>>,
    results: Vec<char>,
) -> Vec<Equation> {
    let mut rs: Vec<char> = results;
    match rs.pop() {
        Some(r) => {
            let (e, ss_) = make_equation(summands, r);
            // (r : rs) :| ss ->
            //     let (e, ss') = make_equation r ss
            //     in e : make_equations (rs :| ss')

            let mut eqs = vec![e];
            eqs.append(&mut make_equations(ss_, rs));
            eqs
        }
        None => vec![],
    }
}

fn make_equation(ss: Vec<Vec<char>>, c: char) -> (Equation, Vec<Vec<char>>) {
    (
        Equation {
            summands: ss.iter().filter_map(|s| s.last()).copied().collect(),
            result_var: c,
        },
        ss.iter()
            .filter(|s| !s.is_empty())
            .map(|s| Vec::from(&s[0..s.len() - 1]))
            .collect(),
    )
}
