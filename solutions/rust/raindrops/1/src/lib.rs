use frunk::monoid::combine_all;

pub fn raindrops(n: u32) -> String {
    let a = |d, p: &str| (n % d == 0).then_some(p.to_string());
    combine_all(&[a(3, "Pling"), a(5, "Plang"), a(7, "Plong")])
        .unwrap_or(n.to_string())
}
