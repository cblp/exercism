use itertools::Itertools;

pub fn build_proverb(list: &[&str]) -> String {
    if list.is_empty() {
        return String::new();
    }
    list.iter()
        .zip(list.iter().dropping(1))
        .map(|(cur, next)| {
            format!("For want of a {} the {} was lost.", cur, next)
        })
        .chain([format!("And all for the want of a {}.", list[0])])
        .join("\n")
}
