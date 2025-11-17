#[derive(Debug, PartialEq, Eq)]
pub enum Comparison {
    Equal,
    Sublist,
    Superlist,
    Unequal,
}

fn is_sublist<T: PartialEq>(sub: &[T], sup: &[T]) -> bool {
    sub.len() == 0
        || sub.len() <= sup.len()
            && if sub[0] == sup[0] {
                sub == &sup[..sub.len()] || is_sublist(sub, &sup[1..])
            } else {
                is_sublist(sub, &sup[1..])
            }
}

pub fn sublist<T: PartialEq>(first_list: &[T], second_list: &[T]) -> Comparison {
    if first_list == second_list {
        Comparison::Equal
    } else if is_sublist(first_list, second_list) {
        Comparison::Sublist
    } else if is_sublist(second_list, first_list) {
        Comparison::Superlist
    } else {
        Comparison::Unequal
    }
}
