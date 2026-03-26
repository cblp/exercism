use std::{cmp::max, collections::HashMap};

#[derive(Debug)]
pub struct Item {
    pub weight: u32,
    pub value: u32,
}

pub fn maximum_value(max_weight: u32, items: &[Item]) -> u32 {
    if items.is_empty() {
        return 0;
    }

    let mut best = HashMap::from([(0, 0)]);
    let mut max_value = 0;
    for item in items {
        let best_before_this_item = best.clone();
        for (best_weight, best_value) in best_before_this_item {
            let new_weight = best_weight + item.weight;
            if new_weight > max_weight {
                continue;
            }
            let new_value = best_value + item.value;
            best.entry(new_weight)
                .and_modify(|v| *v = max::<u32>(*v, new_value))
                .or_insert(new_value);
            if new_value > max_value {
                max_value = new_value;
            }
        }
    }

    max_value
}
