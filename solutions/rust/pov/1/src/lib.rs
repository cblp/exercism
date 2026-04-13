use std::{
    collections::BTreeMap, collections::LinkedList, fmt::Debug, iter::once,
};

// Метка не хранится внутри Node — она всегда ключ BTreeMap в родителе
#[derive(Debug, PartialEq, Eq)]
struct Node<T: Debug + Ord> {
    children: BTreeMap<T, Node<T>>,
}

impl<T: Debug + Ord> Node<T> {
    fn new() -> Self {
        Self {
            children: BTreeMap::new(),
        }
    }

    fn path_to<'a>(
        &'a self,
        my_label: &'a T,
        target: &T,
    ) -> Option<LinkedList<&'a T>> {
        if my_label == target {
            return Some(LinkedList::from([my_label]));
        }
        for (child_label, child_node) in &self.children {
            if let Some(mut path) = child_node.path_to(child_label, target) {
                path.push_front(my_label);
                return Some(path);
            }
        }
        None
    }

    // Ok(новый_корень) если from найден, Err(self) если нет
    fn pov_from(
        mut self,
        my_label: T,
        from: &T,
        extra: Option<(T, Node<T>)>,
    ) -> Result<(T, Node<T>), (T, Node<T>)> {
        if my_label == *from {
            if let Some((extra_label, extra_node)) = extra {
                self.children.insert(extra_label, extra_node);
            }
            return Ok((my_label, self));
        }

        let mut rest = BTreeMap::new();
        let mut found = None;
        for (k, v) in std::mem::take(&mut self.children) {
            if found.is_none() && v.path_to(&k, from).is_some() {
                found = Some((k, v));
            } else {
                rest.insert(k, v);
            }
        }

        if let Some((child_label, child_node)) = found {
            if let Some((extra_label, extra_node)) = extra {
                rest.insert(extra_label, extra_node);
            }
            self.children = rest;
            child_node
                .pov_from(child_label, from, Some((my_label, self)))
                .map_err(|_| unreachable!())
        } else {
            self.children = rest;
            Err((my_label, self))
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Tree<T: Debug + Ord>(Option<(T, Node<T>)>);

impl<T: Debug + Ord> Tree<T> {
    pub fn new(label: T) -> Self {
        Self(Some((label, Node::new())))
    }

    pub fn with_child(mut self, child: Self) -> Self {
        if let (Some((_, node)), Some((child_label, child_node))) =
            (self.0.as_mut(), child.0)
        {
            node.children.insert(child_label, child_node);
        }
        self
    }

    pub fn pov_from(&mut self, from: &T) -> bool {
        let (label, node) = self.0.take().unwrap();
        match node.pov_from(label, from, None) {
            Ok(inner) => {
                self.0 = Some(inner);
                true
            }
            Err(inner) => {
                self.0 = Some(inner);
                false
            }
        }
    }

    pub fn path_between<'a>(
        &'a mut self,
        from: &'a T,
        to: &'a T,
    ) -> Option<Vec<&'a T>> {
        let (label, node) = self.0.as_ref()?;
        Some(
            merge_paths(node.path_to(label, from)?, node.path_to(label, to)?)
                .into_iter()
                .collect(),
        )
    }
}

fn merge_paths<'a, T: Debug + PartialEq>(
    mut a: LinkedList<&'a T>,
    mut b: LinkedList<&'a T>,
) -> LinkedList<&'a T> {
    b.pop_front();
    let a_head = a.pop_front().unwrap();
    if a.front() == b.front() {
        merge_paths(a, b)
    } else {
        LinkedList::from_iter(a.into_iter().rev().chain(once(a_head)).chain(b))
    }
}
