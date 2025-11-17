use std::rc::Rc;

#[derive(Clone)]
enum ListNode<T> {
    Nil,
    Cons(T, Rc<ListNode<T>>),
}
use ListNode::*;

impl<T> ListNode<T> {
    fn is_none(&self) -> bool {
        match self {
            Nil => true,
            Cons(..) => false,
        }
    }

    fn tail(&self) -> Option<Rc<ListNode<T>>> {
        match self {
            Nil => None,
            Cons(_, t) => Some(t.clone()),
        }
    }
}

#[derive(Clone)]
pub struct SimpleLinkedList<T>(Rc<ListNode<T>>);

impl<T: Copy> SimpleLinkedList<T> {
    pub fn new() -> Self {
        Self(Rc::new(Nil))
    }

    // pub fn iter(&self) -> Self {
    //     self.clone()
    // }

    // You may be wondering why it's necessary to have is_empty()
    // when it can easily be determined from len().
    // It's good custom to have both because len() can be expensive for some types,
    // whereas is_empty() is almost always cheap.
    // (Also ask yourself whether len() is expensive for SimpleLinkedList)
    pub fn is_empty(&self) -> bool {
        self.0.is_none()
    }

    pub fn len(&self) -> usize {
        let mut l = 0;
        let mut node = self.0.clone();
        while let Some(tail) = node.tail() {
            l += 1;
            node = tail;
        }
        l
    }

    pub fn push(&mut self, element: T) {
        self.0 = Rc::new(Cons(element, self.0.clone()));
    }

    pub fn pop(&mut self) -> Option<T> {
        match self.0.clone().as_ref() {
            Nil => None,
            Cons(head, tail) => {
                self.0 = tail.clone();
                Some(*head)
            }
        }
    }

    pub fn peek(&self) -> Option<&T> {
        match self.0.as_ref() {
            Nil => None,
            Cons(head, _) => Some(head),
        }
    }

    #[must_use]
    pub fn rev(self) -> Self {
        let mut r = Self::new();
        for x in self {
            r.push(x);
        }
        r
    }
}

impl<T: Copy> Iterator for SimpleLinkedList<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        self.pop()
    }
}

impl<T: Copy> FromIterator<T> for SimpleLinkedList<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut r = Self::new();
        for x in iter {
            r.push(x);
        }
        r
    }
}

// In general, it would be preferable to implement IntoIterator for SimpleLinkedList<T>
// instead of implementing an explicit conversion to a vector. This is because, together,
// FromIterator and IntoIterator enable conversion between arbitrary collections.
//
// The reason this exercise's API includes an explicit conversion to Vec<T> instead
// of IntoIterator is that implementing that interface is fairly complicated, and
// demands more of the student than we expect at this point in the track.
//
// Please note that the "front" of the linked list should correspond to the "back"
// of the vector as far as the tests are concerned.

impl<T: Copy> From<SimpleLinkedList<T>> for Vec<T> {
    fn from(linked_list: SimpleLinkedList<T>) -> Vec<T> {
        let mut v = Vec::from_iter(linked_list);
        v.reverse();
        v
    }
}
