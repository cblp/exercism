use std::{marker::PhantomData, ptr::NonNull};

// this module adds some functionality based on the required implementations
// here like: `LinkedList::pop_back` or `Clone for LinkedList<T>`
// You are free to use anything in it, but it's mainly for the test framework.
mod pre_implemented;

struct Node<T> {
    value: T,
    prev: Option<NonNull<Node<T>>>,
    next: Option<NonNull<Node<T>>>,
}

pub struct LinkedList<T> {
    head: Option<NonNull<Node<T>>>,
    last: Option<NonNull<Node<T>>>,
    len: usize,
    _phantom: PhantomData<Box<Node<T>>>,
}

pub struct Cursor<'a, T> {
    list: &'a mut LinkedList<T>,
    node: Option<NonNull<Node<T>>>,
}

pub struct Iter<'a, T>(Option<&'a Node<T>>);

impl<T> LinkedList<T> {
    pub fn new() -> Self {
        Self {
            head: None,
            last: None,
            len: 0,
            _phantom: PhantomData,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    pub fn len(&self) -> usize {
        self.len
    }

    /// Return a cursor positioned on the front element
    pub fn cursor_front(&mut self) -> Cursor<'_, T> {
        Cursor {
            node: self.head,
            list: self,
        }
    }

    /// Return a cursor positioned on the back element
    pub fn cursor_back(&mut self) -> Cursor<'_, T> {
        Cursor {
            node: self.last,
            list: self,
        }
    }

    /// Return an iterator that moves from front to back
    pub fn iter(&self) -> Iter<'_, T> {
        Iter(self.head.map(|ptr| unsafe { ptr.as_ref() }))
    }
}

impl<T> Drop for LinkedList<T> {
    fn drop(&mut self) {
        let mut current = self.head;
        while let Some(curptr) = current {
            unsafe {
                let next = curptr.as_ref().next;
                drop(Box::from_raw(curptr.as_ptr()));
                current = next;
            }
        }
    }
}

unsafe impl<T: Send> Send for LinkedList<T> {}

unsafe impl<T: Sync> Sync for LinkedList<T> {}

// the cursor is expected to act as if it is at the position of an element
// and it also has to work with and be able to insert into an empty list.
impl<T> Cursor<'_, T> {
    /// Take a mutable reference to the current element
    pub fn peek_mut(&mut self) -> Option<&mut T> {
        Some(&mut unsafe { self.node?.as_mut() }.value)
    }

    /// Move one position forward (towards the back) and
    /// return a reference to the new position
    pub fn next(&mut self) -> Option<&mut T> {
        unsafe {
            self.node = self.node?.as_mut().next;
            Some(&mut self.node?.as_mut().value)
        }
    }

    /// Move one position backward (towards the front) and
    /// return a reference to the new position
    pub fn prev(&mut self) -> Option<&mut T> {
        unsafe {
            self.node = self.node?.as_mut().prev;
            Some(&mut self.node?.as_mut().value)
        }
    }

    /// Remove and return the element at the current position and move the
    /// cursor to the neighboring element that's closest to the back.
    /// This can be either the next or previous position.
    pub fn take(&mut self) -> Option<T> {
        let node = unsafe { Box::from_raw(self.node?.as_ptr()) };
        match (node.prev, node.next) {
            (Some(mut prev), Some(mut next)) => unsafe {
                prev.as_mut().next = Some(next);
                next.as_mut().prev = Some(prev);
                self.node = Some(next);
            },
            (None, Some(mut next)) => unsafe {
                next.as_mut().prev = None;
                self.list.head = Some(next);
                self.node = Some(next);
            },
            (Some(mut prev), None) => unsafe {
                prev.as_mut().next = None;
                self.list.last = Some(prev);
                self.node = Some(prev);
            },
            (None, None) => {
                self.list.head = None;
                self.list.last = None;
                self.node = None;
            }
        }
        self.list.len -= 1;
        Some(node.value)
    }

    fn insert_single(&mut self, value: T) {
        let node = NonNull::new(Box::into_raw(Box::new(Node {
            value,
            prev: None,
            next: None,
        })));
        self.list.head = node;
        self.list.last = node;
        self.node = node;
    }

    pub fn insert_after(&mut self, value: T) {
        match self.node {
            None => self.insert_single(value),
            Some(mut node) => {
                let new_node = NonNull::new(Box::into_raw(Box::new(Node {
                    value,
                    next: unsafe { node.as_ref().next },
                    prev: Some(node),
                })));
                match unsafe { node.as_ref() }.next {
                    Some(mut next) => unsafe { next.as_mut().prev = new_node },
                    None => self.list.last = new_node,
                }
                unsafe { node.as_mut().next = new_node };
            }
        }
        self.list.len += 1;
    }

    pub fn insert_before(&mut self, value: T) {
        match self.node {
            None => self.insert_single(value),
            Some(mut node) => {
                let new_node = NonNull::new(Box::into_raw(Box::new(Node {
                    value,
                    next: Some(node),
                    prev: unsafe { node.as_ref().prev },
                })));
                match unsafe { node.as_ref() }.prev {
                    Some(mut prev) => unsafe { prev.as_mut().next = new_node },
                    None => self.list.head = new_node,
                }
                unsafe { node.as_mut().prev = new_node };
            }
        }
        self.list.len += 1;
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<&'a T> {
        match self.0 {
            Some(node) => {
                self.0 = node.next.map(|ptr| unsafe { ptr.as_ref() });
                Some(&node.value)
            }
            None => None,
        }
    }
}
