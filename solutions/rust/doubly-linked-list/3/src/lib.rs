use std::ptr::null_mut;

// this module adds some functionality based on the required implementations
// here like: `LinkedList::pop_back` or `Clone for LinkedList<T>`
// You are free to use anything in it, but it's mainly for the test framework.
mod pre_implemented;

struct Node<T> {
    value: T,
    prev: *mut Node<T>,
    next: *mut Node<T>,
}

pub struct LinkedList<T> {
    head: *mut Node<T>,
    last: *mut Node<T>,
    len: usize,
}

pub struct Cursor<'a, T> {
    list: &'a mut LinkedList<T>,
    node: *mut Node<T>,
}

pub struct Iter<'a, T>(Option<&'a Node<T>>);

impl<T> LinkedList<T> {
    pub fn new() -> Self {
        Self {
            head: null_mut(),
            last: null_mut(),
            len: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.head.is_null()
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
        Iter(unsafe { self.head.as_ref() })
    }
}

impl<T> Drop for LinkedList<T> {
    fn drop(&mut self) {
        let mut current = self.head;
        while !current.is_null() {
            unsafe {
                let next = (*current).next;
                drop(Box::from_raw(current));
                current = next;
            }
        }
    }
}

// the cursor is expected to act as if it is at the position of an element
// and it also has to work with and be able to insert into an empty list.
impl<T> Cursor<'_, T> {
    /// Take a mutable reference to the current element
    pub fn peek_mut(&mut self) -> Option<&mut T> {
        unsafe { self.node.as_mut().map(|node| &mut node.value) }
    }

    /// Move one position forward (towards the back) and
    /// return a reference to the new position
    pub fn next(&mut self) -> Option<&mut T> {
        unsafe {
            self.node = self.node.as_mut()?.next;
            self.node.as_mut().map(|n| &mut n.value)
        }
    }

    /// Move one position backward (towards the front) and
    /// return a reference to the new position
    pub fn prev(&mut self) -> Option<&mut T> {
        unsafe {
            self.node = self.node.as_mut()?.prev;
            self.node.as_mut().map(|n| &mut n.value)
        }
    }

    /// Remove and return the element at the current position and move the
    /// cursor to the neighboring element that's closest to the back.
    /// This can be either the next or previous position.
    pub fn take(&mut self) -> Option<T> {
        if self.node.is_null() {
            None
        } else {
            let node = unsafe { Box::from_raw(self.node) };
            if !node.next.is_null() {
                if !node.prev.is_null() {
                    unsafe {
                        (*node.next).prev = node.prev;
                        (*node.prev).next = node.next;
                    }
                } else {
                    unsafe { (*node.next).prev = null_mut() }
                    self.list.head = node.next;
                }
                self.node = node.next;
            } else {
                if !node.prev.is_null() {
                    self.node = node.prev;
                    unsafe { (*self.node).next = null_mut() }
                } else {
                    self.list.head = null_mut();
                    self.node = null_mut();
                }
                self.list.last = self.node;
            }
            self.list.len -= 1;
            Some(node.value)
        }
    }

    fn insert_single(&mut self, value: T) {
        let node = Box::into_raw(Box::new(Node {
            value,
            prev: null_mut(),
            next: null_mut(),
        }));
        self.list.head = node;
        self.list.last = node;
        self.node = node;
    }

    pub fn insert_after(&mut self, value: T) {
        if self.node.is_null() {
            self.insert_single(value);
        } else {
            let new_node = Box::into_raw(Box::new(Node {
                value,
                next: unsafe { (*self.node).next },
                prev: self.node,
            }));
            if !unsafe { (*self.node).next }.is_null() {
                unsafe { (*(*self.node).next).prev = new_node }
            } else {
                self.list.last = new_node;
            }
            unsafe { (*self.node).next = new_node };
        }
        self.list.len += 1;
    }

    pub fn insert_before(&mut self, value: T) {
        if self.node.is_null() {
            self.insert_single(value);
        } else {
            let new_node = Box::into_raw(Box::new(Node {
                value,
                next: self.node,
                prev: unsafe { (*self.node).prev },
            }));
            if !unsafe { (*self.node).prev }.is_null() {
                unsafe { (*(*self.node).prev).next = new_node }
            } else {
                self.list.head = new_node;
            }
            unsafe { (*self.node).prev = new_node };
        }
        self.list.len += 1;
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<&'a T> {
        match self.0 {
            Some(node) => {
                self.0 = unsafe { node.next.as_ref() };
                Some(&node.value)
            }
            None => None,
        }
    }
}
