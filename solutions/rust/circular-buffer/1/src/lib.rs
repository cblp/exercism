use std::mem::swap;

pub struct CircularBuffer<T> {
    data: Vec<Option<T>>,
    head: usize,
    tail: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    EmptyBuffer,
    FullBuffer,
    Internal,
}

impl<T> CircularBuffer<T> {
    pub fn new(capacity: usize) -> Self {
        let mut data = Vec::with_capacity(capacity + 1);
        for _ in 0..=capacity {
            data.push(None);
        }
        Self {
            data,
            head: 0,
            tail: 0,
        }
    }

    fn is_empty(&self) -> bool {
        self.head == self.tail
    }

    pub fn read(&mut self) -> Result<T, Error> {
        if self.is_empty() {
            return Err(Error::EmptyBuffer);
        }
        let mut r = None;
        swap(&mut r, &mut self.data[self.head]);
        self.head = (self.head + 1) % self.data.len();
        r.ok_or(Error::Internal)
    }

    pub fn clear(&mut self) {
        while self.tail != self.head {
            self.data[self.head] = None;
            self.head = (self.head + 1) % self.data.len();
        }
    }

    pub fn write(&mut self, element: T) -> Result<(), Error> {
        if (self.tail + 1) % self.data.len() == self.head {
            return Err(Error::FullBuffer);
        }
        self.data[self.tail] = Some(element);
        self.tail = (self.tail + 1) % self.data.len();
        Ok(())
    }

    pub fn overwrite(&mut self, element: T) {
        if (self.tail + 1) % self.data.len() == self.head {
            self.data[self.head] = None;
            self.head = (self.head + 1) % self.data.len();
        }
        self.data[self.tail] = Some(element);
        self.tail = (self.tail + 1) % self.data.len();
    }
}
