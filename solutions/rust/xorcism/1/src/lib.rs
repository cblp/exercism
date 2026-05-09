use std::borrow::Borrow;

/// A munger which XORs a key with some data
#[derive(Clone)]
pub struct Xorcism<'a> {
    key: &'a [u8],
    i: usize,
}

impl<'a> Xorcism<'a> {
    /// Create a new Xorcism munger from a key
    pub fn new<Key: AsRef<[u8]> + ?Sized>(key: &'a Key) -> Self {
        Self {
            key: key.as_ref(),
            i: 0,
        }
    }

    /// XOR each byte of the input buffer with a byte from the key.
    pub fn munge_in_place(&mut self, data: &mut [u8]) {
        for byte in data.iter_mut() {
            *byte ^= self.key[self.i];
            self.i = (self.i + 1) % self.key.len();
        }
    }

    /// XOR each byte of the data with a byte from the key.
    pub fn munge<Data>(&mut self, data: Data) -> impl Iterator<Item = u8>
    where
        Data: IntoIterator,
        <Data as IntoIterator>::Item: Borrow<u8>,
    {
        data.into_iter().map(move |byte| {
            let out = byte.borrow() ^ self.key[self.i];
            self.i = (self.i + 1) % self.key.len();
            out
        })
    }
}
