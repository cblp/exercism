use std::{
    borrow::Borrow,
    io::{Read, Write},
};

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

    pub fn reader(self, r: impl Read) -> impl Read {
        XorReader {
            xor: self,
            parent: r,
        }
    }

    pub fn writer(self, w: impl Write) -> impl Write {
        XorWriter {
            xor: self,
            parent: w,
        }
    }
}

struct XorReader<'a, R> {
    xor: Xorcism<'a>,
    parent: R,
}

impl<R: Read> Read for XorReader<'_, R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let n = self.parent.read(buf)?;
        self.xor.munge_in_place(&mut buf[..n]);
        Ok(n)
    }
}

struct XorWriter<'a, W> {
    xor: Xorcism<'a>,
    parent: W,
}

impl<W: Write> Write for XorWriter<'_, W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let mut out = Vec::with_capacity(buf.len());
        for byte in self.xor.munge(buf) {
            out.push(byte);
        }
        self.parent.write(&out)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.parent.flush()
    }
}
