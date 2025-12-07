package circular

import "errors"

// Implement a circular buffer of bytes supporting both overflow-checked writes
// and unconditional, possibly overwriting, writes.
//
// We chose the provided API so that Buffer implements io.ByteReader
// and io.ByteWriter and can be used (size permitting) as a drop in
// replacement for anything using that interface.

// Define the Buffer type here.

type Buffer struct {
	data   []byte
	reader uint
	writer uint
}

func NewBuffer(requested_capacity int) *Buffer {
	return &Buffer{data: make([]byte, requested_capacity+1)}
}

func (buffer *Buffer) is_full() bool {
	return buffer.reader == (buffer.writer+1)%uint(len(buffer.data))
}

func (buffer *Buffer) is_empty() bool {
	return buffer.reader == buffer.writer
}

func (buffer *Buffer) advance_reader() {
	buffer.reader = (buffer.reader + 1) % uint(len(buffer.data))
}

func (buffer *Buffer) ReadByte() (byte, error) {
	if buffer.is_empty() {
		return 0, errors.New("ENODATA")
	}
	value := buffer.data[buffer.reader]
	buffer.advance_reader()
	return value, nil
}

func (buffer *Buffer) write_and_advance_writer(c byte) {
	buffer.data[buffer.writer] = c
	buffer.writer = (buffer.writer + 1) % uint(len(buffer.data))
}

func (buffer *Buffer) WriteByte(c byte) error {
	if buffer.is_full() {
		return errors.New("ENOBUFS")
	}
	buffer.write_and_advance_writer(c)
	return nil
}

func (buffer *Buffer) Overwrite(c byte) {
	if buffer.is_full() {
		buffer.advance_reader()
	}
	buffer.write_and_advance_writer(c)
}

func (buffer *Buffer) Reset() {
	buffer.reader = 0
	buffer.writer = 0
}
