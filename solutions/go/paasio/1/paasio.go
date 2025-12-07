package paasio

import (
	"io"
	"sync"
)

type readCounter struct {
	base  io.Reader
	mu    sync.Mutex
	bytes int64
	ops   int64
}

type writeCounter struct {
	base  io.Writer
	mu    sync.Mutex
	bytes int64
	ops   int64
}

type readWriteCounter struct {
	base  io.ReadWriter
	mu    sync.Mutex
	bytes int64
	ops   int64
}

func NewWriteCounter(writer io.Writer) WriteCounter {
	return &writeCounter{base: writer}
}

func NewReadCounter(reader io.Reader) ReadCounter {
	return &readCounter{base: reader}
}

func NewReadWriteCounter(readwriter io.ReadWriter) ReadWriteCounter {
	return &readWriteCounter{base: readwriter}
}

func (c *readCounter) Read(p []byte) (int, error) {
	bytes, err := c.base.Read(p)
	c.mu.Lock()
	defer c.mu.Unlock()
	c.bytes += int64(bytes)
	c.ops += 1
	return bytes, err
}

func (c *readWriteCounter) Read(p []byte) (int, error) {
	bytes, err := c.base.Read(p)
	c.mu.Lock()
	defer c.mu.Unlock()
	c.bytes += int64(bytes)
	c.ops += 1
	return bytes, err
}

func (c *readCounter) ReadCount() (int64, int) {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.bytes, int(c.ops)
}

func (c *readWriteCounter) ReadCount() (int64, int) {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.bytes, int(c.ops)
}

func (c *writeCounter) Write(p []byte) (int, error) {
	bytes, err := c.base.Write(p)
	c.mu.Lock()
	defer c.mu.Unlock()
	c.bytes += int64(bytes)
	c.ops += 1
	return bytes, err
}

func (c *readWriteCounter) Write(p []byte) (int, error) {
	bytes, err := c.base.Write(p)
	c.mu.Lock()
	defer c.mu.Unlock()
	c.bytes += int64(bytes)
	c.ops += 1
	return bytes, err
}

func (c *writeCounter) WriteCount() (int64, int) {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.bytes, int(c.ops)
}

func (c *readWriteCounter) WriteCount() (int64, int) {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.bytes, int(c.ops)
}
