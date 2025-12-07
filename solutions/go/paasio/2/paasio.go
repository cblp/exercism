package paasio

import (
	"io"
	"sync"
)

type iocounter struct {
	reader io.Reader
	writer io.Writer
	mutex  sync.RWMutex
	bytes  int64
	ops    int
}

func (c *iocounter) record(bytes int, err error) (int, error) {
	c.mutex.Lock()
	defer c.mutex.Unlock()
	c.bytes += int64(bytes)
	c.ops++
	return bytes, err
}

func NewWriteCounter(writer io.Writer) WriteCounter {
	return &iocounter{writer: writer}
}

func NewReadCounter(reader io.Reader) ReadCounter {
	return &iocounter{reader: reader}
}

func NewReadWriteCounter(readwriter io.ReadWriter) ReadWriteCounter {
	return &iocounter{reader: readwriter, writer: readwriter}
}

func (c *iocounter) Read(p []byte) (int, error) {
	return c.record(c.reader.Read(p))
}

func (c *iocounter) ReadCount() (int64, int) {
	c.mutex.RLock()
	defer c.mutex.RUnlock()
	return c.bytes, c.ops
}

func (c *iocounter) Write(p []byte) (int, error) {
	return c.record(c.writer.Write(p))
}

func (c *iocounter) WriteCount() (int64, int) {
	return c.ReadCount()
}
