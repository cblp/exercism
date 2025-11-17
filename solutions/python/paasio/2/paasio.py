import io


class MeteredFile(io.BufferedRandom):
    """Implement using a subclassing model."""

    _is_open: bool
    _read_bytes: int
    _read_ops: int
    _write_bytes: int
    _write_ops: int

    def __init__(self, *args, **kwargs):
        super().__init__("", *args, **kwargs)
        self._is_open = False
        self._read_bytes = 0
        self._read_ops = 0
        self._write_bytes = 0
        self._write_ops = 0

    def __enter__(self):
        self._is_open = True
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self._is_open = False
        return super().__exit__(exc_type, exc_val, exc_tb)

    def __iter__(self):
        return self

    def __next__(self):
        bs = super().readline()
        if bs:
            self._read_ops += 1
            self._read_bytes += len(bs)
            return bs
        else:
            raise StopIteration

    def read(self, size=-1):
        if self._is_open:
            bs = super().read(size)
            self._read_ops += 1
            self._read_bytes += len(bs)
            return bs
        else:
            raise ValueError("I/O operation on closed file.")

    @property
    def read_bytes(self):
        return self._read_bytes

    @property
    def read_ops(self):
        return self._read_ops

    def write(self, b):
        if self._is_open:
            n = super().write(b)
            self._write_ops += 1
            self._write_bytes += n
            return n
        else:
            raise ValueError("I/O operation on closed file.")

    @property
    def write_bytes(self):
        return self._write_bytes

    @property
    def write_ops(self):
        return self._write_ops


class MeteredSocket:
    """Implement using a delegation model."""

    _recv_bytes: int
    _recv_ops: int
    _send_bytes: int
    _send_ops: int

    def __init__(self, socket):
        self.wrapped = socket
        self._recv_bytes = 0
        self._recv_ops = 0
        self._send_bytes = 0
        self._send_ops = 0

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        return self.wrapped.__exit__(exc_type, exc_val, exc_tb)

    def recv(self, bufsize, flags=0):
        bs = self.wrapped.recv(bufsize, flags)
        self._recv_ops += 1
        self._recv_bytes += len(bs)
        return bs

    @property
    def recv_bytes(self):
        return self._recv_bytes

    @property
    def recv_ops(self):
        return self._recv_ops

    def send(self, data, flags=0):
        n = self.wrapped.send(data, flags)
        self._send_ops += 1
        self._send_bytes += n
        return n

    @property
    def send_bytes(self):
        return self._send_bytes

    @property
    def send_ops(self):
        return self._send_ops
