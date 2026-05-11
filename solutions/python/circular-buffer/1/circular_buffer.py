# pylint: disable=disallowed-name
# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=missing-module-docstring


class BufferFullException(BufferError):
    """Exception raised when CircularBuffer is full.

    message: explanation of the error.
    """

    def __init__(self, message: str):
        pass


class BufferEmptyException(BufferError):
    """Exception raised when CircularBuffer is empty.

    message: explanation of the error.
    """

    def __init__(self, message: str):
        pass


class CircularBuffer:
    _buffer: list[str]
    _reader: int
    _writer: int

    def __init__(self, capacity: int):
        self._buffer = [""] * (capacity + 1)
        self._reader = 0
        self._writer = 0

    @property
    def physical_capacity(self) -> int:
        return len(self._buffer)

    def is_empty(self) -> bool:
        return self._reader == self._writer

    def is_full(self) -> bool:
        return self._reader == (self._writer + 1) % self.physical_capacity

    def read(self) -> str:
        if self.is_empty():
            raise BufferEmptyException("Circular buffer is empty")
        data = self._buffer[self._reader]
        self._reader = (self._reader + 1) % self.physical_capacity
        return data

    def write(self, data: str) -> None:
        if self.is_full():
            raise BufferFullException("Circular buffer is full")
        self._buffer[self._writer] = data
        self._writer = (self._writer + 1) % self.physical_capacity

    def overwrite(self, data: str) -> None:
        if self.is_full():
            self._reader = (self._reader + 1) % self.physical_capacity
        self._buffer[self._writer] = data
        self._writer = (self._writer + 1) % self.physical_capacity

    def clear(self) -> None:
        self._reader = self._writer
