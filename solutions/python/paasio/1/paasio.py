import io
import socket


class MeteredFile(io.BufferedRandom):
    """Implement using a subclassing model."""

    def __init__(self, *args, **kwargs) -> None:
        super().__init__("", *args, **kwargs)

    # def __enter__(self):
    #     pass

    # def __exit__(self, exc_type, exc_val, exc_tb):
    #     pass

    # def __iter__(self):
    #     pass

    # def __next__(self):
    #     pass

    # def read(self, size=-1):
    #     pass

    @property
    def read_bytes(self) -> int:
        return 0

    @property
    def read_ops(self) -> int:
        return 0

    # def write(self, b):
    #     pass

    # @property
    # def write_bytes(self):
    #     pass

    # @property
    # def write_ops(self) -> None:
    #     pass


class MeteredSocket:
    """Implement using a delegation model."""

    def __init__(self, socket: socket.socket) -> None:
        self.wrapped = socket

    def __enter__(self) -> MeteredSocket:
        pass

    def __exit__(self, exc_type, exc_val, exc_tb):
        pass

    def recv(self, bufsize: int, flags: int = 0) -> bytes:
        return self.wrapped.recv(bufsize, flags)

    @property
    def recv_bytes(self) -> int:
        return 0

    @property
    def recv_ops(self) -> int:
        return 0

    # def send(self, data, flags=0):
    #     pass

    @property
    def send_bytes(self) -> int:
        return 0

    @property
    def send_ops(self) -> int:
        return 0
