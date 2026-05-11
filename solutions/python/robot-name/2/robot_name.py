# pylint: disable=missing-class-docstring
# pylint: disable=missing-function-docstring
# pylint: disable=missing-module-docstring


class Robot:
    # pylint: disable=too-few-public-methods

    _counter = 0

    name: str

    def __init__(self) -> None:
        self.reset()

    def reset(self) -> None:
        Robot._counter += 1
        self.name = f"AA{Robot._counter:03d}"
