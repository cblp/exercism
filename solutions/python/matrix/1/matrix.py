class Matrix:
    data: list[list[int]]

    def __init__(self, matrix_string: str):
        self.data = [
            [int(cell) for cell in row.split()]
            for row in matrix_string.split("\n")
        ]

    def row(self, index: int) -> list[int]:
        return self.data[index - 1]

    def column(self, index: int) -> list[int]:
        return [row[index - 1] for row in self.data]
