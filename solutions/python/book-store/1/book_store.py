from collections import Counter

COST_1 = 8_00
COST_2 = 15_20
COST_3 = 21_60
COST_4 = 25_60
COST_5 = 30_00


def take_5(books: dict[int, int]) -> bool:
    if all(books[b] >= 1 for b in range(1, 6)):
        for b in range(1, 6):
            books[b] -= 1
        return True
    else:
        return False


def take_some(n: int, books: dict[int, int]) -> bool:
    assert 1 <= n < 5
    if sum(books[b] >= 1 for b in range(1, 6)) >= n:
        for b, _ in sorted(
            books.items(), key=lambda book_count: book_count[1], reverse=True
        )[:n]:
            books[b] -= 1
        return True
    else:
        return False


def total(basket: list[int]) -> int:
    result = 0
    books = Counter(basket)
    while take_5(books):
        result += COST_5
    while take_some(4, books):
        result += COST_4
    while take_some(3, books):
        result += COST_3
    while take_some(2, books):
        result += COST_2
    while take_some(1, books):
        result += COST_1
    return result
