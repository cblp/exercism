from collections import deque


def find_fewest_coins(coins: list[int], target: int) -> list[int]:
    if target < 0:
        raise ValueError("target can't be negative")
    if target == 0:
        return []
    coins_biggest_to_smallest = sorted(coins, reverse=True)
    coinsets: deque[list[int]] = deque([[]])
    visited_values = set()
    while coinsets:
        coinset_prev = coinsets.popleft()
        for coin in coins_biggest_to_smallest:
            coinset = coinset_prev + [coin]
            value = sum(coinset)
            if value == target:
                return sorted(coinset)
            if value < target and value not in visited_values:
                visited_values.add(value)
                coinsets.append(coinset)
    raise ValueError("can't make target with given coins")
