def find(search_list: list[int], value: int) -> int:
    left = 0
    right = len(search_list) - 1
    while left <= right:
        mid = (left + right) // 2
        if search_list[mid] == value:
            return mid
        if search_list[mid] < value:
            left = mid + 1
        else:
            right = mid - 1
    raise ValueError("value not in array")
