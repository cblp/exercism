package binarysearch

func SearchInts(list []int, key int) int {
	left := 0
	right := len(list) - 1
	for left <= right {
		mid := left + (right-left)/2
		if list[mid] == key {
			return mid
		}
		if list[mid] < key {
			left = mid + 1
		} else {
			right = mid - 1
		}
	}
	return -1
}
