package secret

var all_commands = []string{"wink", "double blink", "close your eyes", "jump"}

func Handshake(code uint) (r []string) {
	is_bit_set := func(n int) bool { return code&(1<<n) != 0 }

	start := 0
	end := 4
	step := 1
	if is_bit_set(4) {
		// Reverse order flag is set
		start = 3
		end = -1
		step = -1
	}
	for i := start; i != end; i += step {
		if is_bit_set(i) {
			r = append(r, all_commands[i])
		}
	}
	return
}
