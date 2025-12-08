package railfence

func subst(message string, rails int, is_decoding bool) string {
	max_step := 2 * (rails - 1)
	out := make([]byte, len(message))
	cipher_ix := 0
	for r := range rails {
		step := 2 * r
		for plain_ix := r; plain_ix < len(message); plain_ix += step {
			if is_decoding {
				out[plain_ix] = message[cipher_ix]
			} else {
				out[cipher_ix] = message[plain_ix]
			}
			cipher_ix++
			if step != max_step {
				step = max_step - step
			}
		}
	}

	return string(out)
}

func Encode(message string, rails int) string {
	return subst(message, rails, false)
}

func Decode(message string, rails int) string {
	return subst(message, rails, true)
}
