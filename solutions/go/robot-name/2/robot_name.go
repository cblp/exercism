package robotname

import "errors"

type Robot struct {
	name string
}

var counter int32

func newName() (string, error) {
	if counter >= 26*26*1000 {
		return "", errors.New("namespace exhausted")
	}
	r := string(
		[]rune{
			'A' + counter/26_000%26,
			'A' + counter/1000%26,
			'0' + counter/100%10,
			'0' + counter/10%10,
			'0' + counter%10,
		},
	)
	counter++
	return r, nil
}

func (r *Robot) Name() (string, error) {
	if r.name == "" {
		name, err := newName()
		if err != nil {
			return "", err
		}
		r.name = name
	}
	return r.name, nil
}

func (r *Robot) Reset() {
	r.name = ""
}
