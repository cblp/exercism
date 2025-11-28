package robotname

type Robot struct {
	name string
}

var counter int32

func newName() string {
	counter++
	return string(
		[]rune{
			'A' + counter/26_000%26,
			'A' + counter/1000%26,
			'0' + counter/100%10,
			'0' + counter/10%10,
			'0' + counter%10,
		},
	)
}

func (r *Robot) Name() (string, error) {
	if r.name == "" {
		r.name = newName()
	}
	return r.name, nil
}

func (r *Robot) Reset() {
	r.name = ""
}
