package complexnumbers

import "math"

type Number struct{ re, im float64 }

func (n Number) Real() float64 { return n.re }

func (n Number) Imaginary() float64 { return n.im }

func (a Number) Add(b Number) Number {
	return Number{re: a.re + b.re, im: a.im + b.im}
}

func (a Number) Subtract(b Number) Number {
	return Number{re: a.re - b.re, im: a.im - b.im}
}

func (a Number) Multiply(b Number) Number {
	return Number{re: a.re*b.re - a.im*b.im, im: a.re*b.im + a.im*b.re}
}

func (n Number) Times(f float64) Number {
	return Number{re: n.re * f, im: n.im * f}
}

func (a Number) Divide(b Number) Number {
	d := b.re*b.re + b.im*b.im
	return Number{
		re: (a.re*b.re + a.im*b.im) / d,
		im: (a.im*b.re - a.re*b.im) / d,
	}

}

func (n Number) Conjugate() Number { return Number{re: n.re, im: -n.im} }

func (n Number) Abs() float64 { return math.Sqrt(n.re*n.re + n.im*n.im) }

func (n Number) Exp() Number {
	f := math.Exp(n.re)
	return Number{re: f * math.Cos(n.im), im: f * math.Sin(n.im)}
}
