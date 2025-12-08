package diffiehellman

import (
	"crypto/rand"
	"math/big"
)

var (
	zero   = big.NewInt(0)
	one    = big.NewInt(1)
	two    = big.NewInt(2)
	bigint = new(big.Int)
)

func eq(a, b *big.Int) bool {
	return a.Cmp(b) == 0
}

// Diffie-Hellman-Merkle key exchange
// Private keys should be generated randomly.

func PrivateKey(p *big.Int) *big.Int {
	a, err := rand.Int(rand.Reader, bigint.Sub(p, two))
	if err != nil {
		panic(err)
	}
	return a.Add(a, two)
}

func pow_mod(g, a, p *big.Int) *big.Int {
	if eq(a, one) {
		return bigint.Mod(g, p)
	}
	var a1 big.Int
	var a0 big.Int
	a1.DivMod(a, two, &a0)
	g1 := pow_mod(g, &a1, p)
	g1.Mul(g1, g1)
	g1.Mod(g1, p)
	if eq(&a0, zero) {
		return g1
	}
	g1.Mul(g1, g)
	return g1.Mod(g1, p)
}

func PublicKey(a, p *big.Int, g int64) *big.Int {
	return pow_mod(big.NewInt(g), a, p)
}

func NewPair(p *big.Int, g int64) (*big.Int, *big.Int) {
	a := PrivateKey(p)
	A := PublicKey(a, p, g)
	s := SecretKey(a, A, p)
	return A, s
}

func SecretKey(private1, public2, p *big.Int) *big.Int {
	return pow_mod(public2, private1, p)
}
