package account

import (
	"sync/atomic"
)

type Account struct {
	state atomic.Int64
}

func Open(amount int64) *Account {
	if amount < 0 {
		return nil
	}
	account := Account{}
	account.state.Store(amount)
	return &account
}

func (a *Account) Balance() (int64, bool) {
	state := a.state.Load()
	if state < 0 {
		return 0, false
	}
	return state, true
}

func atomicallyModifyBalance(
	a *atomic.Int64,
	action func(int64) (storeNeeded bool, newValue int64),
) (ok bool, old int64, new int64) {
	success := false
	for !success {
		old = a.Load()
		ok, new = action(old)
		if !(old >= 0 && ok) {
			return
		}
		success = a.CompareAndSwap(old, new)
	}
	return
}

func (a *Account) Deposit(amount int64) (int64, bool) {
	ok, _, newState := atomicallyModifyBalance(
		&a.state,
		func(state int64) (bool, int64) {
			return state >= 0 && -amount <= state, state + amount
		},
	)
	return newState, ok
}

func (a *Account) Close() (int64, bool) {
	ok, old, _ := atomicallyModifyBalance(
		&a.state,
		func(state int64) (bool, int64) { return state >= 0, -1 },
	)
	return max(old, 0), ok
}
