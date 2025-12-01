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

func (a *Account) Deposit(amount int64) (int64, bool) {
	success := false
	var newState int64
	for !success {
		oldState := a.state.Load()
		if oldState < 0 || -amount > oldState {
			return 0, false
		}
		newState = oldState + amount
		success = a.state.CompareAndSwap(oldState, newState)
	}
	return newState, true
}

func (a *Account) Close() (int64, bool) {
	success := false
	var oldState int64
	for !success {
		oldState = a.state.Load()
		if oldState < 0 {
			return 0, false
		}
		success = a.state.CompareAndSwap(oldState, -1)
	}
	return oldState, true
}
