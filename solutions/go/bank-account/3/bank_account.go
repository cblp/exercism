package account

import (
	"sync/atomic"
)

type AccountState struct {
	isOpen  bool
	balance int64
}

type Account struct{ state atomic.Value }

func Open(amount int64) *Account {
	if amount < 0 {
		return nil
	}
	account := Account{}
	account.state.Store(AccountState{isOpen: true, balance: amount})
	return &account
}

func (a *Account) Balance() (int64, bool) {
	state := a.state.Load().(AccountState)
	if !state.isOpen {
		return 0, false
	}
	return state.balance, true
}

func atomicallyModifyBalance(
	a *atomic.Value,
	action func(AccountState) (storeNeeded bool, newValue AccountState),
) (ok bool, old AccountState, new AccountState) {
	success := false
	for !success {
		old = a.Load().(AccountState)
		ok, new = action(old)
		if !ok {
			return
		}
		success = a.CompareAndSwap(old, new)
	}
	return
}

func (a *Account) Deposit(amount int64) (int64, bool) {
	ok, _, newState := atomicallyModifyBalance(
		&a.state,
		func(state AccountState) (bool, AccountState) {
			return state.isOpen && -amount <= state.balance,
				AccountState{
					balance: state.balance + amount,
					isOpen:  state.isOpen,
				}
		},
	)
	return newState.balance, ok
}

func (a *Account) Close() (int64, bool) {
	ok, old, _ := atomicallyModifyBalance(
		&a.state,
		func(state AccountState) (bool, AccountState) {
			return state.isOpen, AccountState{isOpen: false}
		},
	)
	return old.balance, ok
}
