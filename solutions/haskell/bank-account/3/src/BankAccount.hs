{-# LANGUAGE BlockArguments #-}
{-# OPTIONS -Wno-missing-signatures #-}

module BankAccount (
    BankAccount,
    closeAccount,
    getBalance,
    incrementBalance,
    openAccount,
) where

import Data.IORef

newtype BankAccount = BankAccount (IORef (Maybe Integer))

openAccount = BankAccount <$> newIORef (Just 0)

closeAccount (BankAccount ref) = atomicWriteIORef ref Nothing

getBalance (BankAccount ref) = readIORef ref

incrementBalance (BankAccount ref) amount =
    atomicModifyIORef' ref \x -> let x' = (amount +) <$> x in (x', x')
