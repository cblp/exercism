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

type BankAccount = IORef (Maybe Integer)

openAccount = newIORef $ Just (0 :: Integer)

closeAccount = (`atomicWriteIORef` Nothing)

getBalance = readIORef

incrementBalance ref amount =
    atomicModifyIORef' ref \x -> let x' = (amount +) <$> x in (x', x')
