{-# LANGUAGE BlockArguments #-}

module BankAccount (
    BankAccount,
    closeAccount,
    getBalance,
    incrementBalance,
    openAccount,
) where

import Control.Concurrent.STM (
    TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVar,
    readTVarIO,
    writeTVar,
 )

newtype BankAccount = BankAccount (TVar (Maybe Integer))

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount tv) = atomically $ writeTVar tv Nothing

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount tv) = readTVarIO tv

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount tv) amount =
    atomically do
        modifyTVar' tv $ fmap (+ amount)
        readTVar tv

openAccount :: IO BankAccount
openAccount = BankAccount <$> newTVarIO (Just 0)
