{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Prelude hiding (head, last)

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

data Node a = Node {value :: a, next, prev :: IORef (Maybe (Node a))}

data Deque a = Deque {head, last :: IORef (Maybe (Node a))}

mkDeque :: IO (Deque a)
mkDeque = Deque <$> newIORef Nothing <*> newIORef Nothing

newNode :: a -> IO (Node a)
newNode x = Node x <$> newIORef Nothing <*> newIORef Nothing

pop' ::
    IORef (Maybe (Node a)) ->
    IORef (Maybe (Node a)) ->
    (Node a -> IORef (Maybe (Node a))) ->
    IO (Maybe a)
pop' thisEnd otherEnd neighbor =
    readIORef thisEnd
        >>= traverse \node -> do
            adj <- readIORef $ neighbor node
            thisEnd .= adj
            maybe otherEnd neighbor adj .= Nothing
            pure node.value

pop, shift :: Deque a -> IO (Maybe a)
pop Deque{head, last} = pop' last head prev
shift Deque{head, last} = pop' head last next

push' ::
    IORef (Maybe (Node a)) ->
    IORef (Maybe (Node a)) ->
    (Node a -> IORef (Maybe (Node a))) ->
    (Node a -> IORef (Maybe (Node a))) ->
    a ->
    IO ()
push' thisEnd otherEnd myRef theirRef x = do
    node <- newNode x
    readIORef thisEnd >>= \case
        this@(Just existing) -> do
            myRef node .= this
            theirRef existing .=! node
        Nothing -> otherEnd .=! node
    thisEnd .=! node

push, unshift :: Deque a -> a -> IO ()
push Deque{head, last} = push' last head prev next
unshift Deque{head, last} = push' head last next prev

(.=) :: IORef a -> a -> IO ()
(.=) = writeIORef

(.=!) :: IORef (Maybe a) -> a -> IO ()
r .=! x = writeIORef r $ Just x
