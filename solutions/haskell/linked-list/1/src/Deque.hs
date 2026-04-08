{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Prelude hiding (head, last)

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

data Node a = Node {value :: a, next, prev :: IORef (Maybe (Node a))}

data Deque a = Deque {head, last :: IORef (Maybe (Node a))}

mkDeque :: IO (Deque a)
mkDeque = do
    head <- newIORef Nothing
    last <- newIORef Nothing
    pure Deque{..}

pop :: Deque a -> IO (Maybe a)
pop deque = do
    deque_last <- readIORef deque.last
    case deque_last of
        Nothing -> pure Nothing
        Just deque_last_ -> do
            deque_last_prev <- readIORef deque_last_.prev
            writeIORef deque.last deque_last_prev
            case deque_last_prev of
                Just deque_last_prev_ ->
                    writeIORef deque_last_prev_.next Nothing
                Nothing ->
                    writeIORef deque.head Nothing
            pure $ Just deque_last_.value

shift :: Deque a -> IO (Maybe a)
shift deque = do
    deque_head <- readIORef deque.head
    case deque_head of
        Nothing -> pure Nothing
        Just deque_head_ -> do
            deque_head_next <- readIORef deque_head_.next
            writeIORef deque.head deque_head_next
            case deque_head_next of
                Just deque_head_next_ ->
                    writeIORef deque_head_next_.prev Nothing
                Nothing ->
                    writeIORef deque.last Nothing
            pure $ Just deque_head_.value

push :: Deque a -> a -> IO ()
push deque x = do
    node_next <- newIORef Nothing
    node_prev <- newIORef Nothing
    let node = Node{next = node_next, prev = node_prev, value = x}

    deque_last <- readIORef deque.last
    case deque_last of
        Just _ -> do
            writeIORef node_prev deque_last
            writeIORef deque.last $ Just node
            do
                Just node_prev_ <- readIORef node_prev
                writeIORef node_prev_.next $ Just node
        Nothing -> do
            writeIORef deque.head $ Just node
            writeIORef deque.last $ Just node

unshift :: Deque a -> a -> IO ()
unshift deque x = do
    node_next <- newIORef Nothing
    node_prev <- newIORef Nothing
    let node = Node{next = node_next, prev = node_prev, value = x}

    deque_heed <- readIORef deque.head
    case deque_heed of
        Just _ -> do
            writeIORef node_next deque_heed
            writeIORef deque.head $ Just node
            do
                Just node_next_ <- readIORef node_next
                writeIORef node_next_.prev $ Just node
        Nothing -> do
            writeIORef deque.last $ Just node
            writeIORef deque.head $ Just node
