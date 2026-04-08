{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

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
            deque.last .= deque_last_prev
            maybe deque.head (.next) deque_last_prev .= Nothing
            pure $ Just deque_last_.value

shift :: Deque a -> IO (Maybe a)
shift deque = do
    deque_head <- readIORef deque.head
    case deque_head of
        Nothing -> pure Nothing
        Just deque_head_ -> do
            deque_head_next <- readIORef deque_head_.next
            deque.head .= deque_head_next
            maybe deque.last (.prev) deque_head_next .= Nothing
            pure $ Just deque_head_.value

push :: Deque a -> a -> IO ()
push deque x = do
    node_next <- newIORef Nothing
    node_prev <- newIORef Nothing
    let node = Node{next = node_next, prev = node_prev, value = x}

    deque_last <- readIORef deque.last
    case deque_last of
        Just deque_last_ -> do
            node_prev .= deque_last
            deque_last_.next .=! node
        Nothing -> deque.head .=! node
    deque.last .=! node

unshift :: Deque a -> a -> IO ()
unshift deque x = do
    node_next <- newIORef Nothing
    node_prev <- newIORef Nothing
    let node = Node{next = node_next, prev = node_prev, value = x}

    deque_head <- readIORef deque.head
    case deque_head of
        Just deque_head_ -> do
            node_next .= deque_head
            deque_head_.prev .=! node
        Nothing -> deque.last .=! node
    deque.head .=! node

(.=) :: IORef a -> a -> IO ()
(.=) = writeIORef

(.=!) :: IORef (Maybe a) -> a -> IO ()
r .=! x = writeIORef r $ Just x
