{-# LANGUAGE LambdaCase #-}

module LinkedList (
    LinkedList,
    datum,
    fromList,
    isNil,
    new,
    next,
    nil,
    reverseLinkedList,
    toList,
) where

import GHC.Stack

data LinkedList a = Cons a (LinkedList a) | Nil deriving (Eq, Show)

list :: (a -> LinkedList a -> b) -> b -> LinkedList a -> b
list f z = \case Cons x xs -> f x xs; Nil -> z

instance Foldable LinkedList where foldr f z = list ((. foldr f z) . f) z

append :: a -> LinkedList a -> LinkedList a
append x = foldr Cons (Cons x Nil)

datum :: (HasCallStack) => LinkedList a -> a
datum = list const (error "list must not be empty")

fromList :: [a] -> LinkedList a
fromList = foldr Cons Nil

isNil :: LinkedList a -> Bool
isNil = list (\_ _ -> False) True

new :: a -> LinkedList a -> LinkedList a
new = Cons

next :: (HasCallStack) => LinkedList a -> LinkedList a
next = list (\_ xs -> xs) (error "list must not be empty")

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = foldr append Nil

toList :: LinkedList a -> [a]
toList = foldr (:) []
