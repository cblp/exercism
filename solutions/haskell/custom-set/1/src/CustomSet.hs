{-# LANGUAGE LambdaCase #-}

module CustomSet (
    delete,
    difference,
    empty,
    fromList,
    insert,
    intersection,
    isDisjointFrom,
    isSubsetOf,
    member,
    null,
    size,
    toList,
    union,
) where

import Data.Foldable (toList)

data CustomSet a = Empty | Node (CustomSet a) a (CustomSet a)
    deriving (Show)

instance Foldable CustomSet where
    foldr f z = \case
        Empty -> z
        Node left x right -> foldr f (f x (foldr f z right)) left

instance (Eq a) => Eq (CustomSet a) where
    setA == setB = toList setA == toList setB

delete :: (Ord a) => a -> CustomSet a -> CustomSet a
delete x = fromList . filter (/= x) . toList

difference :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
difference setA setB = fromList $ filter (not . (`elem` setB)) (toList setA)

empty :: CustomSet a
empty = Empty

fromList :: (Ord a) => [a] -> CustomSet a
fromList = foldr insert empty

insert :: (Ord a) => a -> CustomSet a -> CustomSet a
insert x = \case
    Empty -> Node Empty x Empty
    Node left y right
        | x < y -> Node (insert x left) y right
        | x > y -> Node left y (insert x right)
        | otherwise -> Node left y right

intersection :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
intersection setA setB = fromList $ filter (`elem` setB) (toList setA)

isDisjointFrom :: (Ord a) => CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA setB = null $ intersection setA setB

isSubsetOf :: (Eq a) => CustomSet a -> CustomSet a -> Bool
isSubsetOf setA setB = all (`elem` setB) setA

member :: (Eq a) => a -> CustomSet a -> Bool
member = elem

size :: CustomSet a -> Int
size = length

union :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
union setA setB = fromList $ toList setA ++ toList setB
