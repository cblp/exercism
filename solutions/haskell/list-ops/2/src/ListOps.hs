{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ListOps (
    length,
    reverse,
    map,
    filter,
    foldr,
    foldl',
    (++),
    concat,
) where

import Prelude (Bool, Int, flip, succ, (.))

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z [] = z
foldl' f z (x : xs) = let !y = f z x in foldl' f y xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x : xs) = f x (foldr f z xs)

length :: [a] -> Int
length = foldl' (\acc _ -> succ acc) 0

reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []

map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x acc -> if p x then x : acc else acc) []

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat = foldr (++) []
