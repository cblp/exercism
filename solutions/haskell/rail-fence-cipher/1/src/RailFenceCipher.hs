{-# LANGUAGE BlockArguments #-}

module RailFenceCipher (encode, decode) where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Foldable (for_, toList)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef)
import Data.Vector (Vector, freeze, (!))
import Data.Vector qualified as Vector
import Data.Vector.Mutable (MVector, new, write)

data Codec = Encode | Decode

encode :: Int -> String -> String
encode = subst Encode

decode :: Int -> String -> String
decode = subst Decode

subst :: Codec -> Int -> String -> String
subst codec rails messageStr =
    runST
        ( do
            out <- new $ length message
            cipherIx <- newSTRef 0
            for_ [0 .. rails - 1] \r -> do
                step <- newSTRef $ 2 * r
                plainIx <- newSTRef r
                while (plainIx <? length message) do
                    case codec of
                        Decode -> (out, plainIx) =. (message, cipherIx)
                        Encode -> (out, cipherIx) =. (message, plainIx)
                    modifySTRef cipherIx (+ 1)
                    whenM (step /=? maxStep) $ modifySTRef step (maxStep -)
                    plainIx += step
            freeze out
        )
        & toList
  where
    message = Vector.fromList messageStr
    maxStep = 2 * (rails - 1)

while :: (Monad m) => m Bool -> m () -> m ()
while cond body = go where go = whenM cond $ body *> go

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM cond body = do c <- cond; when c body

(=.) :: (MVector s Char, STRef s Int) -> (Vector Char, STRef s Int) -> ST s ()
(dst, i) =. (src, j) = do
    i_ <- readSTRef i
    j_ <- readSTRef j
    write dst i_ $ src ! j_

(+=) :: STRef s Int -> STRef s Int -> ST s ()
x += y = do
    y_ <- readSTRef y
    modifySTRef x (+ y_)

(<?) :: (Ord a) => STRef s a -> a -> ST s Bool
x <? y = readSTRef x <&> (< y)

(/=?) :: (Eq a) => STRef s a -> a -> ST s Bool
x /=? y = readSTRef x <&> (/= y)
