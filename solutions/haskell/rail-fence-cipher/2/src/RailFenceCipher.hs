{-# LANGUAGE BlockArguments #-}

module RailFenceCipher (encode, decode) where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Foldable (for_, toList)
import Data.Functor ((<&>))
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef)
import Data.Traversable (for)
import Data.Vector (freeze, (!))
import Data.Vector qualified as Vector
import Data.Vector.Mutable (new, write)

data Codec = Encode | Decode

encode :: Int -> String -> String
encode = subst Encode

decode :: Int -> String -> String
decode = subst Decode

subst :: Codec -> Int -> String -> String
subst codec rails messageStr =
    runST do
        out <- new $ length message
        for_ (indices rails $ length message) \(plainIx, cipherIx) ->
            case codec of
                Decode -> write out plainIx $ message ! cipherIx
                Encode -> write out cipherIx $ message ! plainIx
        freeze out <&> toList
  where
    message = Vector.fromList messageStr

indices :: Int -> Int -> [(Int, Int)]
indices rails messageLength =
    runST do
        cipherIx <- newSTRef 0
        ( for [0 .. rails - 1] \r -> do
                step <- newSTRef $ 2 * r
                plainIx <- newSTRef r
                while (plainIx <? messageLength) do
                    ixs <- (,) <$> readSTRef plainIx <*> readSTRef cipherIx
                    modifySTRef cipherIx (+ 1)
                    whenM (step /=? maxStep) $ modifySTRef step (maxStep -)
                    step_ <- readSTRef step
                    modifySTRef plainIx (+ step_)
                    pure ixs
            )
            <&> concat
  where
    maxStep = 2 * (rails - 1)

while :: (Monad m) => m Bool -> m a -> m [a]
while cond body = go
  where
    go = do c <- cond; if c then (:) <$> body <*> go else pure []

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM cond body = do c <- cond; when c body

(<?) :: (Ord a) => STRef s a -> a -> ST s Bool
x <? y = readSTRef x <&> (< y)

(/=?) :: (Eq a) => STRef s a -> a -> ST s Bool
x /=? y = readSTRef x <&> (/= y)
