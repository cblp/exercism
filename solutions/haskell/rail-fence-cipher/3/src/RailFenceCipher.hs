{-# LANGUAGE BlockArguments #-}

module RailFenceCipher (encode, decode) where

import Control.Monad.ST (runST)
import Data.Foldable (for_, toList)
import Data.Functor ((<&>))
import Data.List (unfoldr)
import Data.Vector (freeze, (!))
import Data.Vector qualified as Vector
import Data.Vector.Mutable (new, write)

data Codec = Encode | Decode

encode :: Int -> String -> String
encode = subst Encode

decode :: Int -> String -> String
decode = subst Decode

subst :: Codec -> Int -> String -> String
subst codec rails inputStr =
    runST do
        output <- new $ length input
        for_ (zip cipherIxs plainIxs) \(cipherIx, plainIx) ->
            case codec of
                Decode -> write output plainIx $ input ! cipherIx
                Encode -> write output cipherIx $ input ! plainIx
        freeze output <&> toList
  where
    input = Vector.fromList inputStr
    plainIxs = makePlainIndices rails $ length input
    cipherIxs = [0 ..]

makePlainIndices :: Int -> Int -> [Int]
makePlainIndices rails messageLength = concatMap railPlainIxs [0 .. rails - 1]
  where
    maxStep = 2 * (rails - 1)

    nextStep s = if s /= maxStep then maxStep - s else s

    railPlainIxs rail
        | rails == 1 = takeWhile (< messageLength) [rail ..]
        | otherwise = unfoldr step (rail, nextStep (2 * rail))

    step (ix, s) =
        (ix < messageLength) `thenJust` (ix, (ix + s, nextStep s))

thenJust :: Bool -> a -> Maybe a
thenJust False _ = Nothing
thenJust True x = Just x
