{-# LANGUAGE LambdaCase #-}

module Change (findFewestCoins) where

import Control.Applicative ((<|>))
import Data.Function (fix)
import Data.Set qualified as Set

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins 0 _ = Just []
findFewestCoins target coins =
    fix
        ( \continue visited_values -> \case
            [] -> Nothing
            coinset : coinsets_to_check ->
                lookup target new_values_coinsets
                    <|> continue
                        (visited_values <> Set.fromList new_values)
                        (coinsets_to_check <> new_coinsets)
              where
                value = sum coinset
                (new_values, new_coinsets) = unzip new_values_coinsets
                new_values_coinsets =
                    [ (new_value, coin : coinset)
                    | coin <- coins
                    , let new_value = coin + value
                    , new_value <= target
                    , new_value `notElem` visited_values
                    ]
        )
        Set.empty
        [[]]
