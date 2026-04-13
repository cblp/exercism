module POV (fromPOV, tracePathBetween) where

import Data.Foldable (asum)
import Data.List (delete)
import Data.Tree (Tree (..))

fromPOV :: (Eq a) => a -> Tree a -> Maybe (Tree a)
fromPOV newRoot = go id
  where
    go addChild parent@(Node root children)
        | root == newRoot = Just $ Node root (addChild children)
        | otherwise =
            asum
                [ go (parent' :) child
                | child <- children
                , let parent' =
                        parent{subForest = addChild $ delete child children}
                ]

tracePathBetween :: (Eq a) => a -> a -> Tree a -> Maybe [a]
tracePathBetween a b tree =
    merge (rootLabel tree) <$> path a tree <*> path b tree
  where
    path x (Node root children)
        | x == root = Just []
        | otherwise =
            asum [(rootLabel child :) <$> path x child | child <- children]

    merge _ (x : xs) (y : ys) | x == y = merge x xs ys
    merge common xs ys = reverse xs ++ common : ys
