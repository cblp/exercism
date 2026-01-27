{-# LANGUAGE LambdaCase #-}

module BST (
    BST,
    bstLeft,
    bstRight,
    bstValue,
    empty,
    fromList,
    insert,
    singleton,
    toList,
) where

data BST a = Empty | Node (BST a) a (BST a)
    deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft = \case
    Empty -> Nothing
    Node left _ _ -> Just left

bstRight :: BST a -> Maybe (BST a)
bstRight = \case
    Empty -> Nothing
    Node _ _ right -> Just right

bstValue :: BST a -> Maybe a
bstValue = \case
    Empty -> Nothing
    Node _ v _ -> Just v

empty :: BST a
empty = Empty

fromList :: (Ord a) => [a] -> BST a
fromList = foldl (flip insert) empty

insert :: (Ord a) => a -> BST a -> BST a
insert x = \case
    Empty -> singleton x
    Node left v right
        | x <= v -> Node (insert x left) v right
        | otherwise -> Node left v (insert x right)

singleton :: a -> BST a
singleton x = Node Empty x Empty

toList :: BST a -> [a]
toList = \case
    Empty -> []
    Node left v right -> toList left ++ v : toList right
