{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Zipper (
    BinTree (BT),
    fromTree,
    left,
    right,
    setLeft,
    setRight,
    setValue,
    toTree,
    up,
    value,
) where

import Data.Functor ((<&>))

data BinTree a = BT
    { value :: a
    , left :: Maybe (BinTree a)
    , right :: Maybe (BinTree a)
    }
    deriving (Eq, Show)

data ParentLink a = Top | ParentOfLeft (Parent a) | ParentOfRight (Parent a)
    deriving (Eq, Show)

data Parent a = P
    { parent :: ParentLink a
    , value :: a
    , sibling :: Maybe (BinTree a)
    }
    deriving (Eq, Show)

data Zipper a = Z
    { parent :: ParentLink a
    , currentSubtree :: BinTree a
    }
    deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Z{parent = Top, currentSubtree = tree}

toTree :: Zipper a -> BinTree a
toTree z = maybe z.currentSubtree toTree $ up z

value :: Zipper a -> a
value zipper = zipper.currentSubtree.value

left :: Zipper a -> Maybe (Zipper a)
left Z{parent, currentSubtree} =
    currentSubtree.left <&> \child ->
        Z
            { parent =
                ParentOfLeft
                    P
                        { parent
                        , value = currentSubtree.value
                        , sibling = currentSubtree.right
                        }
            , currentSubtree = child
            }

right :: Zipper a -> Maybe (Zipper a)
right Z{parent, currentSubtree} =
    currentSubtree.right <&> \child ->
        Z
            { parent =
                ParentOfRight
                    P
                        { parent
                        , value = currentSubtree.value
                        , sibling = currentSubtree.left
                        }
            , currentSubtree = child
            }

up :: Zipper a -> Maybe (Zipper a)
up z =
    case z.parent of
        Top -> Nothing
        ParentOfLeft p ->
            Just
                Z
                    { parent = p.parent
                    , currentSubtree =
                        BT
                            { value = p.value
                            , left = Just z.currentSubtree
                            , right = p.sibling
                            }
                    }
        ParentOfRight p ->
            Just
                Z
                    { parent = p.parent
                    , currentSubtree =
                        BT
                            { value = p.value
                            , left = p.sibling
                            , right = Just z.currentSubtree
                            }
                    }

setValue :: a -> Zipper a -> Zipper a
setValue x z =
    z
        { currentSubtree =
            BT
                { value = x
                , left = z.currentSubtree.left
                , right = z.currentSubtree.right
                }
        }

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft t z =
    z
        { currentSubtree =
            BT
                { value = z.currentSubtree.value
                , left = t
                , right = z.currentSubtree.right
                }
        }

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight t z =
    z
        { currentSubtree =
            BT
                { value = z.currentSubtree.value
                , left = z.currentSubtree.left
                , right = t
                }
        }
