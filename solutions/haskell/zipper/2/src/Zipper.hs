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
    { btValue :: a
    , btLeft :: Maybe (BinTree a)
    , btRight :: Maybe (BinTree a)
    }
    deriving (Eq, Show)

data ParentLink a = Top | ParentOfLeft (Parent a) | ParentOfRight (Parent a)
    deriving (Eq, Show)

data Parent a = Parent
    { pParent :: ParentLink a
    , pValue :: a
    , pSibling :: Maybe (BinTree a)
    }
    deriving (Eq, Show)

data Zipper a = Zipper {zParent :: ParentLink a, zSubtree :: BinTree a}
    deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree zSubtree = Zipper{zParent = Top, zSubtree}

toTree :: Zipper a -> BinTree a
toTree z@Zipper{zSubtree} = maybe zSubtree toTree $ up z

value :: Zipper a -> a
value Zipper{zSubtree = BT{btValue}} = btValue

left :: Zipper a -> Maybe (Zipper a)
left Zipper{zParent, zSubtree = BT{btLeft, btValue, btRight}} =
    btLeft <&> \child ->
        Zipper
            { zParent =
                ParentOfLeft
                    Parent
                        { pParent = zParent
                        , pValue = btValue
                        , pSibling = btRight
                        }
            , zSubtree = child
            }

right :: Zipper a -> Maybe (Zipper a)
right Zipper{zParent, zSubtree = BT{btLeft, btValue, btRight}} =
    btRight <&> \child ->
        Zipper
            { zParent =
                ParentOfRight
                    Parent
                        { pParent = zParent
                        , pValue = btValue
                        , pSibling = btLeft
                        }
            , zSubtree = child
            }

up :: Zipper a -> Maybe (Zipper a)
up Zipper{zParent, zSubtree} =
    case zParent of
        Top -> Nothing
        ParentOfLeft Parent{pParent, pValue, pSibling} ->
            Just
                Zipper
                    { zParent = pParent
                    , zSubtree = BT pValue (Just zSubtree) pSibling
                    }
        ParentOfRight Parent{pParent, pValue, pSibling} ->
            Just
                Zipper
                    { zParent = pParent
                    , zSubtree = BT pValue pSibling (Just zSubtree)
                    }

setValue :: a -> Zipper a -> Zipper a
setValue btValue z@Zipper{zSubtree} = z{zSubtree = zSubtree{btValue}}

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft btLeft z@Zipper{zSubtree} = z{zSubtree = zSubtree{btLeft}}

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight btRight z@Zipper{zSubtree} = z{zSubtree = zSubtree{btRight}}
