module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree (..))

treeFromTraversals :: (Ord a) => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals preorder inorder
    | length preorder /= length inorder = Nothing
    | otherwise =
        case go preorder inorder of
            Just Leaf -> Nothing
            x -> x
  where
    go preorder inorder =
        case preorder of
            [] -> Just Leaf
            root : preorder' ->
                case break (== root) inorder of
                    (inorderLeft, _ : inorderRight)
                        | root `notElem` inorderRight ->
                            let (preorderLeft, preorderRight) =
                                    splitAt (length inorderLeft) preorder'
                             in Branch
                                    <$> go preorderLeft inorderLeft
                                    <*> pure root
                                    <*> go preorderRight inorderRight
                    _ -> Nothing
