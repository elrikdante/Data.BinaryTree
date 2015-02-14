-- |

module Data.BinaryTree.Main where

data Tree a  = Node a | Branch (Tree a) (Tree a)

showTree              :: Show a =>Tree a -> String
showTree (Node x)     = shows x ""
showTree (Branch l r) = "<" ++ showTree l ++ "|" ++ showTree r ++ ">"

t :: Tree Int
t = Branch
    (Node 1)
    (Branch (Node 2)
     (Branch
      (Branch (Node 4) (Node 5))
      (Node 6)
     )
    )