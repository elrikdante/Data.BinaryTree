-- |

module Data.BinaryTree.Main where

data Tree a  = Node a | Branch (Tree a) (Tree a)

showTree              :: Show a =>Tree a -> String
showTree (Node x)     = show x
showTree (Branch l r) = "<" ++ showTree l ++ "|" ++ showTree r ++ ">"

showsTree              :: Show a => Tree a -> String -> String
showsTree (Node x)     = shows x
showsTree (Branch l r) = ('<':) . showsTree l . ('|':) . showsTree r . ('>':)

t :: Tree Int
t = Branch
    (Node 1)
    (Branch (Node 2)
     (Branch
      (Branch (Node 4) (Node 5))
      (Node 6)
     )
    )
