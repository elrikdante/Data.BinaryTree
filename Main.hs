-- |

module Data.BinaryTree.Main where

data Tree a  = Node a | Branch (Tree a) (Tree a)

type Network a = [Tree a]

xshowTree               :: Show a =>Tree a -> String
xshowTree (Node x)      = show x
xshowTree (Branch l r)  = "<" ++ showTree l ++ "|" ++ showTree r ++ ">"

showsTree              :: Show a => Tree a -> ShowS
showsTree (Node x)     = shows x
showsTree (Branch l r) = ('<':) . showsTree l . ('|':) . showsTree r . ('>':)

showTree t = showsTree t ""

readsTree          :: Read a => ReadS (Tree a)
readsTree ('<':s)  = do
  (l, '|':ls) <- readsTree s
  (r, '>':u)  <- readsTree ls
  return (Branch l r,u)
readsTree s = do
  (x, t) <- reads s
  return (Node x,t)

readsTrees :: String -> Network a
readsTrees ""          = []
readsTrees enc'Network = do
  tree <- readsTree enc'tree
  fst tree : readsTrees enc'trees
  where
    (enc'tree:enc'trees) = lines enc'Network



t :: Tree Int
t = Branch
    (Node 1)
    (Branch (Node 2)
     (Branch
      (Branch (Node 4) (Node 5))
      (Node 6)
     )
    )
t'enc  = showTree t
t'dec :: [(Tree Int, String)]
t'dec  = readsTree t'enc
