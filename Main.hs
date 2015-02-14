-- |authour: Dante Elrik

module Data.BinaryTree.Main where
import Data.List (intersperse)
import Prelude hiding (join) -- future proofing =)

data Tree a    = Node a
               | Branch (Tree a) (Tree a)
type Network a = [Tree a]

join         :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)

showsTree              :: Show a => Tree a -> ShowS
showsTree (Node x)     = shows x
showsTree (Branch l r) = ('<':) . showsTree l . ('|':) . showsTree r . ('>':)

showTree t = showsTree t ""

readsTree          :: Read a => ReadS (Tree a)
readsTree ('<':s)  = do
  (l, '|':ls) <- readsTree s
  (r, '>':u)  <- readsTree ls
  return (Branch l r,u)
readsTree s        = do
  (x, t) <- reads s
  return (Node x,t)

readsNetwork             :: Read a => String -> Network a
readsNetwork ""          = []
readsNetwork enc'Network = do
  (tree, ('\n':enc'trees)) <- readsTree enc'Network
  tree : (readsNetwork enc'trees)

t :: Tree Int
t = Branch
    (Node 1)
    (Branch (Node 2)
     (Branch
      (Branch (Node 4) (Node 5))
      (Node 6)
     )
    )

t'dec     :: [(Tree Int, String)]
t'dec     = readsTree t'enc
t'enc     = showTree t
network   :: Int -> String
network s = join "\n" $ take s $ repeat t'enc
