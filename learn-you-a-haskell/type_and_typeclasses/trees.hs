data Tree a = EmpthyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmpthyTree EmpthyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmpthyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmpthyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

-- let nums = [1..13]
-- let numsTree = foldr treeInsert EmpthyTree nums
-- treeElem 8 numsTree
-- treeElem 20 numsTree
