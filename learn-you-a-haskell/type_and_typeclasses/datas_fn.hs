data Nat = Zero | succ Nat
data List a = Nil | Cons a (List a)
data Tree a = Leaf a | Node (Tree a) a (Tree a)
data Maybe a = Nothing | Just a

-- List
len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

-- Trees
exist :: Ord a => a -> Tree a -> Bool
exist a (Leaf b)     = a == b
exist a (Tree l b r) 
  | a == b = True
  | a < b = exist a l 
  | a > b = exist a r

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r
