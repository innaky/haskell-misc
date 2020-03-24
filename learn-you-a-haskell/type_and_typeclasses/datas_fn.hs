data Nat = Zero | succ Nat
data List a = Nil | Cons a (List a)
data Tree a = Leaf a | Node (Tree a) a (Tree a)
data Maybe a = Nothing | Just a
data Some a b = Left a | Right b
data Shape = Circle Float | Rec Float Float

instance (Eq a) => Eq (Maybe a) where
  Nothing == Nothing = True
  Just x  == Just y  = x == y
  _       == _       = False

instance Eq Nat where
  Zero == Zero = True
  Nat  == Nat  = True
  _    == _    = False

instance Eq List where
  Nil      == Nil    = True
  Cons a b == Cons c d = (a == c) && (b == d) 
  _        == _      = False

instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just a) =  (Just f a)

instance Functor [] where
  fmap f Nil = Nil
  fmap f (Cons a b) = Cons (f a) (fmap Cons f b)

instance Functor (Some a) where
  fmap _ (Left a) = Left a
  fmap f (Right a) = Right (f a)

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)
  
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
