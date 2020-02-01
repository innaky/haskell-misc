max' :: (Ord a) => a -> a -> a
max' x y
  | x > y = x
  | otherwise = y

compare' :: (Ord a) => a -> a -> Ordering
compare' a b
  | a > b = GT
  | a < b = LT
  | otherwise = EQ

increment [] = []
increment (x:xs) = x+1 : increment xs

decrement [] = []
decrement (x:xs) = x -1 : decrement xs

--map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- (a -> Bool) is a predicate and take another list with the same type of element of the predicate and
-- return a list with the same type of element.

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter p xs  -- predicate zone: if predicate element is True, element is appent in the list.
  | otherwise = filter p xs -- predicate zone: if not True, evaluate the next element.

increment' :: Num a => [a] -> [a]
increment' [] = []
increment' lst = map' (\x -> x + 1) lst

decrement' :: Num a => [a] -> [a]
decrement' [] = []
decrement' lst = map' (\x -> x - 1) lst

even' :: Integral a => [a] -> [a]
even' [] = []
even' lst = filter' (\x -> (mod x 2) == 0) lst

max_lst' :: (Ord a) => [a] -> a
max_lst' [] = error "This is an empthy list"
max_lst' [x] = x
max_lst' (x:xs)
  | x > temp_number = x
  | otherwise = temp_number
  where temp_number = max_lst' xs

max_other :: (Ord a) => [a] -> a
max_other [] = error "This is an empthy list"
max_other [x] = x
max_other (x:xs)
  | x > max_other xs = x
  | otherwise = max_other xs
