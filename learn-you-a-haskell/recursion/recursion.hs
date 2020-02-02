replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n elem
  | n <= 0 = []
  | otherwise = elem:replicate' (n - 1) elem

maxr' :: (Ord a) => [a] -> a
maxr' [] = error "This is an empty list"
maxr' [x] = x
maxr' (x:xs)
  | x > maxr' xs = x
  | otherwise = maxr' xs

take' :: (Num a, Ord a) => a -> [b] -> [b]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (h:t) = h: take' (n - 1) t

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) =  reverse' t ++ [h]

-- return x repeat infinite times
repeat' :: a -> [a]
repeat' x = x:repeat x

zip' _ [] = []
zip' [] _ = []
zip' (h:t) (x:xs) = [(h,x)] ++ zip' t xs
--zip' (h:t) (x:xs) = (h,x):zip' t xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (a:bc)
  | x == a = True
  | otherwise = elem' x bc

-- awesome
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let before = quicksort [ a | a <- xs, a <= x]
      after = quicksort [ a | a <- xs, a > x]
  in before ++ [x] ++ after
  
