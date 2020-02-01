doubleMe x = x + x

--doubleUs x y = x*2 + y*2
doubleUS x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                         then x
                      else doubleMe x

onlylowercase :: [Char] -> [Char] --not necesary, the compiler can infer by itself
onlylowercase xs = [x | x <- xs, x `elem` ['a'..'z']]

fact :: Integer -> Integer
fact 0 = 1
fact x = x * fact(x-1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- [1,2,3] is just syntactic sugar for 1:2:3:[]
-- A pattern like for head is x:xs, other pattern x:y:z:zs

car' :: [a] -> a
car' [] = error "This is an empty list."
car' (x:_) = x

cdr' :: [a] -> [a]
cdr' [] = error "This is an empty list."
cdr' (_:xs) = xs

cadr' :: [a] -> a
cadr' [] = error "This is an empty list."
cadr' xs = car' (cdr' xs)

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs
