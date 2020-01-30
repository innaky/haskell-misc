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
