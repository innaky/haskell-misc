check :: String -> Maybe String
check (x:xs) = Just [x]
check _ = Nothing

checkgen :: [a] -> Maybe [a]
checkgen (x:_) = Just [x]
checkgen _ = Nothing

checkEmpty :: [[Char]] -> Maybe [[Char]]
checkEmpty [] = Nothing
checkEmpty input = Just input
