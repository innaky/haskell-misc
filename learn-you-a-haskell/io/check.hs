check :: String -> Maybe String
check [] = Nothing
check (x:xs) = Just [x]

checkpol :: [a] -> Maybe [a]
check [] = Nothing
check (x:xs) = Just [x]
