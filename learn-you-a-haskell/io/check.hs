check :: String -> Maybe String
check [] = Nothing
check (x:xs) = Just [x]
