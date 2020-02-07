-- this source is redundant, best use let bindings
main = do
  a <- return "hahah"
  b <- return "other string"
  putStrLn $ a ++ " " ++ b

-- with let bindings
-- main = do
--   let a = "hahha"
--       b = "other string"
--   putStrLn $ a ++ " " ++ b
