main = do
  return () --it's a bogus I/O action... that doesn't do anything
  return "Any" -- return takes a value and wraps it up in a "box"
  line <- getLine -- <- takes a box and takes the value out of it
  return "Other String"
  return 4
  putStrLn line
