class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  x >> y = x >>= \_ -> y
  fail :: String -> m a
  fail msg -> error msg

instance Monad Maybe where
  return x = Just x
  Nothing >>= f = Nothing
  Just x >>= f = f x
  fail _ = Nothing

instance Monad [] where
  return x = [x]
  xs >>= f = concat (map f xs)
  fail _ = []

-- monad reader
instance Monad ((->) r) where
  return x = \_ -> x
  h >>= f = \w -> f (h w) w
