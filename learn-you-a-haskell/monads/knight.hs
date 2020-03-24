type KnightPosition = (Int,Int)

class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

instance MonadPlus [] where
  mzero = []
  mplus = (++)
  
guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

movek :: KnightPosition -> [KnightPosition]
movek (c, r) = do
  (c', r') <- [(c+2, r-1), (c+2, r+1), (c+1, r+2), (c-1, r+2)
               , (c-2, r+1), (c-2, r-1), (c-1, r-2), (c+1, r-2)
               ]
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (c',r')

-- Check in three steps (6,2) to (6,1)?

-- output "functor" form
-- movek(6,2) (one step)
-- fmap movek (movek(6,2)) (two steps)
-- fmap movek (concat $ (fmap movek (movek(6,2))) (three steps)

-- output monad form
t3 start = return start >>= movek >>= movek >>= movek
-- t3 (6,2)

  
--predicate
in3steps :: KnightPosition -> KnightPosition -> Bool
in3steps start end = end `elem` t3 start
