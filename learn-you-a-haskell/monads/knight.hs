import Control.Monad.Writer
import Data.List (replicate)

type Kn = (Int, Int)

movek :: Kn -> [Kn]
movek (c,r) = [(c+2, r-1), (c+2, r+1), (c+1, r+2), (c-1, r+2), (c-2, r+1), (c-2, r-1), (c-1, r-2), (c+1, r-2)]

validPosition :: (Eq a1, Eq a2, Num a1, Num a2, Enum a1, Enum a2) => [(a1, a2)] -> [(a1, a2)]
validPosition lstKn = filter (\(x, y) -> (x `elem` [1..8] && y `elem` [1..8])) lstKn

-- monadic composition (generic function)
tn :: Int -> Kn -> [Kn] 
tn n start = validPosition $ return start >>= foldr (<=<) return (replicate n movek)

t3 :: Kn -> [Kn]
t3 start = validPosition $ return start >>= movek >>= movek >>= movek
  
movekLog :: Kn -> Writer [String] [Kn]
movekLog (x,y) = do
  tell ["output " ++ show (x,y)]
  return (validPosition $ movek (x,y))

multiMovekLog :: Writer [String] [Kn]
multiMovekLog = do
  a <- movekLog (4,5)
  b <- movekLog (head a)
  return (b)

in3steps :: Kn -> Kn -> Bool
in3steps start end = end `elem` t3 start

-- more generic function
inNsteps :: Int -> Kn -> Kn -> Bool
inNsteps n start end = end `elem` tn n start
