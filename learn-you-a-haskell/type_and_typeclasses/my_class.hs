data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Red, Eq)
data TrafficLight = Red | Green | Yellow deriving (Show)

class YesNo a where
  yesno :: a -> Bool
  
instance YesNo Int where
  yesno 0 = False
  yesno _ = True
  
instance YesNo [a] where
  yesno [] = False
  yesno _ = True
  
instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

instance YesNo Bool where
  yesno = id -- id is just a std lib function that takes a parameter and return the same thing
  
instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False
  
instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True
