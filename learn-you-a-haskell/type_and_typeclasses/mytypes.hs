data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

moved :: Shape -> Float -> Float -> Shape
moved (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
moved (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

zeroCircle :: Float -> Shape
zeroCircle r = Circle (Point 0 0) r

zeroRectangle :: Float -> Float -> Shape
zeroRectangle width height = Rectangle (Point 0 0) (Point width height)
