-- bool
-- data Bool = False | True

-- Int
-- data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647

-- new data type = value constructors(functions)
-- deriving is to make Shape type part of the Show type class
data Shape' = Circle' Float Float Float | Rectangle' Float Float Float Float deriving (Show)

surface :: Shape' -> Float
surface (Circle' _ _ r) = pi * r ^ 2
surface (Rectangle' x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- a list of concentric circles with different radii
-- map (Circle 10 20) [4,5,6,6]  

data Point = Point Float Float deriving (Show)
data Shape = Circle Float Float | Rectangle Point Point deriving (Show)

surface' :: Shape -> Float
surface' (Circle _ _ r) = pi * r ^ 2
surface' (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- surface' (Rectangle' (Point 0 0) (Point 100 100)) --> 10000.0

-- nudge :: Shape -> Float -> Float -> Shape
-- nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
-- nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))