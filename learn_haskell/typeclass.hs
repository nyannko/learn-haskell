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
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface' :: Shape -> Float
surface' (Circle _ r) = pi * r ^ 2
surface' (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- surface' (Rectangle' (Point 0 0) (Point 100 100)) --> 10000.0

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))
-- *Main> nudge (Circle (Point 10 10) 20) 1 1
-- Circle (Point 11.0 11.0) 20.0

-- auxlilliary function
baseCircle :: Float -> Shape 
baseCircle r = Circle (Point 0 0) r 

baseRect :: Float -> Float -> Shape 
baseRect width height = Rectangle (Point 0 0) (Point width height)

-- Record syntax
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
 } deriving (Show)
-- firstName guy
-- :t firstName (function)
-- data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)
-- Car {company="Ford", model="Mustang", year=1967}

data Car a b c = Car { company :: a
                       , model :: b
                       , year :: c
                        } deriving (Show)

-- tellCar :: Car -> String
-- tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

tellCar :: (Show a) => Car String String a -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y