import qualified Data.Map as Map

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
-- data Person' = Person' { firstName :: String
--                      , lastName :: String
--                      , age :: Int
--                      , height :: Float
--                      , phoneNumber :: String
--                      , flavor :: String
--  } deriving (Show)
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

data Vector a = Vector a a a deriving (Show)
vplus :: (Num t) => Vector t -> Vector t -> Vector t 
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)
vecMult :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vecMult` (Vector l m n) = Vector (i * l) (j * m) (k * n)
scalarMult :: (Num t) => Vector t -> Vector t -> t 
(Vector i j k) `scalarMult` (Vector l m n) = i * l + j * m + k * n 


-- haskell will see if the value constructors match.
-- and then it will check if all the data contained inside matches by testing 
-- each pair of fields with ==
-- data Person = Person {
--     firstName :: String,
--     lastName :: String,
--     age :: Int
-- } deriving (Eq)

-- If a type constructor have fields, their types has to be part of Show or Read
-- if we want to make our type an instance of them.
data Person = Person {
    firstName :: String,
    lastName :: String,
    age :: Int 
} deriving (Eq, Show, Read)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
            deriving (Eq, Ord, Show, Read, Bounded, Enum)
-- show Friday
-- read "Monday" :: Day
-- Saturday == Sunday
-- Tuesday < Wednesday 
-- minBound :: Day
-- succ Monday
-- pred Sunday

-- type synonyms
-- type key word is used to synonym for an already existing type

phoneBook :: [(String, String)]
phoneBook = 
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

type PhoneNumber = String 
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool 
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook
-- inPhoneBook "mei" "123" [("mei", "123")]

type AssocList k v = [(k, v)]
-- AssocList is a type constructor that takes two types and produces concrete type, like AssocList String, Int

-- Maybe : type constructor
-- Maybe 'a': concrete value

-- Just like we can partially apply functions to get new functions, 
-- we can partially apply type parameters and get new type constructors from them.
-- a type that represents a map (from Data.Map) from integers to something
-- type IntMap v = Map Int v
-- type IntMap' = Map Int

-- AssocList doesn't mean that we can do stuff like AssocList [(1,2),(4,5),(7,9)]. 
-- All it means is that we can refer to its type by using different names. 

-- data Either a b = Left a | Right b deriving (Show, Read, Eq, Ord) 
-- :t Main.Right 'a'
-- Main.Right 'a' :: Main.Either a Char

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = 
    case Map.lookup lockerNumber map of 
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker number " ++ show lockerNumber ++ " is already taken"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]
-- lockerLookup 109 lockers

-- recursive data structures
-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-- data List a= Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)

-- 4 `Cons` (5 `Cons` Empty)
-- 4:(5:[])

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)
-- 4 :-: 5 :-: Empty
-- 4 :-: (5 :-: Empty)
-- ???
infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys 
(x :-: xs) .++ ys = x :-: (xs .++ ys)
-- *Main> let a = 3 :-: 4 :-: 5 :-: Empty
-- *Main> let b = 6 :-: 7 :-: Empty
-- *Main> a .++ b
-- 3 :-: (4 :-: (5 :-: (6 :-: (7 :-: Empty))))

-- we can matched on (x :-: xs) because pattern matching works (only) on constructors.

-- binary search tree
-- Sets and maps from Data.Set and Data.Map are implemented using trees, 
-- only instead of normal binary search trees, they use balanced binary search trees.
-- how?

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- The singleton function is just a shortcut for making a node that has something and then two empty sub-trees.
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree 

treeInsert :: (Ord a) => a -> Tree a -> Tree a 
treeInsert x EmptyTree = singleton x 
treeInsert x (Node a left right) 
            | x == a = Node x left right 
            | x < a = Node a (treeInsert x left) right 
            | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool 
treeElem x EmptyTree = False 
treeElem x (Node a left right) 
            | x == a = True 
            | x < a = treeElem x left 
            | x > a = treeElem x right
-- let nums = [8,6,4,1,7,3,5]
-- let numsTree = foldr treeInsert EmptyTree nums
-- Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))
--  8 `treeElem` numsTree
-- True


-- Typeclass typeclasses are like interfaces. A typeclass defines some behavior 
-- (like comparing for equality, comparing for ordering, enumeration) and then types that can behave in that way are made instances of that typeclass.



