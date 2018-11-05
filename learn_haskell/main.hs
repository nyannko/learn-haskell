-- list comprehension
-- [ x | x <- [50..100], x `mod` 7 == 3]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

replaceBy2 xs = [2 | x <- xs]
-- replace by 1 and sum
len' xs = sum [1 | _ <- xs]

xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
loopcomprehension xxs = [[ x | x <- xs, even x ] | xs <- xxs] 
-- loopcomprehension xxs


-- tuple
triangles = [(x, y, z) | x <- [1..10], y <- [1..10], z <- [1..10], x^2 + y^2 == z^2, x + y + z == 24]

-- type: Int, Integer, Float, Double, Bool, Char
factorial :: Integer -> Integer
factorial x = product [1..x]

circumference :: Float -> Float
circumference r = 2 * pi * r

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

removeLowercase :: [Char] -> [Char]
removeLowercase st = [x | x <- st, x `elem` ['A'..'Z']]

fizzBuzz :: Int -> String 
fizzBuzz x
    | x < 3 = show x
    | otherwise = show 0

primes :: [Integer]
primes = sieve [2..100]
        where
            sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

-- pattern matching
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "one"
sayMe 2 = "two"
sayMe x = "not between one and two"

factorial' :: (Integral a) => a -> a
factorial' 0 = 1 
factorial' n = n * factorial' (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1,x2) (y1,y2) = (x1 + y1, x2 + y2)

first :: (Num a) => (a, a, a) -> a
first (x, _, _) = x

second :: (Num a) => (a, a, a) -> a
second (_, x, _) = x

-- use parenthese for unpacking multiple variables 
head' :: (Num a) => [a] -> a
head' [] = error "Empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "This list is empty"
tell (x:[]) = "This list has one element " ++ show x
tell (x:y:[]) = "This list has two elements " ++ show x  ++ "and "++ show y
tell (x:y:_) = "This list has multiple elements " ++ show x  ++ "and" ++ show y

length' :: (Num b) => [a] -> b 
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- as
capital :: String -> String
capital "" = "Empty string"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- guards: just like if .. elif .. else in Python
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi 
    | bmi <= 18.5 = "under weight"
    | bmi <= 25.0 = "normal"
    | bmi <= 30.0 = "fat"
    | otherwise = "whale"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' w h
    | w / h ^ 2 <= 18.5 = "under weight"
    | w / h ^ 2 <= 25.0 = "normal"
    | w / h ^ 2 <= 30.0 = "fat"
    | otherwise = "whale"

max' :: (Ord a) => a -> a -> a 
max' a b 
    | a > b = a 
    | otherwise = b

compare' :: (Ord a) => a -> a -> Ordering
compare' a b
    | a > b = GT
    | a == b = EQ
    | otherwise = LT

-- where
bmiTell'' :: (RealFloat a) => a -> a -> String
bmiTell'' w h
    | bmi <= 18.5 = "under weight"
    | bmi <= 25.0 = "normal"
    | bmi <= 30.0 = "fat"
    | otherwise = "whale"
    where bmi = w / h ^ 2

initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname

-- let
-- let bindings in expressions: let a = 9 in a + 1 => 10
-- [let square = x* x in (square 5, square 3, square 10)]
cylinder :: (RealFloat a) => a -> a -> a 
cylinder r h = 
    let sideArea = 2 * pi * r * h 
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

clacBmis :: (RealFloat a) => [(a, a)] -> [a]
clacBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25]

-- case expression
-- case expression of pattern -> result
--                    pattern -> result
head'' :: [a] -> a 
head'' xs = case xs of [] -> error "Empty list"
                       (x:_) ->x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
            where what [] = "empty."
                  what [x] = "a singleton list."
                  what xs = "a longer list."

-- recursion
-- 1.boundary(base case)
-- 2.a set of rules
maximum' :: (Ord a) => [a] -> a 
maximum' [] = error "Empty list"
maximum' [x] = x
maximum' (x:xs) 
        | x > maxTail = x
        | otherwise = maxTail
        where maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a 
maximum'' [] = error "Empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

replicate' :: (Ord i, Num i) => i -> a -> [a]
replicate' n x
        | n <= 0 = []
        | otherwise = x : replicate' (n - 1) x

take' ::(Ord i, Num i) => i -> [a] -> [a]
take' n _
      | n <= 0 = [] -- take -1 [1,2,3] why does not work?
take' _ []  = [] -- take 10 []
take' n (x:xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' n = n:repeat' n

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' n [] = False
elem' n (x:xs)  
    | n == x = True
    | otherwise = elem' n xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let small = quicksort [a | a <- xs, a <= x]
        big = quicksort [a | a <- xs, a > x]
    in small ++ [x] ++ big
