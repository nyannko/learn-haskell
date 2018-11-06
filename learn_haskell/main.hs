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
      | n <= 0 = [] 
-- take -1 [1,2,3] why does not work?
take' _ []  = [] 
-- take 10 []
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

-- higher order function
-- x is applied to multiThree, that creates a function that takes one param and 
-- returns a function.
-- putting a space between two things is function application
-- multiThree :: (Num a) => a -> (a -> (a -> a))
multiThree :: (Num a) => a -> a -> a -> a 
multiThree x y z = x * y * z

compareWithHundread :: (Num a, Ord a) => a -> Ordering 
-- compareWithHundread x = compare 100 x
compareWithHundread = compare 100

-- infix function
divideByTen :: (Floating a) => a -> a 
divideByTen = (/10)

isUpperAlpha :: Char -> Bool 
isUpperAlpha  = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
-- applyTwice (min 10) 9
-- applyTwice (3:) [1]

zipwith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipwith' _ [] _ = []
zipwith' _ _ [] = []
zipwith' f (x:xs) (y:ys) = f x y : zipwith' f xs ys 
-- zipwith' (+) [1,2,3] [4,5,6]

-- evaluates the function flipping the order of arguments
filp' :: (a -> b -> c) -> (b -> a -> c)
filp' f x y = f y x
-- flip' compare 1 2
-- flip' (+) 3 10
-- flip' zip [1,2,3,4,5] "hello"  

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs
-- map' fst [(1,2), (3,4)]
-- map' (+3) [1,2,3]

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
        | p x = x : filter' p xs
        | otherwise = filter' p xs
-- filter' (<2) [1,2,3]
-- filter' even [1,2,3]

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = 
        let small = quicksort' (filter (<=x) xs)
            big = quicksort' (filter (>x) xs)
        in small ++ [x] ++ big

largestDivisible :: (Integral a) => a 
largestDivisible = head $ filter p [10000, 9999..]
        where p x = x `mod` 233 == 0

-- sum (takeWhile (<10000) (filter odd (map (^2) [1..])))  
-- sum (takeWhile (<10000) [m | m <- [n^2 | n <- [1..]], odd m])  

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n 
        | even n = n:chain(n `div` 2)
        | odd n = n:chain(n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong(map chain [1..100]))
        where isLong xs = length xs > 15

-- let listOfFuns = map (*) [0..]
-- (listOfFuns !! 4) 5

numLongChains' :: Int  
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- lambda \x
-- map (+3) [1,2,3]
-- map (\x -> x + 3) [1,2,3]
-- map (\(a, b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]
-- zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]   

addThree' :: (Num a) => a -> a -> a -> a 
addThree' = \x -> \y -> \z -> x + y + z

flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f = \x y -> f y x

-- foldl (+) 0 [1..10]
sum'' :: (Num a) => [a] -> a 
sum'' xs = foldl (\acc x -> acc + x) 0 xs

sum''' :: (Num a) => [a] -> a
sum''' xs = foldl (+) 0 xs

-- foldl --> elem
elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys = foldl (\acc x -> if x == y then True else acc)  False ys

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

-- foldl1 foldr1, no explicit starting value 
sum'''' :: (Num a) => [a] -> a
sum'''' = foldl1 (+) 

product' :: (Num a) => [a] -> a
product' = foldl1 (*)

maximum''' :: (Ord a) => [a] -> a 
maximum''' = foldr1 (\x acc -> if x > acc then x else acc)

head''' :: [a] -> a
head''' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldr1 (\_ x -> x)

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

-- *Main> scanl (+) 0 [1,2,3]
-- [0,1,3,6]
-- *Main> scanl1 (+) [1,2,3]
-- [1,3,6]
-- scanl (flip (:)) [] [3,2,1]  
-- [[],[3],[2,3],[1,2,3]]

-- $
-- sum $ map sqrt [1..131]
-- f (g (z x)) <> f $ g $ z x
-- map ($ 3) [(4+),(10*),(^2),sqrt] 


-- function composition

-- example 1
-- map (\x -> negate (abs(x))) [1,2,3]
-- map (\x -> negate $ abs(x)) [1,2,3]
-- map (negate . abs) [1,2,3]

-- map (\xs -> negate (sum (tail xs))) [[1,2,3], [4,5,6]]
-- map (\xs -> negate $ sum $ tail xs) [[1,2,3], [4,5,6]]
-- map (\x -> negate.sum.tail) [[1,2,3], [4,5,6]]

-- point free style
-- fn x = ceiling (negate (tan (cos (max 50 x))))
-- fn = ceiling . negate . tan . cos . max 50

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]
-- oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSquareSum' :: Integer
oddSquareSum' = 
    let oddSquares = filter odd (map (^2) [1..])
        belowList = takeWhile (<10000) oddSquares
    in sum belowList