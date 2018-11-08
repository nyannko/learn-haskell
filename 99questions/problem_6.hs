-- (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

-- Example in Haskell:

-- *Main> isPalindrome [1,2,3]
-- False
-- *Main> isPalindrome "madamimadam"
-- True
-- *Main> isPalindrome [1,2,4,8,16,8,4,2,1]
-- True

-- Declaration must be at the beginning of a module.
import Control.Monad
import Control.Applicative
import Control.Arrow

-- 1. recursion for removing head and tail
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = (isPalindrome $ init xs) && (last xs) == (x)
-- alternative 
-- isPalindrome xs = (head xs == last xs) && (isPalindrome $ init $ tail xs)
-- isPalindrome xs = (head xs == last xs) && (isPalindrome $ tail $ init xs)

-- Eq is used for types that support equality testing.
-- If there's an Eq class constraint for a type variable in a function,
-- it uses `==` or `/=` somewhere inside the defination.

-- python impl
-- def p(l):
--     if l == [] or len(l) == 1:
--         return True
--     else:
--         return p(l[1:-1]) and (l[0] == l[-1])

-- 2. intuitive way by using reverse
isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' xs =  xs == reverse xs

isPalindrome'' :: (Eq a) => [a] -> Bool
isPalindrome'' xs = foldl (\acc (a, b) -> if a == b then acc else False) True input
        where 
            input = zip xs (reverse xs)

-- for a palindrome, zip origin with its reverse should return the same tuple,
-- just apply lambda function to compare if elements in each tuple are equal.
-- zip [1,2,1] (reverse [1,2,1])
-- [(1,1),(2,2),(1,1)]
-- foldl (\acc (a, b) -> if a == b then acc else False) True $ zip [1,2,3] $ reverse [1,2,3]

-- 3. ?
isPalindrome''' :: (Eq a) => [a] -> Bool
isPalindrome''' = Control.Monad.liftM2 (==) id reverse

-- 4. ?
isPalindrome'''' :: (Eq a) => [a] -> Bool
isPalindrome'''' = (==) Control.Applicative.<*> reverse

-- does half as many compares. wtf?
palindrome :: (Eq a) => [a] -> Bool
palindrome xs = p [] xs xs
    where p rev (x:xs) (_:_:ys) = p (x:rev) xs ys
          p rev (x:xs) [_] = rev == xs
          p rev xs [] = rev == xs

-- 5. zipWith: ZipWith (+) [1,2,3] [3,2,1]
palindrome' :: (Eq a) => [a] -> Bool
palindrome' xs = and $ zipWith (==) xs (reverse xs)
-- alternative
-- palindrome' xs = foldr (&&) True $ zipWith (==) xs (reverse xs)
-- convert value to True or False

-- 6. Control.Arrows (&&&) fan out operator  ?
palindrome'' :: (Eq a) => [a] -> Bool
palindrome'' xs = (uncurry (==) . (id &&& reverse)) xs

-- point free 
palindrome''' :: (Eq a) => [a] -> Bool
palindrome''' = (uncurry (==) . (id &&& reverse))

-- 7. first half == second half
isPalindrome1 list = take half_len list == reverse ( drop ( half_len + (len `mod` 2)) list) 
        where   
            len = length list
            half_len = len `div` 2

isPalindrome2 list = f_part == reverse s_part
        where
            len = length list
            half_len = len `div` 2
            (f_part, s_part') = splitAt half_len list
            s_part = drop (len `mod` 2) s_part'
