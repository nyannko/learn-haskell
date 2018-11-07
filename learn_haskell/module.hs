import Data.List
import Data.List (nub, sort)
import Data.List hiding (nub)
-- (Data.Map.filter)
import qualified Data.Map
-- (M.filter)
import qualified Data.Map as M 

-- unload the list
-- :m -Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length.nub

-- 1. interperse
-- intersperse '.' "MONKEY"  

-- 2. intercalate 
-- intercalate "." ["www", "google", "com"]

-- 3. transpose
-- transpose [[1,2,3],[4,5,6],[7,8,9]]
-- like map(list, zip(*a)) in python

-- sum each row of the transpose of the matrix
-- Data.List.map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]
-- [18,8,6,17]


-- 4. list to string
-- concat ["foo","bar","car"]  
-- flat list once
-- concat [[3,4,5],[2,3,4],[2,1,1]]  
-- concat $ concat [[[3],[4]]]

-- 5. and, or, any, all
-- and $ map(==4) [1,2,3,4]
-- or $ map (==4) [2,3,4,5,6,1]  
-- any (==4) [1,2,3,4] 
-- any (`elem` ['A'..'Z']) "HEYGUYSwhatsup"  
-- all (>4) [5,6,7]

-- 6. iterate
-- take 10 $ iterate (*2) 1
-- take 3 $ iterate (++ "haha") "haha"

-- 7. splitAt
-- splitAt 3 "hello" ("hel","lo")
-- change the order 
-- let (a, b) = splitAt 3 "foobar" in b++a

-- 8. takeWhile, dropWhile
-- take the element from the list until one does not satisfy the predicate
-- sum $ takeWhile (<10000) $ map (^3) [1..]
-- dropWhile (/=' ') "This is a sentence" 
-- dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1]  
-- let stock [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]  
-- head (dropWhile (\(val,y,m,d) -> val < 1000) stock)  

-- 9. span (False split), break (True split)
-- span (/=4) [1,2,3,4,5,6,7]  
-- break (==4) [1,2,3,4,5,6,7]  

-- 10. sort
-- sort [8,5,3,2,1,6,4,2]  

-- 11. group 
-- group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]  

-- the same as python counter
-- map (\l@(x:xs) -> (x, length l)).group. sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]

-- 12. inits, tails
-- let w = "abcde" in zip (inits w) (tails w)
-- [("","abcde"),("a","bcde"),("ab","cde"),("abc","de"),("abcd","e"),("abcde","")]

-- 13. search substring
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack = 
    let nlen = length needle
    in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

-- "cat" `isInfixOf` "im a cat burglar" 
-- "hey" `isPrefixOf` "hey there!"  
-- "there!" `isSuffixOf` "oh hey there!"

-- 14. find 
-- find return just something or nothing
-- find (>3) [1,2,3,4], Just 4
-- find (>3) [1,2,3] Nothing

-- 15. elemIndex, elemIndices, findIndex, findIndices
-- 4 `elemIndex` [1,2,3,4,5,6]  Just 3
-- 10 `elemIndex` [1,2,3,4,5,6]  Nothing
-- ' ' `elemIndices` "Where are the spaces?" 
-- findIndex (==4) [5,3,2,1,6,4]  
-- findIndices (`elem` ['A'..'Z']) "Where Are The Caps?"  

-- zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3] 

-- 16. lines "first line\nsecond line\nthird line"  
-- unlines ["first line","second line","third line"]  

-- 17. nub, remove the duplicated elements

-- 18. words, unwords, list -> string, string -> list
-- words "hey these are the words in this sentence"  list(string)
-- unwords ["hey","there","mate"]  l.split()

-- 19. delete

-- 20. \\ list difference function, union, intersect, insert

-- list intersect [9,4,9,8,4] `intersect` [4,9,10,5]
-- returns [9,4,9,4]
-- list intersect [4,9,10,5] `intersect` [9,4,9,8,4]
-- returns [4,9]
-- same operations:
-- https://leetcode.com/problems/intersection-of-two-arrays-ii/description/

-- insert 4 [1,2,3,5,6,7]  

-- 21. alternatives for length, take, drop, splitAt, !!, replicate in Data.List
-- genericLength,genericTake,genericDrop,genericSplitAt,genericIndex, genericReplicate
-- alternatives for nub, delete, union, intsect, group (==)
-- nubBy,deleteBy,unionBy,intersectBy, groupBy (followed by functions)
-- let values = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3,-2.4,-14.5, 2.9, 2.3]  
-- groupBy (\x y -> (x > 0) == (y > 0)) values
-- :m + Data.Function
-- groupBy ((==) `on` (> 0)) values