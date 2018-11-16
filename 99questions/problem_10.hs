-- (*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

-- Example:

-- * (encode '(a a a a b c c a a d e e e e))
-- ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
-- Example in Haskell:

-- encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
module Problem_10
(
    encode 
) where
import Data.List
import Problem_9
import Control.Arrow

-- 1. list comprehension
encode xs = [(length x, head x) | x <- group xs]

-- 2. map
encode1 xs = map (\x -> (length x, head x)) (group xs)

-- pointer free
encode2 :: (Eq a) => [a] -> [(Int, a)]
encode2 = map (\x -> (length x, head x)) . group

-- 3. foldr
encode3 xs = (enc . pack) xs
    where enc = foldr (\x acc -> (length x, head x) : acc) []

-- 4. takeWhile and dropWhile
encode4 [] = []
encode4 (x:xs) = (length $ x : takeWhile (==x) xs, x) : encode4 (dropWhile (==x) xs)

-- 5. zip
encode5 :: (Eq a) => [a] -> [(Int, a)]
encode5 xs = zip (map length l) h where
    l = group xs
    h = head l 

-- 6. ??
encode6 :: (Eq a) => [a] -> [(Int, a)]
encode6 xs = foldr f final xs Nothing 
    where 
        f x r (Just a@(i, q)) | x == q = r (Just (i+1, q))
                              | otherwise = a : r (Just (1, x))
        f x r Nothing = r (Just (1, x))

        final (Just a@(i, q)) = [a]
        final Nothing = []

-- 7. ???
-- encode7 :: (Eq a) => [a] -> [(Int, a)]
-- encode7 xs = build (\c n ->
--     let
--       f x r (Just a@(i,q)) | x == q = r (Just (i+1,q))
--                            | otherwise = a `c` r (Just (1, x))
--       f x r Nothing = r (Just (1, x))
   
--       final (Just a@(i,q)) = a `c` n
--       final Nothing = n
   
--     in
--       foldr f final xs Nothing)

-- 8. &&&
encode8 :: (Eq a) => [a] -> [(Int, a)]
encode8 xs = map (length &&& head) $ group xs

-- 9. 
encode9 :: (Eq a) => [a] -> [(Int, a)]
encode9 = map ((,) <$> length <*> head) . pack