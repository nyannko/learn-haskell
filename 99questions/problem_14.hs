-- (*) Duplicate the elements of a list.
-- > dupli [1, 2, 3]
-- [1,1,2,2,3,3]

-- 1. concatMap
dupli :: (Eq a) => [a] -> [a] 
dupli = concatMap (\a -> [a, a]) 
-- create a list from a list generating function by application of this function on all elements 
-- in a list passed as the second argument

dupli' :: (Eq a) => [a] -> [a] 
dupli' = concatMap (replicate 2)

-- 2. list comprehension
dup list = concat [[x, x] | x <- list]

-- 3.
dup1 [] = []
dup1 (x:xs) = x:x:dup xs

-- 4. list instance of applicative ?
-- dup2 = (<**> [id,id])

-- 5. foldl and foldr
dup3 :: (Eq a) => [a] -> [a] 
dup3 = foldl (\acc x -> acc ++ [x, x]) []
dup4 :: (Eq a) => [a] -> [a] 
dup4 = foldr (\ x xs -> x : x : xs) []

-- 6. list monad
dup5 xs = xs >>= (\x -> [x,x])


