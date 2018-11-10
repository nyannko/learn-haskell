-- (**) Eliminate consecutive duplicates of list elements.

-- If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
-- > compress "aaaabccaadeeee"
-- "abcade"

-- compress :: (Eq a) => [a] -> [a]
-- compress [] = []
-- compress [x] = [x]
-- compress [x, x] = [x]
-- compress (()) = compress xs' + ..
import Data.List

compress :: (Eq a) => [a] -> [a]
compress = map head . group 

compress' (x:ys@(y:_))
    | x == y = compress' ys
    | otherwise = x : compress' ys 
compress' ys = ys

-- mine sulotion
compress1 :: (Eq a) => [a] -> [a]
compress1 [] = []
compress1 [x] = [x] -- Non-exhaustive patterns in function compress1 if do not add this line
compress1 (x:ys@(y:_)) = 
    if x == y then compress1 ys else x: compress1 ys

-- wtf
compress2 xs = foldr f (const []) xs Nothing
  where
    f x r a@(Just q) | x == q = r a
    f x r _ = x : r (Just x)

-- stack
compress3 :: (Eq a) => [a] -> [a]
compress3 = foldr skipDups []
    where skipDups x [] = [x]
          skipDups x acc
                | x == head acc = acc
                | otherwise = x : acc

-- similar
compress4 :: (Eq a) => [a] -> [a]
compress4 list = compress_acc list []
          where compress_acc [] acc = acc
                compress_acc [x] acc = (acc ++ [x])
                compress_acc (x:xs) acc
                  | x == (head xs)  = compress_acc xs acc
                  | otherwise       = compress_acc xs (acc ++ [x])

compress5 []     = []
compress5 (x:xs) = x : (compress5 $ dropWhile (== x) xs)
       
-- foldr
compress6 :: (Eq a) => [a] -> [a]
compress6 x = foldr (\a b -> if a == (head b) then b else a:b) [last x] x

compress7 :: (Eq a) => [a] -> [a]
compress7 x = foldl (\a b -> if (last a) == b then a else a ++ [b]) [head x] x
compress7' x = reverse $ foldl (\a b -> if (head a) == b then a else b:a) [head x] x

-- ....???
-- {-# INLINE compress #-}
-- compress8 :: Eq a => [a] -> [a]
-- compress8 xs = build (\c n ->
--   let
--     f x r a@(Just q) | x == q = r a
--     f x r _ = x `c` r (Just x)
--   in
--     foldr f (const n) xs Nothing)
