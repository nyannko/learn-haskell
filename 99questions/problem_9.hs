-- (**) Pack consecutive duplicates of list elements into sublists. 
-- If a list contains repeated elements they should be placed in separate sublists.

-- Example:

-- * (pack '(a a a a b c c a a d e e e e))
-- ((A A A A) (B) (C C) (A A) (D) (E E E E))
-- Example in Haskell:

-- *Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
--              'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]

-- 1. use built-in function Data.List groupBy
import Data.List 
pack xs = groupBy (==) xs

-- 2. span
pack1 (x:xs) = let (first, rest) = span (==x) xs 
                in (x:first) : pack1 rest
pack1 [] = []
-- how to convert to where clause?

-- 3. filter
grp [] = []
grp (x:xs) = (x:(filter (==x) xs)):(grp $ filter (/=x) xs)

--4. takeWhile and dropWhile
pack2 :: (Eq a) => [a] -> [[a]]
pack2 [] = []
pack2 (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

--5. foldr
pack3 :: (Eq a) => [a] -> [[a]]
pack3 = foldr func [] 
    where func x []  = [[x]]
          func x (y:xs) = 
            if x == (head y) then ((x:y):xs) else ([x]:y:xs)

-- 6. A simple solution?
pack':: (Eq a) => [a] -> [[a]]
pack' [] = []
pack' [x] = [[x]]
-- pack' (x:xs) = if x == head(xs) then (x:head(xs)) else pack' (tail xs) ???
pack' (x:xs) = if x `elem` (head (pack xs))
                then (x:(head (pack xs))) : (tail (pack xs))
                else [x] :(pack xs)

-- 7.
pack4 [] = []
pack4 [x] = [[x]]
pack4 (x:xs)
    | x == head  h_p_xs = (x:h_p_xs):t_p_hs
    | otherwise         = [x]:p_xs
    where p_xs@(h_p_xs:t_p_hs) = pack4 xs

-- 8.
myPack [] = []
myPack (y:ys) = impl ys [[y]]
    where
        impl [] packed = packed
        impl (x:xs) packed 
            | x == (head (last packed)) = impl xs ((init packed) ++ [x:(last packed)])
            | otherwise      = impl xs (packed ++ [[x]])
 
-- 9.
pack5 [] = []
pack5 (y:ys) = reverse $ impl ys [[y]]
        where 
            impl [] packed = packed 
            impl (x:xs) p@(z:zs)
                | x == (head z)  = impl xs ((x:z):zs)
                | otherwise = impl xs ([x]:p)
