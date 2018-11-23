-- (**) Replicate the elements of a list a given number of times.
-- > repli "abc" 3
-- "aaabbbccc"

repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

repli' :: [a] -> Int -> [a]
repli' = flip $ concatMap . replicate

repli1 :: [a] -> Int -> [a] 
repli1 xs n = concatMap (take n . repeat) xs

-- list monad
repli2 :: [a] -> Int -> [a] 
repli2 xs n = xs >>= replicate n 

-- without replicate
repli3 :: [a] -> Int -> [a]
repli3 xs n = foldl (\acc e -> acc ++ repli4 e n) [] xs
    where
      repli4 _ 0 = []
      repli4 x n = x : repli4 x (n-1)

repli5 :: [a] -> Int -> [a] 
repli5 [] _ = [] 
repli5 (x:xs) n = foldr (const (x:)) (repli5 xs n) [1..n]