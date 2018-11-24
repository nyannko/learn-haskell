-- (**) Drop every N'th element from a list.
-- *Main> dropEvery "abcdefghik" 3
-- "abdeghk"

-- for example: take 2 drop 3
dropEvery :: [a] -> Int -> [a] 
dropEvery [] _ = [] 
dropEvery list count = take (count - 1) list ++ dropEvery (drop count list) count

dropEvery1 :: [a] -> Int -> [a] 
dropEvery1 [] _ = [] 
dropEvery1 (x:xs) n = dropEvery' (x:xs) n 1 where 
    dropEvery' (x:xs) n i = (if (n `divides` i) then 
        [] else 
        [x])
        ++ (dropEvery' xs n (i+1))
    dropEvery' [] _ _ = [] 
    divides x y = y `mod` x == 0 

dropEvery2 :: [a] -> Int -> [a] 
dropEvery2 list count = helper list count count 
        where helper [] _ _ = [] 
              helper (x:xs) count 1 = helper xs count count 
              helper (x:xs) count n = x : (helper xs count (n - 1)) 

dropEvery3 :: [a] -> Int -> [a] 
dropEvery3 xs n = helper xs n 
        where helper [] _ = [] 
              helper (x:xs) 1 = helper xs n 
              helper (x:xs) k = x : helper xs (k-1)

dropEvery4 :: [a] -> Int -> [a] 
dropEvery4 xs n = helper xs 1
        where helper :: [a] -> Int -> [a] 
              helper [] _ = [] 
              helper (x:xs) i 
                | i == n = helper xs 1 
                | i /= n = x:helper xs (i+1)

dropEvery5 :: [a] -> Int -> [a] 
dropEvery5 xs n 
    | length xs < n = xs 
    | otherwise = take (n-1) xs ++ dropEvery5 (drop n xs) n

dropEvery6 = flip $ \n -> map snd . filter ((n/=) .fst) .zip (cycle [1..n]) 

dropEvery7 :: [a] -> Int -> [a] 
dropEvery7 xs n = [i|(i, c) <- (zip xs [1,2..]), (mod c n) /= 0]

dropEvery8 :: [a] -> Int -> [a]
dropEvery8 [] _ = []
dropEvery8 xs n = concat (split n xs)
 where
  split _ [] = []
  split n xs = fst splitted : split n ((safetail . snd) splitted)
   where
    splitted = splitAt (n-1) xs
    safetail xs | null xs = []
                | otherwise = tail xs 

dropEvery9 xs n = map fst $ filter (\(x, i) -> i `mod` n /=0) $ zip xs [1..]
dropEvery10 xs n = map fst $ filter ((n/=) . snd) $ zip xs (cycle [1..n])

dropEvery11 :: Int -> [a] -> [a]
dropEvery11 n xs = snd $ foldl (\acc e -> if fst acc > 1 then (fst acc - 1, snd acc ++ [e]) else (n, snd acc)) (n, []) xs

dropEvery12 :: [a] -> Int -> [a]
dropEvery12 xs n = fst $ foldr (\x (xs, i) -> (if mod i n == 0 then xs else x:xs, i - 1)) ([], length xs) xs

dropEvery13 :: Int -> [a] -> [a]
dropEvery13 k = snd . unzip . filter (\(i, _) -> i `mod` k /= 0) . zip [1..]

dropEvery14 :: [a] -> Int -> [a]
dropEvery14 lst n = snd $ foldl helper (1, []) lst
    where helper (i,acc) x = if n == i
                             then (1,acc)
                             else (i+1,acc++[x])