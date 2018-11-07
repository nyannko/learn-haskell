-- find the K'th element of a list
-- Prelude> elementAt [1,2,3] 2
-- 2
-- Prelude> elementAt "haskell" 5
-- 'e'

-- 1 the definition of !! (zero-indexed)
elementAt :: [a] -> Int -> a 
elementAt (x:_) 0 = x
elementAt (_:xs) k = elementAt xs (k - 1)

-- 2
elementAt' :: [a] -> Int -> a
elementAt' [] _ =  error "Index too large"
elementAt' (x:_) 1 = x
elementAt' (x:xs) k 
        | k < 1 = error "Index too large"
        | otherwise = elementAt' xs (k - 1)

-- 3
elementAt'' :: [a] -> Int -> a 
elementAt'' (x:_) 1 = x
elementAt'' (_:xs) k = elementAt'' xs (k - 1)
elementAt'' _ _ = error "Index too large" -- ??

-- prelude functions
-- 4
elementAt''' xs n
        | length xs < n = error "Index too large"
        | otherwise = fst . last $ zip xs [1..n]

-- 5 ???
elementAt'''' xs n = head $ foldr ($) xs 
                          $ replicate (n - 1) tail

elementAt''''' xs n
        | length xs < n = error "Index too large"
        | otherwise = last $ take n xs
        -- | otherwise = head . reverse $ take n xs
        -- | otherwise = head $ drop (n - 1) xs

-- point-free style, function composition 
elementAt_w'pf = (last .) . take . (+ 1)
elementAt_w'pf' = flip $ (last .) . take . (+1)
