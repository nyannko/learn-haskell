-- (**) Run-length encoding of a list (direct solution).

-- Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

-- P13> encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']

data ListItem a = Single a | Multiple Int a deriving (Show)

encodeDirect' :: (Eq a) => [a] -> [(Int, a)] 
encodeDirect' = foldr helper [] 
    where 
        helper x [] = [(1, x)]
        helper x (y@(a, b):ys) 
            | x == b = (1+a, x):ys
            | otherwise = (1, x):y:ys 

encodeList :: (Eq a) => [a] -> [ListItem a]
encodeList = map encodeHelper . encodeDirect'
        where 
            encodeHelper (1, x) = Single x 
            encodeHelper (n, x) = Multiple n x 

-- ???
encode' :: (Eq a) => [a] -> [ListItem a] 
encode' [] = [] 
encode' (x:xs) = encode1 1 x xs 
encode1 n y [] = [encode2 n y] 
encode1 n y  (x:xs) | y == x = encode1 (n+1) y xs 
                    | otherwise = encode2 n y : (encode1 1 x xs)
encode2 1 y = Single y 
encode2 n y = Multiple n y

-- 3. ???
encode3 :: (Eq a) => [a] -> [ListItem a] 
encode3 [] = [] 
encode3 (x:xs) 
    | count == 1 = (Single x) : (encode3 xs) 
    | otherwise = (Multiple count x) : (encode3 rest) 
    where 
        (matched, rest) = span (==x) xs 
        count = 1 + (length matched)

-- 4. 
encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect []=[]
encodeDirect (x:xs) = encodeDirectHelper 1 x xs
    
encodeDirectHelper :: Eq a => Int->a->[a]->[ListItem a]
encodeDirectHelper n x [] = [encodeHelper(n,x)]
encodeDirectHelper n x xs = if x==(head xs)
            then encodeDirectHelper (n+1) x (tail xs)
            else [encodeHelper(n,x)] ++ (encodeDirect xs)
    
encodeHelper :: (Int, a)-> ListItem a
encodeHelper (1,x)= Single x
encodeHelper (n,x)= Multiple n x