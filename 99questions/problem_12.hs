-- (**) Decode a run-length encoded list.

-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

-- Example in Haskell:

-- P12> decodeModified 
--        [Multiple 4 'a',Single 'b',Multiple 2 'c',
--         Multiple 2 'a',Single 'd',Multiple 4 'e']

data ListItem a = Single a | Multiple Int a deriving (Show)

decodeModified2 :: [ListItem a] -> [a]
decodeModified2 = concatMap decodeHelper2
    where
      decodeHelper2 (Single x)     = [x]
      decodeHelper2 (Multiple n x) = replicate n x

toTuple :: ListItem a -> (Int, a)
toTuple (Single x)     = (1, x)
toTuple (Multiple n x) = (n, x)

decodeModified'' :: [ListItem a] -> [a]
decodeModified'' = concatMap (uncurry replicate . toTuple)


decodeModified :: [ListItem a]-> [a]
decodeModified = foldl (\x y -> x ++ decodeHelper y) []
    where
        decodeHelper :: ListItem a -> [a]
        decodeHelper (Single x)     = [x]
        decodeHelper (Multiple n x) = replicate n x

decodeModified1 :: [ListItem a] -> [a]
decodeModified1 = foldl (\acc e -> case e of Single x -> acc ++ [x]; Multiple n x -> acc ++ replicate n x) []

decode :: Eq a => [(Int,a)] -> [a]
decode xs = foldr f [] xs
  where
    f (1, x) r = x : r
    f (k, x) r = x : f (k-1, x) r

-- decode :: Eq a => [(Int, a)] -> [a]
-- decode xs = build (\c n ->
--     let 
--         f (1, x) r = x `c` r 
--         f (k, x) r = x `c` f (k-1, x) r
--     in 
--         foldr f n xs )