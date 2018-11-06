-- find the number of elements in the list
-- myLength

-- 1. simple recursion
-- be careful with signature
myLength' :: (Num b) => [a] -> b
myLength' [] = 0
myLength' (x:xs) = 1 + myLength' xs

-- length function in Python
-- >>> def length(l):
-- ...     if l == []:
-- ...         return 0
-- ...     else:
-- ...         return 1 + length(l[1:])

-- 2. mapping to one
myLength xs = sum [1 | _ <- xs]

-- $
-- myLength'' :: (Num b) => [a] -> b
myLength'' xs = sum $ map (\_ -> 1) xs

-- function composition (without xs in both side of the equation)
-- myLength''' :: (Num b) => [a] -> b
myLength''' = sum . map (\_ -> 1) 

-- 3. accumulator ?
myLengthAccu :: [a] -> Int
myLengthAccu list = myLengthAccu list 0
    where
        myLengthAccu [] n = n
        myLengthAccu (_:xs) n = myLengthAccu xs (n + 1)



-- 4. foldl/foldr: what is n?
myLengthFold1 :: [a] -> Int
myLengthFold1 = foldl (\n _ -> n + 1) 0

myLengthFold2 :: [a] -> Int
myLengthFold2 = foldr (\_ n -> n + 1) 0

myLengthFold3 xs = foldl1 (\n _ -> n + 1) xs
myLengthFold4 xs = foldr1 (\_ n -> n + 1) xs

-- ???
myLengthFold5 xs = foldr (\_ -> (+1)) 0 xs
myLengthFold6 xs = foldr (const (+1)) 0 xs
myLengthFold7 xs = foldl (const . (+1)) 0 xs

-- 5. zip
-- get the index in a tuple
myLengthZip1 xs = snd $ last $ zip xs [1..]
-- point free(.)
myLengthZip2 = snd . last . flip zip [1..]
-- snd $ last $ flip zip [1..] [4,5,6]
myLengthZip3 = fst . last. zip [1..]
-- fst $ last $ zip [1..] [4,5,6]
