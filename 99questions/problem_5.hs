-- reverse a list
-- Prelude> myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- Prelude> myReverse [1,2,3,4]
-- [4,3,2,1]
-- built-in: reverse


-- 1. recursion
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- str impl in python
-- >>> def reverse(l):
-- ...     if not l:
-- ...         return ""
-- ...     else:
-- ...         return reverse(l[1:]) + l[:1]

-- 2. ?..kind of speachless for now
myReverse' :: [a] -> [a]
myReverse' list = reverse' list []
        where 
            reverse' [] reversed = reversed
            reverse' (x:xs) reversed = reverse' xs (x:reversed)


-- 3. built-in reverse:
-- basic idea is to apply flip to each element from left to right
reverseOrigin :: [a] -> [a]
reverseOrigin = foldl (flip (:)) []
-- foldl (flip (:)) [] [1,2,3] 
-- flip basic usage: flip (:) [1] 2 --> 2:[1] --> [2,1]
-- left elem1: flip (:) [] 1    --> [1] , and this new result became the new input of foldl
-- left elem2: flip (:) [1] 2   --> [2,1]
-- left elem3: flip (:) [2,1] 3 --> [3,2,1]


-- 4. fold
-- basic fold usage: 
-- foldl (+) 0 [1,2,3]
-- foldl1 (+) [1,2,3]
myReverseFold1 :: [a] -> [a]
myReverseFold1 = foldl (\acc x -> x : acc) []

-- myReverseFold2 :: [a] -> [a]
-- myReverseFold2 = foldl1 (\acc x -> x : acc)

-- 5. foldr ??
myReverseFold2 :: [a] -> [a]
myReverseFold2 xs = foldr (\x fId empty -> fId (x : empty)) id xs []

-- thinking:
-- In python if I want to reverse a list or string, I just search in stackoverflow and get the answer.
-- Basically the answer is to use 
-- for string:
-- no in place except constructing a new string, because 'str' object does not support item assignment
-- not in place: reverse('string')(iterator), 'string'[::-1]
-- for list:
-- in place: [1,2,3].reverse()
-- not in place: reversed([1,2,3])(iterator), [1,2,3][::-1]
-- but I never think about what's the difference and why that works
-- I looked into the source code in https://hg.python.org/cpython/file/96f08a22f562/Objects/listobject.c#l944
-- it use two pointers to swap values.
-- the solution in haskell is basically use the recursion, map, filter, fold, zip.