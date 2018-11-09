-- (**) Flatten a nested list structure.
--  data NestedList a = Elem a | List [NestedList a]
-- *Main> flatten (Elem 5)
-- [5]
-- *Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
-- *Main> flatten (List [])
-- []
-- we have to define a new data type, because lists in Haskell are homogeneous.
-- [1,[2,[3,4],5]] is a type error. Therefore, we must have a way of representing a list
-- that may be nested. 

-- wtf?! no idea

-- concatMap (\x -> [(x,x+2,x/2)]) [1,3,5]
-- concatMap (\x -> [1..x]) [3,1,2] -> [1, 2, 3(x)] ++ [1] ++ [1,2] = [1,2,3,1,1,2]

-- for solution 8
import qualified Data.Foldable as F 

data NestedList a = Elem a | List [NestedList a]

-- 1. concatMap ??
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

-- 2. without comcatMap ?
flatten' :: NestedList a -> [a]
flatten' (Elem a) = [a]
flatten' (List (x:xs)) = flatten' x ++ flatten' (List xs)
flatten' (List []) = []

-- 3. using things that act just like concatMap ???
-- http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#arrow-notation
flatten'' :: NestedList a -> [a]
flatten'' (Elem x) = return x 
flatten'' (List x) = flatten'' =<< x

-- 4. foldMap ??
flatten1 :: NestedList a -> [a]
flatten1 (Elem x) = [x]
flatten1 (List x) = foldMap flatten x

-- 5.???
flatten2 :: NestedList a -> [a]
flatten2 a = flt' a []
    where flt' (Elem x)         xs = x:xs
          flt' (List (x:ls))    xs = flt' x (flt' (List ls) xs)
          flt' (List [])        xs = xs

-- 6. foldr ?
flatten3 :: NestedList a -> [a]
flatten3 (Elem x) = [x]
flatten3 (List xs) = foldr (++) [] $ map flatten3 xs

-- 7. accumulator function ?
flatten4 = reverse . rec []
    where 
        rec acc (List []) = acc
        rec acc (Elem x) = x:acc 
        rec acc (List (x:xs)) = rec (rec acc x) (List xs)

-- 8.???
instance F.Foldable NestedList where
    foldMap f (Elem x) = f x 
    foldMap f (List []) = mempty
    foldMap f (List (x:xs)) = F.foldMap f x `mappend` F.foldMap f (List xs) 

-- 9. give up
flatten5 :: NestedList a -> [a]
flatten5 = F.foldMap (\x -> [x])