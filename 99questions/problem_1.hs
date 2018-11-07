-- find the last element of a list
-- Prelude> myLast [1,2,3,4]
-- 4
-- Prelude> myLast ['x','y','z']
-- 'z'

-- 1
myLast :: [a] -> a
myLast [] = error "No end for empty lists!"
myLast [x] = x
myLast (_:xs) = myLast xs

-- 2
myLast' [] = error "No end for empty list"
myLast' xs = xs !! (sum [1 | _ <- xs] - 1)

-- 3
myLast'' [] = error "No end for empty list"
myLast'' xs = xs !! (length xs - 1)
{-myLast''' = foldr1 (const id)-}

-- 4
myLast'''' = head . reverse
-- head(reverse([1,2,3]))
