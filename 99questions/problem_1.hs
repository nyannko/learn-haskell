-- find the last element of a list
-- myLast

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
