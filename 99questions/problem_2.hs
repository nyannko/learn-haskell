-- find the last but one element of a list
-- myButLast

-- 1
myButLast x = reverse x !! 1

-- 2
myButLast' [x, _] = x
myButLast' (_:xs) = myButLast'' xs

-- 3
myButLast'' [] = error "No such element"
myButLast'' [_] = error "No such element"
myButLast'' x = head(tail(reverse x))

-- 4
myButLast''' [] = error "No such element"
myButLast''' [_] = error "No such element"
myButLast''' x = head(reverse(init x))

-- 5
{-myButLast'''' [a] -> a-}
myButLast'''' = last . init

-- 6 ?
myButLast''''' (x:(_:[])) = x
myButLast''''' (_:xs) = myButLast''''' xs

-- 7 ?
lastbut1 :: Foldable f => f a -> a
lastbut1 = fst . foldl (\(a,b) x -> (b,x)) (err1,err2)
  where
    err1 = error "lastbut1: Empty list"
    err2 = error "lastbut1: Singleton"

-- 8 ?
lastbut1safe :: Foldable f => f a -> Maybe a
lastbut1safe = fst . foldl (\(a,b) x -> (b, Just x)) (Nothing, Nothing)

-- 9 !
myButLast'''''' [] = error "Empty lsit"
myButLast'''''' [x] = error "Too few elements"
myButLast'''''' (x:xs) = 
                  if length xs == 1 then x
                  else myButLast'''''' xs
