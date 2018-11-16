-- (*) Modified run-length encoding.

-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
-- P11> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']
import Problem_10
import Data.List
data ListItem a = Single a | Multiple Int a deriving (Show)
encodeModified xs = [y | x <- group xs, let y = if (length x) == 1 then Single (head x) else Multiple (length x) (head x)]
-- group: splits a list to items to lists of equal and adjacent elements.

encodeModified' :: (Eq a) => [a] -> [ListItem a]
encodeModified' = map helper . encode 
        where 
            helper (1, x) = Single x 
            helper (n, x) = Multiple n x