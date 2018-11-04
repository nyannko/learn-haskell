-- list comprehension
-- [ x | x <- [50..100], x `mod` 7 == 3]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

replaceBy2 xs = [2 | x <- xs]
-- replace by 1 and sum
len' xs = sum [1 | _ <- xs]

xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
loopcomprehension xxs = [[ x | x <- xs, even x ] | xs <- xxs] 
-- loopcomprehension xxs


-- tuple
triangles = [(x, y, z) | x <- [1..10], y <- [1..10], z <- [1..10], x^2 + y^2 == z^2, x + y + z == 24]

-- type: Int, Integer, Float, Double, Bool, Char

factorial :: Integer -> Integer
factorial x = product [1..x]

circumference :: Float -> Float
circumference r = 2 * pi * r

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

removeLowercase :: [Char] -> [Char]
removeLowercase st = [x | x <- st, x `elem` ['A'..'Z']]



fizzBuzz :: Int -> String 
fizzBuzz x
    | x < 3 = show x
    | otherwise = show 0
