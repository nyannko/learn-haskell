import Data.Char
import Control.Monad
-- Input and Output
-- imperative languages get things done by giving the computer a series of steps to execute
-- functional programming is more of defining what stuff is.
-- In Haskell, a function can't change some state(like change the contents of a variable)
-- When a function change state, this function has side-effects.
-- The only thing a function can do in Haskell is give back some result based on the input parameters.

-- define a name called main and call a function called putStrLn with parameter.
main = putStrLn "hello, world"
-- ghc --make helloworld(file name)
-- ./helloworld

-- Prelude> :t putStrLn
-- putStrLn :: String -> IO ()
-- Prelude> :t putStrLn "hello"
-- putStrLn "hello" :: IO ()

-- putStrLn takes a string and returns an I/O action that has a result type of ()
-- An I/O action is something that, when performed, will carry out an action with a side-effect
-- An I/O action will be performed when we give it a name of main and then run our program.

main' = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")

-- main always has a type signature of main :: IO something, where something is some concrete type. 
-- By convention, we don't usually specify a type declaration for main.

-- *Main> :t getLine
-- getLine :: IO String
-- getLine is an I/O action that contains a result type of String.
-- name <- getLine: perform the I/O action getLine and then bind its result value to name.
-- to get the value out of an I/O action, you have to perform it inside another I/O action
-- by binding it to a name with <-

-- If we're taking data out of an I/O action, we can only take it out when we're inside another 
-- I/O action. This is how Haskell manages to neatly separate the pure and impure parts of our code.
-- getLine is in a sense impure because its result value is not guaranteed to be the same when performed twice.
-- Thats why it's sort of tainted with the IO type constructor and we can only get that data out in I/O code.
-- And because I/O code is tainted too, any computation that depends on tainted I/O data will have a tainted result.

-- invalid code
-- nameTag = "Hello, my name is " ++ getLine
-- ++ cannot concatenate a string and an I/O action. we first ahve to get the result out of the I/O
-- action to get a value of type String `name <- getLine`

main'' = do 
    foo <- putStrLn "Hello, what;s your name?"
    name <- getLine 
    putStrLn ("Hey " ++ name ++ ", you rock!")

-- In a do block, the last action cannot be bound to a name like the first two were.
-- for now just think of it in the way that the do block automatically extracts the value from the last action and binds it to its own result.
-- Except for the last line, every line in a do block that doesn't bind can also be written with a bind. 


-- I/O actions will only be performed when they are given a name of main or when they're inside a bigger I/O 
-- action that we composed with a do block.


-- import Data.Char
main''' = do 
    putStrLn "What's your first name?"
    firstName <- getLine 
    putStrLn "What's your last name?"
    lastName <- getLine 
    let bigFirstName = map toUpper firstName 
        bigLastName = map toUpper lastName 
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?" 

-- distinguish <-(I/O) with =(expression)

main1 = do 
    line <- getLine 
    if null line 
        then return ()
        else do 
            putStrLn $ reverseWords line 
            main 

reverseWords :: String -> String 
reverseWords = unwords . map reverse . words

-- the return in Haskell is really nothing like the return in most other langugaes.
-- In Haskell(in I/O actions specifically), it makes an I/O action out of a pure value.

main2 = do 
    return ()
    return "haha"
    line <- getLine
    return "blahblah"
    return 4
    putStrLn line

main3 = do 
    a <- return "hell"
    b <- return "yeah"
    putStrLn $ a ++ " " ++ b

main4 = do 
    let a = "hell"
        b = "yeah"
    putStrLn $ a ++ " " ++ b


-- putStr
main5 = do  putStr "Hey, "  
            putStr "I'm "  
            putStrLn "Andy!"   

-- putChar 
main6 = do putChar 't' 
           putChar 'e' 
           putChar 'h'

-- putStr :: String -> IO()
-- putStr [] = return () 
-- putStr (x:xs) = do 
--     putChar x 
--     putStr xs

main7 = do print True 
           print 2 
           print "haha" 
           print 3.2 
           print [3,4,3]

main8 = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main8
        else return ()

-- import Control.Monad 
main9 = do 
    c <- getChar 
    when (c /= ' ') $ do 
        putChar c 
        main9

-- sequence
main10 = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]

main11 = do 
    rs <- sequence [getLine, getLine, getLine] 
    print rs

-- *Main> sequence (map print [1,2,3])
-- 1
-- 2
-- 3
-- [(),(),()]
-- when we evaluate an I/O action in GHCI, it's performed 
-- and then its result is printed out, unless that result is (), in which case it's not printed out.
-- ghci> mapM print [1,2,3]
-- 1
-- 2
-- 3
-- [(),(),()]
-- ghci> mapM_ print [1,2,3]
-- 1
-- 2
-- 3

-- import Control.Monad
-- import Data.Char

main12 = forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l

main13 = do 
    colors <- forM [1,2,3,4] (\a -> do 
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        color <- getLine 
        return color) 
    putStrLn "The colors that you associate with 1,2,3 and 4 are: " 
    mapM putStrLn colors 

-- getContents
-- Lazy I/O
main14 = forever $ do 
    putStr "Give me some input: "
    l <- getLine 
    putStrLn $ map toUpper l 


