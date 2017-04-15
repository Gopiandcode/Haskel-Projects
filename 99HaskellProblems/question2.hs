printOut :: (Show a) => a -> IO ()
printOut a = do
    putStrLn ("99 Haskell Problems - Problem no. " ++ (show a) ++ ":")

delimiters :: [Char]
delimiters = [' ', '\n', ',']

getWord :: IO String
getWord = do
    x <- getChar
    if(x `elem` delimiters) then return []
        else do
            y <- getWord
            return (x:y)

getList :: IO [Int]
getList = do
    x <- getWord
    if(x == [])
        then return []
        else do
            yints <- getList
            let xint = (read x)::Int
            return (xint : yints)

second_to_last_recursive :: [a] -> a
second_to_last_recursive (x:y:[]) = x
second_to_last_recursive (x:xs)   = second_to_last_recursive xs
second_to_last_recursive _        = error "second_to_last_recursive called with insufficient parameters"

second_to_last_higher_order :: [a] -> a
second_to_last_higher_order = head . foldr (\x acc -> if (length acc) < 2 then x:acc else acc) []

main = do
    printOut 2
    putStrLn "Enter a list:"
    xs <- getList
    putStrLn "Second to last element obtained recursively:"
    putStrLn (show $ second_to_last_recursive xs)
    putStrLn "Second to last element obtained by higher order methods:"
    putStrLn (show $ second_to_last_higher_order xs)