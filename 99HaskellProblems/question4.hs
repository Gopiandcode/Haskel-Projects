printOut :: (Show a) => a -> IO ()
printOut a = do
    putStrLn ("99 Haskell Problems - Problem no. " ++ (show a) ++ ":")

delimiters :: String
delimiters = " \n,"

getWord :: IO String
getWord = do
    x <- getChar
    if(x `elem` delimiters)
        then return []
        else do
            y <- getWord
            return (x: y)

getList :: IO [Int]
getList = do
    x <- getWord
    if (x == []) 
        then return []
        else do
            yints <- getList
            let xint = (read x)::Int
            return (xint : yints)


list_length_recursive :: [a] -> Int
list_length_recursive [] = 0
list_length_recursive (x:xs) = 1 + (list_length_recursive xs)

list_length_higher_order :: [a] -> Int
list_length_higher_order = sum . map (\x -> 1)


main = do
    printOut 4
    putStrLn ("Enter a list:")
    xs <- getList
    putStrLn "Recursivly obtained list length:"
    putStrLn (show $ list_length_recursive xs)
    putStrLn "Higher order obtained list length:"
    putStrLn (show $ list_length_higher_order xs)

