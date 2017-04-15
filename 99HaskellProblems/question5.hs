printOut :: (Show a) => a -> IO ()
printOut a = do
    putStrLn ("99 Haskell Problems - Problem no. " ++ (show a) ++ ":")

delimiters :: String
delimiters = " \n,"

getWord :: IO String
getWord = do
    x <- getChar
    if (x `elem` delimiters)
        then return []
        else do
            y <- getWord
            return (x: y)

getList :: IO [Int]
getList = do
    x <- getWord
    if(x == [])
        then return []
        else do
            yints <- getList
            let xint = (read x) :: Int
            return (xint : yints)

reverse_list_recursive:: [a] -> [a]
reverse_list_recursive = revinto []
    where revinto xs (a:bs) = revinto (a:xs) bs
          revinto xs []     = xs

reverse_list_higher_order :: [a] -> [a]
reverse_list_higher_order = foldr (\x acc -> acc ++ [x]) []

main = do
    printOut 5
    putStrLn "Enter a list:"
    xs <- getList
    putStrLn "Recursively obtained reversed list:"
    putStrLn (show $ reverse_list_recursive xs)
    putStrLn "Higher Orderly obtained reversed list:"
    putStrLn (show $ reverse_list_higher_order xs)