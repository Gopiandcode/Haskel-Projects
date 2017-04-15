printOut :: (Show a) => a -> IO ()
printOut a = do
    putStrLn ("99 Haskell Problems - Problem no. " ++ (show a) ++ ":")

delimiters :: [Char]
delimiters = [' ', '\n', ',']

getWord :: IO String
getWord = do
    x <- getChar
    if(x `elem` delimiters)
     then return []
     else do
        y <- getWord
        return (x : y)

getInt = do
    x <- getWord
    let xint = (read x) :: Int
    return xint
 
getList :: IO [Int]
getList = do 
    x <- getWord
    if(x == [])
        then return []
        else do
            yints <- getList
            let xint = (read x) :: Int
            return (xint : yints)

kth_element_recursively :: Int -> [a] -> a
kth_element_recursively 1 []     = error "Index out of bounds"
kth_element_recursively 1 (x:xs) = x
kth_element_recursively a (x:xs) = kth_element_recursively (a-1) xs
kth_element_recursively _  _     = error "Index out of bounds"

kth_element_higher_order k = head . drop (k-1)

main = do
    printOut 3
    putStrLn "Enter a list:"
    xs <- getList
    putStrLn "Enter a kth element:"
    kth <- getInt
    putStrLn "Recursively obtained kth element:"
    putStrLn (show $ kth_element_recursively kth xs)
    putStrLn "Higher order function obtained kth element:"
    putStrLn (show $ kth_element_higher_order kth xs)
