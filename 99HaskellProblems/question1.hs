printOut:: (Num a, Show a) => a -> IO ()
printOut a = do
      putStrLn ("99 Haskell Problems - Problem no. " ++ (show a) ++ ":")


getWord :: IO String
getWord = do
      x <- getChar
      if(x == ' ' || x == '\n' || x == ',') then return []
        else do
          y <- getWord
          return (x : y)

getList :: IO [Int]
getList = do
      x <- getWord
      if(x == []) then return []
        else do
            let xint = (read x):: Int
            y <- getList
            return (xint : y)

last_elem_recursive :: [a] -> a
last_elem_recursive [] = error "Can not get the last element of an empty list."
last_elem_recursive (x:[]) = x
last_elem_recursive (x:xs) = last_elem_recursive xs

last_elem_higher_order :: (Eq a) => [a] -> a
last_elem_higher_order = head . foldr (\x acc -> if acc == [] then [x] else acc) []


main = do
    printOut 1
    putStrLn "Enter a list on a line, and then input a blank line:"
    xs <- getLine
    putStrLn "Recursively obtained last element:"
    putStrLn (show $ last_elem_recursive xs)
    putStrLn "Higher order obtained last element:"
    putStrLn (show $ last_elem_higher_order xs)