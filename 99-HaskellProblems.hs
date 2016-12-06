prob1 :: [a] -> a
prob1 (x:[]) = x
prob1 (x:xs) = prob1 xs


prob2 :: [a] -> a
prob2 (x:y:[]) = x
prob2 (x:xs) = prob2 xs

prob3 :: Int -> [a] -> a
prob3 0 xs = head xs
prob3 k (x:xs) = prob3 (k-1) xs

prob4 :: [a] -> Int
prob4 = foldl (\acc x -> acc + 1) 0

prob5 :: [a] -> [a]
prob5 = aux []
      where aux ys [] = ys
            aux ys (x:xs) = aux  (x:ys) (xs)

prob6 :: (Eq a) => [a] -> Bool
prob6 xs = (prob5 xs == xs)

data NestedList a = Elem a | List [NestedList a]
prob7 :: NestedList a -> [a]
prob7 (List []) = []
prob7 (Elem x) = [x]
prob7 (List (x:xs)) = (prob7 x) ++ prob7 (List xs)

prob8 :: (Eq a) => [a] -> [a]
prob8 xs = aux [] xs
         where aux ys [] = ys
               aux ys (x:xs) = if x `elem` ys then aux ys xs else aux (ys ++ [x]) xs

prob9 :: (Eq a) => [a] -> [[a]]
prob9 (x:xs) = zs
         where zs = [[x]] ++ [ if (head (zs !! (length zs - 2))) == xs !! i then (zs !! i) ++ [xs !! i] else [xs !! i] | i <- [0..length xs -1]]
