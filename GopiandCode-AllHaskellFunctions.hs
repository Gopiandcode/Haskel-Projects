lIndex :: [a] -> Int -> a
lIndex xs i = last [i | (i, b)<-zip xs ys]
        where ys = [x | x <-[0..i]]
rIndex :: [a] -> Int -> a
rIndex zs i = auxFunc zs i
              where auxFunc (x:xs) i = if i == 0 then x else auxFunc xs (i-1)
--hIndex :: [Int] -> Int -> Int
--hIndex xs i =  

nEval :: (a -> b) -> a -> b
nEval f a = f a

nAbs :: (Num a, Ord a) => a -> a
nAbs x
    | x < 0      = -x
    | otherwise  =  x

lAll :: (a -> Bool) -> [a] -> Bool
lAll f xs = last ys
           where ys = [True] ++ [f (xs !! i) && (ys !! i) | i <- [0..length xs -1]]

rAll :: (a -> Bool) -> [a] -> Bool
rAll f [] = True
rAll f (x:xs)
    | f x         = True && rAll f xs
    | otherwise   = False

hAll :: (a -> Bool) -> [a] -> Bool
hAll f xs = foldr (\x acc -> f x && acc) True xs

lAnd :: [Bool] -> Bool
lAnd xs = last ys
        where ys = [True] ++ [(xs !! i) && (ys !! i) | i <- [0..length xs -1]]

rAnd :: [Bool] -> Bool
rAnd (x:[]) = x
rAnd (x:xs) = x && rAnd xs

hAnd :: [Bool] -> Bool
hAnd xs = foldr (\x acc -> x && acc) True xs

lAny :: (Eq a) => (a -> Bool) -> [a] -> Bool
lAny f xs = last ys
         where ys = [False] ++ [ f (xs !! i) || (ys !! i) | i <- [0..length xs -1]]

rAny :: (Eq a) => (a -> Bool) -> [a] -> Bool
rAny f [] = False
rAny f (x:xs) = (f x) || rAny f xs

hAny :: (Eq a) => (a -> Bool) -> [a] -> Bool
hAny f xs = foldr (\x acc -> (f x) || acc) False xs

nCompare :: (Ord a) => a -> a -> Ordering
nCompare a b
    | a > b         = GT
    | a < b         = LT
    | otherwise     = EQ

lConcat :: [[a]] -> [a]
lConcat (xs:xss) = last ys
             where ys = [xs] ++ [(xss !! i) ++ (ys !! i) | i <- [0..length xss -1]]

rConcat :: [[a]] -> [a]
rConcat [] = []
rConcat (xs:xss) = xs ++ (rConcat xss)

hConcat :: [[a]] -> [a]
hConcat xs = foldr1 (\xs acc -> xs ++ acc) xs

--lBreak :: (a -> Bool) -> [a] -> ([a], [a])
--lBreak f xs = ([ i | i <- xs,])
--        where ys = 

nBreak :: (a -> Bool) -> [a] -> ([a], [a])
nBreak f xs = (fhalf xs, rhalf xs)
          where fhalf []     = []
                fhalf (x:xs) = if f x then x : (fhalf xs) else []
                rhalf []     = []
                rhalf (x:xs) = if f x then rhalf xs else x:xs
                
nCurry :: ((a,b) -> c) -> a -> b -> c
nCurry f a b = f (a,b)

nCycle :: [a] -> [a]
nCycle xs = [xs !! (mod i (length xs)) | i <- [1..]]

lDrop :: Int -> [a] -> [a]
lDrop n xs = [xs !! i | i <-[n..length xs -1]]

rDrop :: Int -> [a] -> [a]
rDrop 0 xs = xs
rDrop _ [] = []
rDrop n (x:xs) = rDrop (n-1) xs

hDrop :: Int -> [a] -> [a]
hDrop n xs = map (xs!!) (filter (\x -> x>= n) [0..length xs -1])

lDropWhile :: (a -> Bool) -> [a] -> [a]
lDropWhile f (x:xs)  = last ys
            where ys = if f x then [[x]] ++ [if f (xs !! i) then [] else ys !! i ++ [xs!!i] | i <- [0..length xs -1]] else [[]] ++ [if f (xs !! i) then [] else ys !! i ++ [xs!!i] | i <- [0..length xs -1]]

rDropWhile :: (a -> Bool) -> [a] -> [a]
rDropWhile f [] = []
rDropWhile f (x:xs) = if f x then rDropWhile f xs else x:xs


hDropWhile :: (a -> Bool) -> [a] -> [a]
hDropWhile f xs = foldr (\x acc -> if f x then acc else x:acc) [] xs

lElem :: (Eq a) => a -> [a] -> Bool
lElem x xs = last ys
        where ys = [False] ++ [(xs!!i == x) || ys !! i | i <- [0..length xs -1]]

rElem :: (Eq a) => a -> [a] -> Bool
rElem a [] = False
rElem a (x:xs) = if x == a then True else rElem a xs

hElem :: (Eq a) => a -> [a] -> Bool
hElem a xs = foldr (\x acc -> (x == a) || acc) False xs

lFilter :: (a -> Bool) -> [a] -> [a]
lFilter f xs = last ys
           where ys = [[]] ++ [if f (xs !! i) then ys !! i ++ [xs !! i] else ys !! i | i <- [0..length xs - 1]]

rFilter :: (a -> Bool) -> [a] -> [a]
rFilter f [] = []
rFilter f (x:xs) = if f x then x: (rFilter f xs) else rFilter f xs

hFilter :: (a -> Bool) -> [a] -> [a]
hFilter f xs = foldr (\x acc -> if f x then x : acc else acc) [] xs

nFlip :: (a -> b -> c) -> b -> a -> c
nFlip f = g 
       where g y x = f x y

nFoldl :: (b -> a -> b) -> b -> [a] -> b
nFoldl f b ([]) = b
nFoldl f b (x:xs) = f (nFoldl f b xs) x


nFoldl1 :: (a -> a -> a) -> [a] -> a
nFoldl1 f (x:[]) = x
nFoldl1 f (x:xs) = f (nFoldl1 f xs) x

nFoldr :: (a -> b -> b) -> b -> [a] -> b
nFoldr f b [] = b
nFoldr f b (x:xs) = f x (nFoldr f b xs)

nFoldr1 :: (a -> a -> a) -> [a] -> a
nFoldr1 f (x:[]) = x
nFoldr1 f (x:xs) = f x (nFoldr1 f xs)

nFst :: (a,b) -> a
nFst (a,b) = a

lGcd :: (Integral a) => a -> a -> a
lGcd a b = min (fst (last (takeWhile (\(x,y) -> x > 0 && y > 0) ys))) (snd (last (takeWhile (\(x,y) -> x > 0 && y > 0) ys)))
         where ys = [(a,b)] ++ [(snd (ys !! i),(fst (ys !! i)) `mod` snd (ys !! i)) | i<-[0..], (fst (ys !! i) > 0) && (snd (ys !! i) > 0)]

rGcd :: (Integral a) => a -> a -> a
rGcd a 0 = a
rGcd 0 b = b
rGcd a b = rGcd (b) (a `mod` b)

--hGcd :: (Integral a) -> a -> a -> a
--hGcd a b = 

lInit :: [a] -> [a]
lInit (x:xs) = last ys
    where ys = [[x]] ++ [ys!! i ++ [xs !! i]| i <- [0..length xs -2]] 

rInit :: [a] -> [a]
rInit (x:xs) = if length xs == 1 then x:[] else x : (rInit xs)

hInit :: [a] -> [a]
hInit xs = map (xs!!) (filter (\i -> i < length xs -1) [0..length xs])

lIterate :: (a -> a) -> a -> [a]
lIterate f x = ys
        where ys = [x , f x] ++ [f (ys !! i) | i <- [0..]]

rIterate :: (a -> a) -> a -> [a]
rIterate f x = x : rIterate f (f x)

--hIterate :: (a -> a) -> a -> [a]
--hIterate f x = zs
--      where zs = [x] ++ (foldl (\x acc -> [(f x)] ++ acc) [] zs)

lLast :: [a] -> a
lLast xs = head [xs !! i | i <- [(length xs -1)..(length xs)]]

rLast :: [a] -> a
rLast (x:[]) = x
rLast (x:xs) = rLast xs

hLast :: [a] -> a
hLast xs = foldr1 (\x acc -> acc) xs

nLcm :: (Integral a) => a -> a -> a
nLcm a b = foldr1 min ([i | i <- [0..a*b], (i `mod` a) == 0, i`mod` b == 0])

nLength :: (Num a) => [a] -> a
nLength xs = sum [1 | i <- xs]

lMap :: (a -> b) -> [a] -> [b]
lMap f xs = [f x | x <-xs]

rMap :: (a -> b) -> [a] -> [b]
rMap f [] = []
rMap f (x:xs) = (f x) : (rMap f xs)

hMap :: (a -> b) -> [a] -> [b]
hMap f (xs) = foldr (\x acc -> (f x) : acc) [] xs

nMax :: (Ord a) => a -> a -> a
nMax a b = if a>b then a else b

lMaximum :: (Ord a) => [a] -> a
lMaximum (x:xs) = last ys
        where ys = [x] ++ [if xs !! i > ys !! i then xs !! i else ys !! i | i <- [0..length xs -1]]

rMaximum :: (Ord a) => [a] -> a
rMaximum (x:xs) = auxFunc x xs
                 where auxFunc y [] = y
                       auxFunc y (z:zs) = if z > y then auxFunc z zs else auxFunc y zs

hMaximum :: (Ord a) => [a] -> a
hMaximum xs = foldr1 max xs

nMin :: (Ord a) => a -> a -> a
nMin a b = if a < b then a else b

lMinimum :: (Ord a) => [a] -> a
lMinimum (x:xs) = last zs
          where zs = [x] ++ [if zs !! i < xs !! i then zs !! i else xs !! i | i <- [0..length xs -1]]

rMinimum :: (Ord a) => [a] -> a
rMinimum (x:xs) = auxFunc x xs
          where auxFunc y [] = y
                auxFunc y (z:zs) = if z < y then auxFunc z zs else auxFunc y zs

hMinimum :: (Ord a) => [a] -> a
hMinimum xs = foldr1 min xs

lNotElem :: (Eq a) => a -> [a] -> Bool
lNotElem v xs = last zs
             where zs = [True] ++ [if (xs !! i) == v then False else True && zs !! i | i <- [0..length xs - 1]]

rNotElem :: (Eq a) => a -> [a] -> Bool
rNotElem v [] = True
rNotElem v (x:xs) = if x /= v then rNotElem v xs else False

hNotElem :: (Eq a) => a -> [a] -> Bool
hNotElem v xs = foldr (\x acc -> (x /= v) && acc) True xs

nNull :: [a] -> Bool
nNull xs = foldr (\x y -> False) True xs

lProduct :: (Num a) => [a] -> a
lProduct xs = last ys
        where ys = [1] ++ [(xs !! i) * (ys !! i) | i <- [0..length xs -1]]

rProduct :: (Num a) => [a] -> a
rProduct (x:[]) = x
rProduct (x:xs) = x * (rProduct xs)
        
hProduct :: (Num a) => [a] -> a
hProduct xs = foldr (*) 1 xs

lRepeat :: a -> [a]
lRepeat x = [x | i <-[1..]]

rRepeat :: a -> [a]
rRepeat x = x: (rRepeat x)

--hRepeat :: a -> [a]
--hRepeat x = foldr (:) [] (x: hRepeat x)

lReplicate :: Int -> a -> [a]
lReplicate n xs = [xs| _ <-[0..n]]

rReplicate :: Int -> a -> [a]
rReplicate n (xs) = if n > 0 then xs : (rReplicate (n-1) xs) else []

hReplicate :: Int -> [a] -> [[a]]
hReplicate n xs = map (xs ++ ) [[] | _ <- [0..n]]

lReverse :: [a] -> [a]
lReverse xs = last zs
          where zs = [[]] ++ [(xs !! i) : (zs !! i) | i <- [0..length xs-1]]

rReverse :: [a] -> [a]
rReverse xs = revInto [] xs
        where revInto ls [] = ls
              revInto ys (z:zs) = revInto (z:ys) (zs)
    
hReverse :: [a] -> [a]
hReverse xs = foldr (\x acc -> acc ++ [x]) [] xs

lScanl :: (a->b->b) -> b -> [a] -> [b]
lScanl f z xs = zs
         where  zs = [z] ++ [f (xs !! i) (zs !! i) | i <- [0..length xs -1]]

         
lScanl1 :: (a -> a -> a) -> [a] -> [a]
lScanl1 f (x:xs) = zs
         where zs = [x] ++ [f (xs !! i) (zs !! i) | i <- [0..length xs -1]]

lScanr :: (a -> b -> b) -> b -> [a] -> [b]
lScanr f z xs = last zs
        where zs = [[z]] ++ [[f (xs !! i) ((zs !! i) !! i)] ++ (zs !! i) | i <- [0..length xs -1]]
        
rScanr :: (a -> b -> b) -> b -> [a] -> [b]    
rScanr f z [] = [z]
rScanr f z (x:xs) = f x q : qs
                where qs@(q:_) = rScanr f z xs