import System.Environment (getArgs)

data Point a = Point a a
data Direction = Left' | Right' | Colinear deriving (Show)


interactWith function inputFile outputFile = do
        input <- readFile inputFile
        writeFile outputFile (function input)

stringToTupleList input = commaSep (lines input) []
                  where commaSep (x:ys) output = if length bs /= 2 then error "Malformed Data" else commaSep ys ((a,b):output)
                                                where a = read (bs !! 0) :: Double
                                                      b = read (bs !! 1) :: Double
                                                      (bs) = split (==',') x
                        commaSep [] output = output

split pred (x:xs) = a : (split pred b)
                   where (a,b_raw) = break pred (x:xs)
                         b         = if not (null b_raw) then drop 1 b_raw else []
split pred []     = []


h_tupleListToString ((a,b):xs) = show a : "," :  show b : "\n" : (h_tupleListToString xs)
h_tupleListToString []         = []

tupleListToString input = concat (h_tupleListToString input)

lineHeading (Point x1 y1) (Point x2 y2) (Point x3 y3)
    | val > 0                               = Right'
    | val == 0                              = Colinear
    | val < 0                               = Left'
    where val = (y2 - y1) * (x3 - x2) - (x2 - x1) * (y3 - y2)

getDirections (a:b:c:rs) = (lineHeading a b c) : getDirections (b : c : rs)
getDirections _          = []

insert x [] = [x]
insert x (y:xs)
   | x <= y           = x : y : xs
   | x > y           = y : (insert x xs)


isort (x:xs) = insert x (isort xs)
isort _      = []

qsort f (x:xs) = qsort f [a|a<-xs,f a <= f x] ++ [x] ++ qsort f [b|b<-xs,f b > f x]
qsort f _      = []

pointToPolarAngle (Point refx refy) (Point x y) = atan2 (y-refy) (x-refx)

getPointX (Point x y) = x
getPointY (Point x y) = y

sortPointsByPolarAngle (x:xs) = (minPoint, removePointFromList minPoint (qsort (pointToPolarAngle minPoint) (x:xs)))
                    where minPoint = minXCoordPoint x xs
                          minXCoordPoint (Point x1 y1) ((Point x2 y2):points) = if y2 < y1 
                                                                                  then minXCoordPoint (Point x2 y2) points 
                                                                                  else if y1 == y2 
                                                                                       then if x1 < x2 
                                                                                               then minXCoordPoint (Point x1 y1) points
                                                                                               else minXCoordPoint (Point x2 y2) points
                                                                                       else minXCoordPoint (Point x1 y1) points
                          minXCoordPoint point _                              = point

removePointFromList (Point x1 y1) ((Point x2 y2):xs) = if x1 == x2 && y1 == y2 then xs else (Point x2 y2) : removePointFromList (Point x1 y1) xs
removePointFromList _              _                 = []


toPointsList ((a, b):xs) = Point a b : toPointsList xs
toPointsList _           = []

fromPointsList ((Point x y):xs) = (x,y) : fromPointsList xs
fromPointsList _                = []

grahamsScan:: (RealFloat a) => [(a,a)] -> [(a,a)]
grahamsScan xs = if length pointsList < 3 then error "Does not work for less than 3 points" else fromPointsList (func rest [c,b,a])
                 where pointsList = point : points
                       (point, points) = sortPointsByPolarAngle (toPointsList xs)
                       (a,b,c,rest) = (pointsList !! 0, pointsList !! 1, pointsList !! 2, drop 3 pointsList)
                       func (o:rest) (s:p:stack) = case (lineHeading p s o) of
                             Right'      -> func (o:rest) (p:stack)
                             otherwise   -> func (rest) (o:s:p:stack)
                       func []  output    = output

findConvexHull input = tupleListToString (grahamsScan (stringToTupleList input))

main = do
      args <- getArgs
      case args of
          [input,output]   -> interactWith findConvexHull input output
          _                -> error "This program requires an input file of tuples and an output file to write to."
