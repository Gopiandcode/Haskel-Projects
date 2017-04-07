import System.Environment (getArgs)

interactWith function inputFile outputFile = do
     input <- readFile inputFile
     writeFile outputFile (function input)



main = mainwith a_function
      where mainwith function = do
                     args <- getArgs
                     case args of
                          [input, output]  -> interactWith function input output
                          _                -> putStrLn "error: program requires exactly two input parameters"
            a_function = id


splitLines [] = []
splitLines str = let (pre,post) = break isLineEndingChar str in
              pre: case post of
                  '\r':'\n':xs     ->  splitLines xs
                  '\n':xs          ->  splitLines xs
                  '\r':xs          ->  splitLines xs
                  _                -> []
isLineEndingChar c = c == '\r' || c == '\n'
             
