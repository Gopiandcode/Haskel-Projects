import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)


main = mainWith i_func
   where mainWith function = do
             args <- getArgs
             case args of
              [input, output] -> interactWith function input output
              _ -> putStrLn "error: exactly two arguments needed"
         i_func = id
