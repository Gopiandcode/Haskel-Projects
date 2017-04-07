import Data.Char (ord)
import Data.Bits (shiftL, (.&.), (.|.))
import System.Environment (getArgs)

base = 65521

interactWith function inputFile outputFile = do
           input <- readFile inputFile
           writeFile outputFile (function input)

main = mainWith a_function
       where mainWith function = do
              args <- getArgs
              case args of
                    [input,output]       -> interactWith a_function input output
                    _                    -> putStrLn "adler_32 requires exactly two inputs"
             a_function = id


adler_32 xs = let (a,b) = foldl step (1,0) xs
              in (b `shiftL` 16) .|. a
    where step (a,b) x = let a' = (a + ord x .&. 0xff)
                             b'  = (b + a')
                         in (a' `mod` base, b' `mod` base)
