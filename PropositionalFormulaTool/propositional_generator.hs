import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.List (foldl')
import System.Process (callCommand)
import System.Directory (doesFileExist)

data Variable = P | Q | R
data Formula = Literal Variable | Not Formula | And Formula Formula | Or Formula Formula | Implication Formula Formula

instance Show Variable where
        show P = "p"
        show Q = "q"
        show R = "r"
instance Show Formula where
        show (Literal a) = show a
        show (Not a) = "-" ++ show a ++ ""
        show (And a b) = "(" ++ (show a) ++ "^" ++ (show b) ++ ")"
        show (Or a b) = "(" ++ (show a) ++ "v" ++ (show b) ++ ")"
        show (Implication a b) = "(" ++ (show a) ++ ">" ++ (show b) ++ ")"


data RndGen = RndGen Integer deriving Show

next :: RndGen -> (Integer, RndGen)
next (RndGen n) = (n, RndGen ((n *17) `mod` 23))

randProposition :: RndGen -> (Variable, RndGen)
randProposition gen = case ((fst res) `mod` 3) of
                0  -> (P, snd res)
                1  -> (Q, snd res)
                2  -> (R, snd res)
                where 
                      res = next gen
randTrueFormula :: RndGen -> Valuation -> (Formula, RndGen)
randTrueFormula gen valuation = case evaluate valuation proposition of
                                                True  -> (Literal proposition, finalGen)
                                                False -> (Not (Literal proposition), finalGen)
                                        where propositionRes = randProposition gen
                                              proposition = fst propositionRes
                                              finalGen = snd propositionRes

randUnsatisfiableFormula :: RndGen -> (Formula, RndGen)
randUnsatisfiableFormula gen = case ((fst res) `mod` 3) of
                        0      -> (And (Literal P) (Not (Literal P)), snd res)
                        1      -> (And (Literal R) (Not (Literal R)), snd res) 
                        2      -> (And (Literal Q) (Not (Literal Q)), snd res)
                        where res = next gen

growUnsatisfiably :: (Formula, RndGen) -> (Formula, RndGen)
growUnsatisfiably (formula, gen) = case formula of
                Literal prop            -> case ((fst res) `mod` 3) of
                                        0               -> (And (Literal prop) (Or (Literal newprop) (Not (Literal newprop))), snd fnl)
                                        1               -> (Or  (Literal prop) (And (Literal newprop) (Not (Literal newprop))), snd fnl)
                                        2               -> (And  (Literal prop) (Implication (Literal newprop) (Literal newprop)), snd fnl)
                                        where res = next gen
                                              fnl = randProposition (snd res)
                                              newprop = fst fnl
                Not    fmla             -> (Not (fst res), snd res)
                                        where res = growUnsatisfiably (fmla, gen)
                And formulaA formulaB   -> case ((fst res) `mod` 3) of
                                        0               -> (And (And formulaA formulaB) (Not newformula), snd fnl)
                                                         where fnl = randUnsatisfiableFormula (snd res)
                                                               newformula = fst fnl
                                        1               -> (Or (And formulaA formulaB) (newformula), snd fnl)
                                                         where fnl = randUnsatisfiableFormula (snd res)
                                                               newformula = fst fnl
                                        2               -> (And newFormulaA newFormulaB, snd fnl)
                                                         where newFormulaAres = growUnsatisfiably (formulaA, snd res)
                                                               newFormulaA    = fst newFormulaAres
                                                               fnl            = growUnsatisfiably (formulaB, snd newFormulaAres)
                                                               newFormulaB    = fst fnl
                                        where res = next gen
                Or formulaA formulaB    -> case ((fst res) `mod` 3) of
                                        0               -> (And (Or formulaA formulaB) (Not newformula), snd fnl)
                                                         where fnl = randUnsatisfiableFormula (snd res)
                                                               newformula = fst fnl
                                        1               -> (Or (Or formulaA formulaB) newformula, snd fnl)
                                                         where fnl = randUnsatisfiableFormula (snd res)
                                                               newformula = fst fnl
                                        2               -> (Or newFormulaA newFormulaB, snd fnl)
                                                         where newFormulaAres = growUnsatisfiably (formulaA, snd res)
                                                               newFormulaA    = fst newFormulaAres
                                                               fnl            = growUnsatisfiably (formulaB, snd newFormulaAres)
                                                               newFormulaB    = fst fnl
                                        where res = next gen
                Implication formulaA formulaB    -> case ((fst res) `mod` 3) of
                                        0               -> (And (Implication formulaA formulaB) (Not newformula), snd fnl)
                                                         where fnl = randUnsatisfiableFormula (snd res)
                                                               newformula = fst fnl
                                        1               -> (Or (Implication formulaA formulaB) newformula, snd fnl)
                                                         where fnl = randUnsatisfiableFormula (snd res)
                                                               newformula = fst fnl
                                        2               -> (Implication newFormulaA newFormulaB, snd fnl)
                                                         where newFormulaAres = growUnsatisfiably (formulaA, snd res)
                                                               newFormulaA    = fst newFormulaAres
                                                               fnl            = growUnsatisfiably (formulaB, snd newFormulaAres)
                                                               newFormulaB    = fst fnl
                                        where res = next gen



randTruePropositions :: Int -> RndGen -> Valuation -> ([Formula],RndGen)
randTruePropositions 0 gen _ = ([], gen)
randTruePropositions n gen valuation = (formula : remain, finalGen)
                                where propositionRes = randProposition gen
                                      proposition = fst propositionRes
                                      genA = snd propositionRes
                                      remainRes = randTruePropositions (n-1) genA valuation
                                      remain = fst remainRes
                                      finalGen = snd remainRes
                                      formula = case evaluate valuation proposition of
                                                True  -> Literal proposition
                                                False -> Not (Literal proposition)


randFormulas :: Int -> Int -> RndGen -> Valuation -> Formula -> ([Formula], RndGen)
randFormulas 0 _ gen _ _ = ([], gen)
randFormulas n branching gen valuation formula = (expandedFormula : remainingFormulas, finalGen)
                                        where expandedFormulaRes = growBranch branching valuation (formula, gen)
                                              expandedFormula    = fst expandedFormulaRes
                                              genA               = snd expandedFormulaRes
                                              newvaluationRes    = randValuation genA
                                              newvaluation       = fst newvaluationRes
                                              genB               = snd newvaluationRes
                                              newformulaRes      = randTrueFormula genB newvaluation
                                              newformula         = fst newformulaRes
                                              genC               = snd newformulaRes
                                              remainingFormulaRes= randFormulas (n-1) branching genC newvaluation newformula
                                              remainingFormulas  = fst remainingFormulaRes
                                              finalGen           = snd remainingFormulaRes

randUnsatisfiableFormulas :: Int -> Int  -> RndGen -> Formula -> ([Formula], RndGen)
randUnsatisfiableFormulas 0 _ gen _ = ([], gen)
randUnsatisfiableFormulas n branching gen formula = (expandedFormula : remainingFormulas, finalGen)
                                where expandedFormulaRes = growBranchUnsatisfiably branching (formula, gen)
                                      expandedFormula    = fst expandedFormulaRes
                                      genA               = snd expandedFormulaRes
                                      newFormulaRes      = randUnsatisfiableFormula genA
                                      newFormula         = fst newFormulaRes
                                      genB               = snd newFormulaRes
                                      remainingFormulaRes= randUnsatisfiableFormulas (n-1) branching genB newFormula
                                      remainingFormulas  = fst remainingFormulaRes
                                      finalGen           = snd remainingFormulaRes


genFormulas :: Int -> Int -> RndGen -> ([Formula], RndGen)
genFormulas n branching gen = randFormulas n branching finalGen newvaluation newformula
                        where newvaluationRes = randValuation gen
                              newvaluation    = fst newvaluationRes
                              genA            = snd newvaluationRes
                              newformulaRes   = randTrueFormula genA newvaluation
                              newformula      = fst newformulaRes
                              finalGen        = snd newformulaRes

genUnsatisfiableFormulas :: Int -> Int -> RndGen -> ([Formula], RndGen)
genUnsatisfiableFormulas n branching gen = randUnsatisfiableFormulas n branching finalGen newFormula
                        where newFormulaRes = randUnsatisfiableFormula gen
                              finalGen      = snd newFormulaRes
                              newFormula    = fst newFormulaRes

randValuation :: RndGen -> (Valuation, RndGen)
randValuation gen = (Valuation resbA resbB resbC, finalGen)
                where resA = randBool gen
                      resbA = fst resA
                      genA = snd resA
                      resB = randBool genA
                      resbB = fst resB
                      genB = snd resB
                      resC = randBool genB
                      resbC = fst resC
                      finalGen = snd resC
                      

randChoice :: RndGen -> [a] -> (a, RndGen)
randChoice gen xs = (xs !! fromIntegral ((fst res) `mod` toInteger (length xs)), snd res)
                where 
                        res = next gen

randBool :: RndGen -> (Bool, RndGen)
randBool gen = case (fst res) `mod` 2 of
                0 -> (True, snd res)
                1 -> (False, snd res)
        where 
                res = next gen

data Valuation = Valuation {
        pValue :: Bool,
        qValue :: Bool,
        rValue :: Bool
} deriving Show


evaluate :: Valuation -> Variable -> Bool
evaluate valuation variable = case variable of
        P -> pValue valuation
        Q -> qValue valuation
        R -> rValue valuation

evaluateFormula :: Valuation -> Formula -> Bool
evaluateFormula valuation (Literal a) = evaluate valuation a
evaluateFormula valuation (Not a) = not $ evaluateFormula valuation a
evaluateFormula valuation (And a b) =  (evaluateFormula valuation a) && (evaluateFormula valuation b)
evaluateFormula valuation (Or a b) =  (evaluateFormula valuation a) || (evaluateFormula valuation b)
evaluateFormula valuation (Implication a b) =  (not (evaluateFormula valuation a)) || (evaluateFormula valuation b)


                
-- given a formula under a valuation will expand it maintaining its valuation 
grow :: Valuation -> (Formula, RndGen)  -> (Formula, RndGen)
grow valuation (formula, gen) = case formula of
                Literal a -> case evaluate valuation a of
                        True -> case evaluate valuation variable of
                                        True -> (connective (Literal a) (Literal variable), genB)
--                                                  where (connective, gen) = randChoice gen [And, Or, Implication]
                                                  where connectiveRes = randChoice genA [And, Or, Implication]
                                                        connective = fst connectiveRes
                                                        genB = snd connectiveRes
                                        False -> case value of
                                                        0 -> (And (Literal a) (Not (Literal variable)), genB)
                                                        1 -> case shouldNegate of
                                                                        True  -> (Or (Literal a) (Not (Literal variable)), genC)
                                                                        False -> (Or (Literal a) (Literal variable), genC)
--                                                                where (shouldNegate, gen) = randBool genA
                                                                where shouldNegateRes = randBool genB
                                                                      shouldNegate    = fst shouldNegateRes
                                                                      genC            = snd shouldNegateRes
                                                        2 -> case shouldNegate of
                                                                        True ->  (Implication (Not (Literal variable)) (Literal a), genC)
                                                                        False -> (Implication (Literal variable) (Literal a), genC)
--                                                                where (shouldNegate, gen) = randBool gen
                                                                where shouldNegateRes = randBool genB
                                                                      shouldNegate    = fst shouldNegateRes
                                                                      genC            = snd shouldNegateRes
                                                where 
--                                                        (randval, gen) = next gen
                                                        randValRes  = next genA
                                                        value = (fst randValRes) `mod` 3
                                                        genB = snd randValRes
--                                where (variable, gen) = randProposition gen
                                where propRes = randProposition gen
                                      variable = fst propRes
                                      genA = snd propRes
                        False -> case evaluate valuation variable of
                                        True -> case value of
                                                        0 -> (Or (Literal a) (Not (Literal variable)), genA)
                                                        1 -> case shouldNegate of
                                                                        True  -> (And (Literal a) (Not (Literal variable)), genC)
                                                                        False -> (And (Literal a) (Literal variable), genC)
--                                                                where (shouldNegate, gen) = randBool genA
                                                                where shouldNegateRes  = randBool genB
                                                                      shouldNegate     = fst shouldNegateRes
                                                                      genC             = snd shouldNegateRes
                                                        2 -> case shouldNegate of
                                                                        True ->  (Implication (Literal variable) (Literal a), genC)
                                                                        False -> (Implication (Not (Literal a)) (Literal variable), genC)
--                                                                where (shouldNegate, gen) = randBool gen
                                                                where shouldNegateRes  = randBool genB
                                                                      shouldNegate     = fst shouldNegateRes
                                                                      genC             = snd shouldNegateRes
                                                where
--                                                        (randval, gen) = next gen
                                                        randValRes = next genA
                                                        value = (fst randValRes) `mod` 3
                                                        genB  = snd randValRes

                                        False -> (connective (Literal a) (Literal variable), genB)
--                                                 where (connective, gen) = randChoice gen [And, Or]
                                                 where connectiveRes  = randChoice genA [And, Or]
                                                       connective     = fst connectiveRes
                                                       genB           = snd connectiveRes
--                                 where (variable, gen) = randProposition gen
                                 where variableRes = randProposition gen
                                       variable    = fst variableRes
                                       genA        = snd variableRes
                Not a     -> (Not (fst res), snd res)
                                where 
                                        res = grow valuation (a, gen)
                And a b   -> (And newa newb, finalgen)
                                where
                                        (newa, newgen) = grow valuation (a, gen)
                                        (newb, finalgen) = grow valuation (b, newgen)
                Implication a b   -> (Implication newa newb, finalgen)
                                        where 
                                                (newa, newgen) = grow valuation (a, gen)
                                                (newb, finalgen) = grow valuation (b, newgen)

                Or a b   -> (Or newa newb, finalgen)
                                where
                                        (newa, newgen) = grow valuation (a, gen)
                                        (newb, finalgen) = grow valuation (b, newgen)

prettify :: [Formula] -> String
prettify [] = ""
prettify (x:xs) = (show x) ++ "\n" ++ (prettify xs)

growBranch 0 _ res = res
growBranch n valuation res = growBranch (n-1) valuation (grow valuation res) 

growBranchUnsatisfiably :: Int -> (Formula, RndGen) -> (Formula, RndGen)
growBranchUnsatisfiably 0  res = res
growBranchUnsatisfiably n res = growBranchUnsatisfiably (n-1) (growUnsatisfiably res)

result = grow (Valuation True True True) ((Literal P), (RndGen 1))

interactWith function inputFile outputFile = do
        input <- readFile inputFile
        writeFile outputFile (function input)

printIntro = do
            putStrLn "    GopiandCode's Satisfiable Propositional Logic Formula Generator   "
            putStrLn " ====================================================================="
            putStrLn " This simple tool will generate a set of (un/)satisfiable propositional"
            putStrLn " logic formulae."
            putStrLn " You can customize the complexity of the constructed formula by "
            putStrLn " increasing the branching amount."
            putStrLn " "
            putStrLn " Command Line Parameters" 
            putStrLn " propositional_generator count complexity output [seed]"
            putStrLn " where"
            putStrLn "     count"
            putStrLn "        - the number of formula to generate"
            putStrLn "     complexity"
            putStrLn "        - the number of formula to generate"
            putStrLn "     output"
            putStrLn "        - the output file - will be overwritten if exists"
            putStrLn "     seed"
            putStrLn "        - optional - the seed used for generating random formula"
	    putStrLn " "
	    putStrLn " you can also use the system to test propositional satisfiability "
            putStrLn " checkers which conform to a specific format."
            putStrLn " "
            putStrLn " propositional_generator --test count [inputSize] [complexity] [seed]"
            putStrLn " where"
            putStrLn "     count"
            putStrLn "        - the number of tests to run"
            putStrLn "     inputSize"
            putStrLn "        - the number of formulas per file submitted to the parser - "
	    putStrLn "          defaults to 10"
            putStrLn "     complexity"
            putStrLn "        - roughly related to the size of the formulas - defaults to 3"
            putStrLn "     seed"
            putStrLn "        - seed used to generate formula"
            putStrLn " Note: Due to the terrible design I've used for accepting inputs, the"
            putStrLn " 	     only accepted permuations of arguments are: "
            putStrLn " propositional_generator --test count"
            putStrLn " propositional_generator --test count seed"
            putStrLn " propositional_generator --test count complexity seed"
            putStrLn " propositional_generator --test count inputSize complexity seed"

            
getCount = do
            putStrLn " How many formula would you like to generate?"
            result <- getLine
            case (readMaybe result) :: Maybe Int  of
                        (Just x) -> do
                                       return x
                        Nothing  -> do
                                        putStrLn " Sorry - that wasn't a valid integral value. Please try again."
                                        getCount
getType       = do
            putStrLn " Do you want to make a [s]atisfiable or [u]nsatisfiable formula:"
            result  <- getLine
            case result !! 0 of
                's'             -> return genFormulas
                'u'             -> return genUnsatisfiableFormulas
                _               -> do
                                        putStrLn " Please answer [s]atisfiable or [u]nsatisfiable:"
                                        getType
getComplexity = do
            putStrLn " How complex should the formulae be? (3 is about right, though feel free to experiment)"
            result <- getLine
            case (readMaybe result) :: Maybe Int  of
                        (Just x) -> do
                                        return x
                        Nothing  -> do
                                        putStrLn " Sorry - that wasn't a valid integral value. Please try again."
                                        getComplexity

getOutputFile = do
            putStrLn "Where should I write the file to? (if it exists I'll overwrite it)"
            getLine

-- checkFileFor words filename = do
--			content <- readFile filename
--			let resultLines = lines content

-- LastN elements function
-- Reference: https://stackoverflow.com/questions/17252851/how-do-i-take-the-last-n-elements-of-a-list
lastN :: Int -> [a] -> [a]
lastN n xs = foldl' (const . drop 1) xs (drop n xs)

checkFileFor required line = lastN (length requiredWords) inputwords == requiredWords
                        where requiredWords = words required
                              inputwords    = words line

checkForSatisifiable = checkFileFor "is satisfiable."
checkForUnsatisifiable = checkFileFor "is not satisfiable."

splitInput :: [String] -> ([String], [String], [String])
splitInput [] = ([],[],[])
splitInput (x:xs) = if checkForSatisifiable x 
                                then (x:remainingSatisfiable, remainingUnsatisfiable, remainingUnknown)
                    else if checkForUnsatisifiable x
                                then (remainingSatisfiable, x:remainingUnsatisfiable, remainingUnknown)
                    else (remainingSatisfiable, remainingUnsatisfiable, x:remainingUnknown)
                        where (remainingSatisfiable, remainingUnsatisfiable, remainingUnknown) = splitInput xs

interpretOutputFile = do
                content <- readFile "output.txt" 
                let resultLines = lines content
                return $ splitInput resultLines

generateSatisfiable gen inputSize complexityValue = do
                                         let res = (genFormulas inputSize complexityValue gen)
                                         writeFile "input.txt" $ prettify $ fst res
                                         return $ snd res

generateUnsatisfiable gen inputSize complexityValue = do
                                         let res = (genUnsatisfiableFormulas inputSize complexityValue gen)
                                         writeFile "input.txt" $ prettify $ fst res
                                         return $ snd res

testSatisfiable inputSize complexityValue ((sat, unsat, un), gen) = do
                                                genA <- generateSatisfiable gen inputSize complexityValue
                                                callCommand "./main.exe"
                                                (newSat, newUnsat, newUn) <- interpretOutputFile
                                                return ((sat, newUnsat ++ unsat, newUn ++ un), genA)

testUnsatisfiable inputSize complexityValue ((sat, unsat, un), gen) = do
                                                genA <- generateUnsatisfiable gen inputSize complexityValue
                                                callCommand "./main.exe"
                                                (newSat, newUnsat, newUn) <- interpretOutputFile
                                                return ((newSat ++ sat, unsat, newUn ++ un), genA)

storeResults (sat, unsat, un) = do
                                callCommand "touch falseSatisfiables.txt"
                                callCommand "touch falseUnsatisfiables.txt"
                                callCommand "touch unknownResults.txt"
                                appendFile "falseSatisfiables.txt" $ unlines sat
                                appendFile "falseUnsatisfiables.txt" $ unlines unsat 
                                appendFile "unknownResults.txt" $ unlines un
                                

printResults testCount (sat, unsat, un) = do
                                        putStrLn $ " Of " ++ (show testCount) ++ " tests run:"
                                        putStrLn $ "      " ++ (show $ length sat) ++   "   - were incorrectly identified as satisfiable when not."
                                        putStrLn $ "      " ++ (show $ length unsat) ++ "   - were incorrectly identified as unsatisfiable when not."
                                        putStrLn $ "      " ++ (show $ length un) ++    "   - produced unknown results."
                                        putStrLn $ " Happy debugging!"


internalRunTestSuite 0 inputSize complexityValue ((sat, unsat, un), gen) = do
                                                                                return ((sat, unsat, un), gen)
internalRunTestSuite count inputSize complexityValue ((sat, unsat, un), gen) = do
                                                                        ((satA,unsatA,unA), genA) <- testSatisfiable inputSize complexityValue ((sat,unsat,un),gen)
                                                                        ((satB,unsatB,unB), genB) <- testUnsatisfiable inputSize complexityValue ((satA,unsatA,unA),genA)
                                                                        internalRunTestSuite (count-1) inputSize complexityValue ((satB, unsatB, unB), genB)

runTests :: Int -> Int -> Int -> Integer -> IO ([String],[String],[String])
runTests count inputSize complexityValue seed = do 
                                                 res <- internalRunTestSuite count inputSize complexityValue (([],[],[]), (RndGen seed))
                                                 return $ fst res

runTestSuite count inputSize complexityValue seed = do
                                        results <- runTests count inputSize complexityValue seed
                                        printResults count results
                                        storeResults results

main = do
      args <- getArgs
      case args of
          ["--test",count]  -> runTestSuite countValue 10 3 25
                                                        where countValue      = read (count) :: Int 
          ["--test",count,seed]  -> runTestSuite countValue 10 3 (toInteger seedValue)
                                                        where countValue      = read (count) :: Int 
                                                              seedValue       = read (seed) :: Int
          ["--test",count,complexity,seed]  -> runTestSuite countValue 10 complexityValue (toInteger seedValue)
                                                        where countValue      = read (count) :: Int 
                                                              complexityValue = read (complexity) :: Int
                                                              seedValue       = read (seed) :: Int
          ["--test",count,inputSize,complexity,seed]  -> runTestSuite countValue inputSizeValue complexityValue (toInteger seedValue)
                                                        where countValue      = read (count) :: Int 
                                                              inputSizeValue  = read (inputSize) :: Int 
                                                              complexityValue = read (complexity) :: Int
                                                              seedValue       = read (seed) :: Int
          [count,complexity,output]   ->  writeFile output $ prettify $ fst (genFormulas countValue complexityValue (RndGen 25))
                                                        where countValue      = read (count) :: Int 
                                                              complexityValue = read (complexity) :: Int
          ["--unsatisfiable", count,complexity,output]   ->  writeFile output $ prettify $ fst (genUnsatisfiableFormulas countValue complexityValue (RndGen 25))
                                                        where countValue      = read (count) :: Int 
                                                              complexityValue = read (complexity) :: Int
 
          [count,complexity,output, seed]   ->  writeFile output $ prettify $ fst (genFormulas countValue complexityValue (RndGen $ toInteger seedValue))
                                                        where countValue      = read (count) :: Int 
                                                              complexityValue = read (complexity) :: Int
                                                              seedValue       = read (seed) :: Int

          ["--unsatisfiable",count,complexity,output, seed]   ->  writeFile output $ prettify $ fst (genUnsatisfiableFormulas countValue complexityValue (RndGen $ toInteger seedValue))
                                                        where countValue      = read (count) :: Int 
                                                              complexityValue = read (complexity) :: Int
                                                              seedValue       = read (seed) :: Int


          _                -> do
                                 printIntro
                                 countValue <- getCount
                                 complexityValue <- getComplexity
                                 output <- getOutputFile
                                 function <- getType
                                 writeFile output $ prettify $ fst (function countValue complexityValue (RndGen 3))
                                


