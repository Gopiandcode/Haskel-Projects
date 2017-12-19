import System.Environment (getArgs)
import Text.Read (readMaybe)

data Variable = P | Q | R 
data Formula = Literal Variable | Not Formula | And Formula Formula | Or Formula Formula | Implication Formula Formula

instance Show Variable where
        show P = "p"
        show Q = "q"
        show R = "r"
instance Show Formula where
        show (Literal a) = show a
        show (Not a) = "-(" ++ show a ++ ")"
        show (And a b) = "(" ++ (show a) ++ "^" ++ (show b) ++ ")"
        show (Or a b) = "(" ++ (show a) ++ "v" ++ (show b) ++ ")"
        show (Implication a b) = "(" ++ (show a) ++ "->" ++ (show b) ++ ")"


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

genFormulas :: Int -> Int -> RndGen -> ([Formula], RndGen)
genFormulas n branching gen = randFormulas n branching finalGen newvaluation newformula
                        where newvaluationRes = randValuation gen
                              newvaluation    = fst newvaluationRes
                              genA            = snd newvaluationRes
                              newformulaRes   = randTrueFormula genA newvaluation
                              newformula      = fst newformulaRes
                              finalGen        = snd newformulaRes

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
                                                                        True ->  (Implication (Not (Literal variable)) (Not (Literal a)), genC)
                                                                        False -> (Implication (Literal a) (Literal variable), genC)
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

result = grow (Valuation True True True) ((Literal P), (RndGen 1))

interactWith function inputFile outputFile = do
        input <- readFile inputFile
        writeFile outputFile (function input)

printIntro = do
            putStrLn "GopiandCode's Satisfiable Propositional Logic Formula Generator"
            putStrLn "This simple tool will generate a set of satisfiable propositional logic formulae."
            putStrLn "You can customize the complexity of the constructed formula by increasing the branching amount."
getCount = do
            putStrLn "How many formula would you like to generate?"
            result <- getLine
            case (readMaybe result) :: Maybe Int  of
                        (Just x) -> do
                                       return x
                        Nothing  -> do
                                        putStrLn "Sorry - that wasn't a valid integral value. Please try again."
                                        getCount
getComplexity = do
            putStrLn "How complex should the formulae be? (3 is about right, though feel free to experiment)"
            result <- getLine
            case (readMaybe result) :: Maybe Int  of
                        (Just x) -> do
                                        return x
                        Nothing  -> do
                                        putStrLn "Sorry - that wasn't a valid integral value. Please try again."
                                        getComplexity

getOutputFile = do
            putStrLn "Where should I write the file to? (if it exists I'll overwrite it)"
            getLine

main = do
      args <- getArgs
      case args of
          [count,complexity,output]   ->  writeFile output $ prettify $ fst (genFormulas countValue complexityValue (RndGen 3))
                                                        where countValue      = read (count) :: Int 
                                                              complexityValue = read (complexity) :: Int
          [count,complexity,output, seed]   ->  writeFile output $ prettify $ fst (genFormulas countValue complexityValue (RndGen $ toInteger seedValue))
                                                        where countValue      = read (count) :: Int 
                                                              complexityValue = read (complexity) :: Int
                                                              seedValue       = read (seed) :: Int
          _                -> do
                                 printIntro
                                 countValue <- getCount
                                 complexityValue <- getComplexity
                                 output <- getOutputFile
                                 writeFile output $ prettify $ fst (genFormulas countValue complexityValue (RndGen 3))
                                


