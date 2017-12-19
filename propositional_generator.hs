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



-- growBranch :: Int -> Valuation -> (RndGen, Formula) -> (RndGen, Formula)
growBranch 0 _ res = res
growBranch n valuation res = growBranch (n-1) valuation (grow valuation res) 

result = grow (Valuation True True True) ((Literal P), (RndGen 1))


