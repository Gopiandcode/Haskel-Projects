module C_Parser where

import Data.Char
import Data.List
import Control.Applicative
type Error = String
type Line_No = Int
type Column_No = Int

class Pretty_Print a where
    stringify :: Int -> a -> String

n_spaces :: Int -> String
n_spaces 0 = []
n_spaces n = ' ' : n_spaces (n-1)

newtype Parser a = P {parse :: (String -> (Either Error a, String))}


instance Functor Parser where
    fmap f (P parser) = P $ \string -> case parser string of
                    (Right a,  string1)        -> (Right $ f a, string1)
                    (Left err, string1)        -> (Left err , string)

instance Applicative Parser where
    pure a  = P $ \stream0   -> (Right a, stream0)
    (P f) <*> (P a) = P $ \stream0 -> case f stream0 of
                (Left err, streamerr)          -> (Left err, stream0)
                (Right f, stream1)             -> case a stream1 of
                            (Left err, streamerr)   -> (Left err, stream0)
                            (Right a, stream2)      -> (Right $ f a, stream2)

instance Alternative Parser where
    empty = P $ \stream0 -> (Left "Empty Stream", stream0)
    (P a) <|> (P b) = P $ \stream0   -> case a stream0 of
                    (Right a, stream1)       -> (Right a, stream1)
                    (Left err, streamerr)    -> case b stream0 of
                                (Left err, streamerrr)      -> (Left err, stream0)
                                (Right b, stream2)          -> (Right b, stream2) 

instance Monad Parser where
    (P a) >>= f = P $ \stream0 -> case a stream0 of
                (Left err, streamerrr)      -> (Left err, stream0)
                (Right a , stream1)         -> parse (f a) stream1

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = P $ \string -> case string of
                    (c:cs) | p c        -> (Right c, cs)
                           | otherwise  -> (Left "did not satisfy", string)
                    []                  -> (Left "empty string", string)
char :: Char -> Parser Char
char c = satisfy (==c)

string :: String -> Parser String
string (c:cs) = P $ \stream0 -> case stream0 of
                    (d:ds) | d == c     -> case parse (string cs) ds of
                                    (Right xs, stream0)      -> (Right (c:xs), stream0)
                                    (Left err, streamerr)    -> (Left err  , stream0)    
                           | otherwise  -> (Left "did not satisfy", stream0)
                    []                  -> (Left "empty string", stream0)
string []     = return []


whileParse :: Parser a -> Parser [a]
whileParse (P a) = P $ \stream0 -> case a stream0 of
                    (Left err, streamerr)  -> (Right [], stream0)
                    (Right x, stream1)     -> case parse (whileParse (P a)) stream1 of
                            (Right xs, stream2)        -> (Right (x:xs), stream2)
                            (Left err, streamerr)      -> (Left err, stream0)

whileParse1 :: Parser a -> Parser [a]
whileParse1 (P a) = P $ \stream0 -> case a stream0 of
                    (Left err, streamerr)  -> (Left err, stream0)
                    (Right x, stream1)     -> case parse (whileParse (P a)) stream1 of
                            (Right xs, stream2)        -> (Right (x:xs), stream2)
                            (Left err, streamerr)      -> (Left err, stream0)


internal_untilParse :: Int -> String -> Maybe (String, String)
internal_untilParse 0  rest    = Just ([], rest)
internal_untilParse n (c:cs) = case internal_untilParse (n-1) cs of
                            (Just (xs,rest))    -> Just (c:xs, rest)
                            (Nothing)           -> Nothing
internal_untilParse n   []   = Nothing


untilParse :: Int -> Parser a -> Parser String
-- retrieves n characters, tests whether the parser matches on those n characters
untilParse n (P a) = P $ \string -> case internal_untilParse n string of
                                    Nothing           -> (Left "end of string", string)
                                    (Just (xs,rest))  -> case a xs of
                                            (Right a, string1)    -> (Right [], string1 ++ rest)
                                            (Left err,string2)    -> case (parse (untilParse n (P a)) ((tail xs) ++ rest)) of
                                                        (Left err, string3)    -> (Left err, string)
                                                        (Right xss, string1)    -> (Right ((head xs) : xss), string1)

try :: Parser a -> Parser (Maybe a)
try (P a) = P $ \string -> case a string of
                (Left err, rest)    -> (Right Nothing, string)
                (Right x, rest)     -> (Right(Just x), rest  )

comment_block :: Parser String
comment_block = (string "/*" >> (untilParse 2 (string "*/")))

comment_line :: Parser String
comment_line = (string "//" >> (untilParse 1 (string "\n")))

macro :: Parser String
macro = (string "#" >> (untilParse 1 (string "\n")))

whiteSpace = (many (comment_block <|> comment_line <|> macro <|> whileParse1 (satisfy (`elem` "\n\t "))))

dropWhiteSpace :: Parser a -> Parser a
dropWhiteSpace p = whiteSpace >> p

keyword :: String -> Parser String
keyword s = dropWhiteSpace (string s)

operator :: String -> Parser String
operator s = dropWhiteSpace (string s)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy (P a) (P b) = P $ \string -> case a string of
                        (Left err, rest) -> (Right [], string)
                        (Right kp,  rest) -> case b rest of
                                (Left err, more) -> (Right [kp], rest)
                                (Right dr, more)  -> case (parse (sepBy (P a) (P b)) more) of
                                                (Left err, rem) -> (Right [kp], more)
                                                (Right rs, rem) -> (Right (kp:rs), rem)

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 (P a) (P b) = P $ \string -> case a string of
                        (Left err, rest) -> (Left err, string)
                        (Right kp,  rest) -> case b rest of
                                (Left err, more) -> (Right [kp], rest)
                                (Right dr, more)  -> case (parse (sepBy (P a) (P b)) more) of
                                                (Left err, rem) -> (Right [kp], more)
                                                (Right rs, rem) -> (Right (kp:rs), rem)

parens :: Parser a -> Parser a
parens p = ((dropWhiteSpace (char '(')) >> p >>= \p -> (dropWhiteSpace (char ')')) >> (return p)) 

brackets :: Parser a -> Parser a
brackets p = ((dropWhiteSpace (char '[')) >> p >>= \p -> (dropWhiteSpace (char ']')) >> (return p)) 

braces :: Parser a -> Parser a
braces p = ((dropWhiteSpace (char '{')) >> p >>= \p -> (dropWhiteSpace (char '}')) >> (return p)) 






escape :: Parser String
escape = do
    d <- char '\\'
    c <- (satisfy (`elem` "\\\"0nrvtbf"))
    return [d,c]

nonEscape :: Parser Char
nonEscape = satisfy (not . (`elem` "\\\"\0\n\r\v\t\b\f"))

character :: Parser String
character = fmap return nonEscape <|> escape

c_string :: Parser String
c_string = do
    char '"'
    strings <- many character
    char '"'
    return $ concat strings

identifier :: Parser String
identifier = dropWhiteSpace $ P $ \stream0 -> case stream0 of
                []          -> (Left "empty string", [])
                (c:cs)      | isAlpha c    -> case (parse (many (satisfy isAlphaNum)) cs) of
                                            (Left err, rest)    -> (Right [c], rest)
                                            (Right vs, rest)    -> (Right (c:vs), rest)
                            | otherwise    -> (Left "did not satisfy", stream0)

digit_sequence :: Parser String
digit_sequence = (some (satisfy isDigit))
                        
integer_constant :: Parser String
integer_constant = dropWhiteSpace $ P $ \stream0 -> case stream0 of
                []          -> (Left "empty string", [])
                (c:cs)      | isDigit c && c == '0'    -> case cs of
                                                        []    -> (Right [c], cs)
                                                        (d:ds) | d =='b' || d == 'x' || isDigit d -> case (parse (many (satisfy isDigit)) ds) of
                                                                                        (Left err, rest)     -> (Right [c], ds)
                                                                                        (Right rs, rest)     -> (Right (c:d:rs), rest)
                                                               | otherwise                        -> (Right [c], cs)
                            | isDigit c                -> case (parse (many (satisfy isDigit)) cs) of
                                                            (Left err, rest)     -> (Right [c], cs)
                                                            (Right rs, rest)     -> (Right (c:rs), rest)
                            | otherwise                -> (Left "did not satisfy", stream0)

fractional_constant :: Parser String
fractional_constant = dropWhiteSpace $ P $ \stream0 -> case (parse (many (satisfy isDigit)) stream0) of
                (Left err, rest)    -> (Left err, stream0)
                (Right xs, rest)    -> case rest of
                        []        -> (Left "did not satisfy", stream0)
                        (c:cs)      | '.' /= c    -> (Left "did not satisfy", stream0)
                                    | otherwise   -> case (parse (many (satisfy isDigit)) cs) of
                                                (Left err, rest)  -> (Right (xs ++ ['.']), cs)
                                                (Right zs, rest)  -> (Right (xs ++ '.':zs), rest)

exponent_part :: Parser String
exponent_part = (char 'e' <|> char 'E') >>= \e -> try (char '+' <|> char '-') >>= (\x -> case x of
                                                                                        Nothing         -> return [e]
                                                                                        (Just q)        -> return (e:q:[])) >>= \d -> digit_sequence >>= \r -> return (d ++ r)
fractional_point_constant :: Parser String
fractional_point_constant = (fractional_constant >>= \e -> try (exponent_part) >>= (\x -> case x of
                                                                                    Nothing     -> return (e)
                                                                                    (Just x)    -> return (e ++ x) ) >>= \res -> try (satisfy (`elem` "LfF")) >>= (\y -> case y of
                                                                                                                                                                    Nothing    -> return res
                                                                                                                                                                    (Just p)   -> return (res ++ [p])))

digital_point_constant :: Parser String      
digital_point_constant = digit_sequence >>= \d -> exponent_part >>= \e -> try (satisfy (`elem` "LfF")) >>= (\l -> case l of
                                                                                                            Nothing      -> return (d ++ e)
                                                                                                            (Just l)     -> return (d ++ e ++ [l]))
floating_point_constant :: Parser String
floating_point_constant = dropWhiteSpace (digital_point_constant <|> fractional_point_constant)

enumeration_constant :: Parser String
enumeration_constant = dropWhiteSpace identifier

character_constant :: Parser String
character_constant = (dropWhiteSpace (char '\'')) >>= \d -> satisfy (\x -> True) >>= \e -> char '\'' >>= \p -> return (d:e:p:[])

-- Primary_Expression Definitions
---------------------------------------
data Primary_Expression = Primary_Identifier String 
                        | Primary_Constant Constant
                        | Primary_String String
                        | Primary_Expression Expression
                        deriving Show
instance Pretty_Print Primary_Expression where
    stringify n (Primary_Constant c)   = (n_spaces n) ++ (stringify 0 c) 
    stringify n (Primary_String s)     = (n_spaces n) ++ s
    stringify n (Primary_Expression e) = (n_spaces n) ++ "(" ++ (stringify 0 e) ++ ")"
    stringify n (Primary_Identifier i) = (n_spaces n) ++ i

primary_expression = (constant >>= \c -> return $ Primary_Constant c)
                 <|> (c_string >>= \s -> return $ Primary_String s)
                 <|> (parens expression >>= \e -> return $ Primary_Expression e)
                 <|> (identifier >>= \i -> return $ Primary_Identifier i)



-- Constant Definitions
---------------------------------------
data Constant = Integer_Constant String
              | Character_Constant String
              | Floating_Constant String
              | Enumeration_Constant String
              deriving Show

instance Pretty_Print Constant where
    stringify n (Integer_Constant s) = (n_spaces n)     ++ s
    stringify n (Character_Constant s) = (n_spaces n)   ++ s
    stringify n (Floating_Constant s) = (n_spaces n)    ++ s
    stringify n (Enumeration_Constant s) = (n_spaces n) ++ s        

constant = (floating_point_constant >>= \f -> return $ Floating_Constant f) 
       <|> (character_constant      >>= \c -> return $ Character_Constant c)
       <|> (integer_constant        >>= \i -> return $ Integer_Constant i)
       <|> (enumeration_constant    >>= \e -> return $ Enumeration_Constant e)



-- Storage Class Specifier Definitions
----------------------------------------
data Storage_Class_Specifier = Auto
                             | Register
                             | Static
                             | Extern
                             | Typedef
                             deriving Show

instance Pretty_Print Storage_Class_Specifier where
    stringify n Auto     = (n_spaces n) ++ "auto"
    stringify n Register = (n_spaces n) ++ "register"
    stringify n Static   = (n_spaces n) ++ "static"
    stringify n Extern   = (n_spaces n) ++ "extern"
    stringify n typedef  = (n_spaces n) ++ "typedef"

storage_class_specifier = (keyword "auto"     >> (return $ Auto))
                      <|> (keyword "register" >> (return $ Register))
                      <|> (keyword "static"   >> (return $ Static))
                      <|> (keyword "extern"   >> (return $ Extern))
                      <|> (keyword "typedef"  >> (return $ Typedef))



-- Type Specifier Definitions
----------------------------------------
data Type_Specifier = Type_Void
                    | Type_Short
                    | Type_Char
                    | Type_Int
                    | Type_Long
                    | Type_Float
                    | Type_Double
                    | Type_Signed
                    | Type_Unsigned
                    | Type_Struct Struct_Specifier
                    | Type_Union Union_Specifier
                    | Type_Enum Enum_Specifier
                    | Type_Typedef Typedef_Name
                    deriving Show

instance Pretty_Print Type_Specifier where
    stringify n Type_Void       = (n_spaces n) ++ "void"
    stringify n Type_Short      = (n_spaces n) ++ "short" 
    stringify n Type_Char       = (n_spaces n) ++ "char"
    stringify n Type_Int        = (n_spaces n) ++ "int"
    stringify n Type_Long       = (n_spaces n) ++ "long"
    stringify n Type_Float      = (n_spaces n) ++ "float"
    stringify n Type_Double     = (n_spaces n) ++ "double"
    stringify n Type_Signed     = (n_spaces n) ++ "signed"
    stringify n Type_Unsigned   = (n_spaces n) ++ "unsigned"
    stringify n (Type_Struct s) = (n_spaces n) ++ (stringify 0 s)
    stringify n (Type_Union s) = (n_spaces n) ++ (stringify 0 s)
    stringify n (Type_Enum s)   = (n_spaces n) ++ (stringify 0 s)
    stringify n (Type_Typedef s)= (n_spaces n) ++ (stringify 0 s)

type_specifier :: Parser Type_Specifier
type_specifier = (keyword "void"     >> (return $ Type_Void))
             <|> (keyword "short"    >> (return $ Type_Short))
             <|> (keyword "char"     >> (return $ Type_Char))
             <|> (keyword "int"      >> (return $ Type_Int))
             <|> (keyword "long"     >> (return $ Type_Long))
             <|> (keyword "float"    >> (return $ Type_Float))
             <|> (keyword "double"   >> (return $ Type_Double))
             <|> (keyword "signed"   >> (return $ Type_Signed))
             <|> (keyword "unsigned"    >> (return $ Type_Unsigned))
             <|> (struct_specifier >>= \s -> return $ Type_Struct  s)
             <|> (union_specifier  >>= \s -> return $ Type_Union   s)
             <|> (enum_specifier   >>= \s -> return $ Type_Enum    s)
             


-- Constant Expression Definition
----------------------------------------
data Constant_Expression = Constant_Conditional Conditional_Expression deriving Show

instance Pretty_Print Constant_Expression where
    stringify n (Constant_Conditional s) = (n_spaces n) ++ (stringify 0 s)

constant_expression = (conditional_expression >>= \e -> return $ Constant_Conditional e)




-- Conditional Expression Definition
----------------------------------------
data Conditional_Expression = Conditional_Logical Logical_Or_Expression
                            | Conditional_Ternary Logical_Or_Expression Expression Conditional_Expression
                            deriving Show

instance Pretty_Print Conditional_Expression where
    stringify n (Conditional_Logical e) = (n_spaces n) ++ (stringify 0 e)
    stringify n (Conditional_Ternary c e1 e2) = (n_spaces n) ++ (stringify 0 c) ++ "?" ++ (stringify 0 e1) ++ ":" ++ (stringify 0 e2)

conditional_expression :: Parser Conditional_Expression
conditional_expression = (logical_or_expression >>= \c -> operator "?" >> expression >>= \e1 -> operator ":" >> conditional_expression >>= \e2 -> return $ Conditional_Ternary c e1 e2)
                     <|> (logical_or_expression >>= \c -> return $ Conditional_Logical c)




-- Logical Or Expression Definition
----------------------------------------
data Logical_Or_Expression = Logical_Or_And Logical_And_Expression
                           | Logical_Or_Or Logical_Or_Expression Logical_And_Expression
                           deriving Show

instance Pretty_Print Logical_Or_Expression where
    stringify n (Logical_Or_And c) = (n_spaces n) ++ (stringify 0 c)
    stringify n (Logical_Or_Or a e) = (n_spaces n) ++ (stringify 0 a) ++ "||" ++ (stringify 0 e)

logical_or_expression :: Parser Logical_Or_Expression
logical_or_expression = dropWhiteSpace (sepBy1 logical_and_expression (operator "||")) >>= (\s -> case s of
                                                                                                    (x:[])       -> return $ Logical_Or_And x
                                                                                                    (x:xs)       -> return $ foldl Logical_Or_Or (Logical_Or_And x) xs)



-- Logical And Expression Definiton
----------------------------------------
data Logical_And_Expression = Logical_And_Or_Inclusive Inclusive_Or_Expression
                            | Logical_And_And Logical_And_Expression Inclusive_Or_Expression
                            deriving Show

instance Pretty_Print Logical_And_Expression where
    stringify n (Logical_And_Or_Inclusive c) = (n_spaces n) ++ (stringify 0 c)
    stringify n (Logical_And_And a e) = (n_spaces n) ++ (stringify 0 a) ++ "&&" ++ (stringify 0 e)

logical_and_expression :: Parser Logical_And_Expression
logical_and_expression = dropWhiteSpace (sepBy1 inclusive_or_expression (operator "&&")) >>= (\s -> case s of
                                                                                                    (x:[])       -> return $ Logical_And_Or_Inclusive x
                                                                                                    (x:xs)       -> return $ foldl Logical_And_And (Logical_And_Or_Inclusive x) xs)



-- Inclusive Or Expression Definiton
----------------------------------------
data Inclusive_Or_Expression = Inclusive_Or_Exclusive Exclusive_Or_Expression
                            | Inclusive_Or_Or Inclusive_Or_Expression Exclusive_Or_Expression
                            deriving Show

instance Pretty_Print Inclusive_Or_Expression where
    stringify n (Inclusive_Or_Exclusive c) = (n_spaces n) ++ (stringify 0 c)
    stringify n (Inclusive_Or_Or a e) = (n_spaces n) ++ (stringify 0 a) ++ "|" ++ (stringify 0 e)

inclusive_or_expression :: Parser Inclusive_Or_Expression
inclusive_or_expression = dropWhiteSpace (sepBy1 exclusive_or_expression (operator "|")) >>= (\s -> case s of
                                                                                                    (x:[])       -> return $ Inclusive_Or_Exclusive x
                                                                                                    (x:xs)       -> return $ foldl Inclusive_Or_Or (Inclusive_Or_Exclusive x) xs)



-- Exclusive Or Expression Definiton
----------------------------------------
data Exclusive_Or_Expression = Enclusive_Or_And And_Expression
                            | Enclusive_Or_Or Exclusive_Or_Expression And_Expression
                            deriving Show

instance Pretty_Print Exclusive_Or_Expression where
    stringify n (Enclusive_Or_And c) = (n_spaces n) ++ (stringify 0 c)
    stringify n (Enclusive_Or_Or a e) = (n_spaces n) ++ (stringify 0 a) ++ "^" ++ (stringify 0 e)

exclusive_or_expression :: Parser Exclusive_Or_Expression
exclusive_or_expression = dropWhiteSpace (sepBy1 and_expression (operator "^")) >>= (\s -> case s of
                                                                                                    (x:[])       -> return $ Enclusive_Or_And x
                                                                                                    (x:xs)       -> return $ foldl Enclusive_Or_Or (Enclusive_Or_And x) xs)




-- And Expression Definiton
----------------------------------------
data And_Expression = And_Equality Equality_Expression
                    | And_And And_Expression Equality_Expression
                    deriving Show

instance Pretty_Print And_Expression where
    stringify n (And_Equality c) = (n_spaces n) ++ (stringify 0 c)
    stringify n (And_And a e) = (n_spaces n) ++ (stringify 0 a) ++ "&" ++ (stringify 0 e)

and_expression :: Parser And_Expression
and_expression = dropWhiteSpace (sepBy1 equality_expression (operator "&")) >>= (\s -> case s of
                                                                                                    (x:[])       -> return $ And_Equality x
                                                                                                    (x:xs)       -> return $ foldl And_And (And_Equality x) xs)





-- Equality Expression Definiton
----------------------------------------
data Equality_Expression = Equality_Relational Relational_Expression
                         | Equality_Equality Equality_Expression Relational_Expression
                         | Equality_Inequality Equality_Expression Relational_Expression
                         deriving Show

instance Pretty_Print Equality_Expression where
    stringify n (Equality_Relational a)   = (n_spaces n) ++ (stringify 0 a)
    stringify n (Equality_Equality a b)   = (n_spaces n) ++ (stringify 0 a) ++ "==" ++ (stringify 0 b)
    stringify n (Equality_Inequality a b) = (n_spaces n) ++ (stringify 0 a) ++ "!=" ++ (stringify 0 b)


m_relational = (relational_expression >>= \r1 -> try (operator "==" <|> operator "!=") >>= \m -> case m of
                                                                                                    Nothing     -> return [("",r1)]
                                                                                                    (Just val)  -> m_relational >>= \m -> return $ (val, r1):m)
equality_expression = (m_relational >>= \ms -> case ms of
                                                ((v,r):[])         -> return $ (Equality_Relational r) 
                                                ((v,r):ms)         -> return $ foldl (\acc (v,r) -> if v == "==" then Equality_Equality acc r else if v == "!=" then Equality_Inequality acc r else error "error in equality_expression") (Equality_Relational r)  (tail $ (\(xs,ys) -> zip ("":xs) (ys)) (unzip ((v,r):ms))  ))




-- Relational Expression
----------------------------------------
data Relational_Expression = Relational_Shift Shift_Expression
                           | Relational_Lt Relational_Expression  Shift_Expression
                           | Relational_Gt Relational_Expression  Shift_Expression
                           | Relational_Lte Relational_Expression Shift_Expression
                           | Relational_Gte Relational_Expression Shift_Expression
                           deriving Show

instance Pretty_Print Relational_Expression where
        stringify n (Relational_Shift a   ) = (n_spaces n) ++ (stringify 0 a)
        stringify n (Relational_Lt    a  b) = (n_spaces n) ++ (stringify 0 a) ++ "<" ++ (stringify 0 b)
        stringify n (Relational_Gt    a  b) = (n_spaces n) ++ (stringify 0 a) ++ ">" ++ (stringify 0 b)
        stringify n (Relational_Lte   a  b) = (n_spaces n) ++ (stringify 0 a) ++ "<="++ (stringify 0 b)
        stringify n (Relational_Gte   a  b) = (n_spaces n) ++ (stringify 0 a) ++ ">="++ (stringify 0 b) 

m_equality = (shift_expression >>= \r1 -> try (operator "<=" <|>  operator ">=" <|> operator "<" <|> operator ">" ) >>= \m -> case m of
                                                                                                    Nothing     -> return [("",r1)]
                                                                                                    (Just val)  -> m_equality >>= \m -> return $ (val, r1):m)

relational_expression = (m_equality >>= \ms -> case ms of
                                                ((v,r):[])         -> return $ (Relational_Shift r) 
                                                ((v,r):ms)         -> return $ foldl (\acc (v,r) -> case v of
                                                                                                    v | v == "<"    -> Relational_Lt  acc r
                                                                                                      | v == ">"    -> Relational_Gt  acc r
                                                                                                      | v == "<="   -> Relational_Lte acc r
                                                                                                      | v == ">="   -> Relational_Gte acc r
                                                                                                      | otherwise   -> error $ v ++ " does not match relational operators") (Relational_Shift r)  (tail $ (\(xs,ys) -> zip ("":xs) (ys)) (unzip ((v,r):ms))  ))





-- Shift Expression Definiton
----------------------------------------
data Shift_Expression = Shift_Additive  Additive_Expression
                      | Shift_Left  Shift_Expression Additive_Expression
                      | Shift_Right Shift_Expression Additive_Expression
                      deriving Show

instance Pretty_Print Shift_Expression where
    stringify n (Shift_Additive a  ) = (n_spaces n) ++ (stringify 0 a) 
    stringify n (Shift_Left     a b) = (n_spaces n) ++ (stringify 0 a) ++ "<<" ++ (stringify 0 b)
    stringify n (Shift_Right    a b) = (n_spaces n) ++ (stringify 0 a) ++ ">>" ++ (stringify 0 b)



m_shift = (additive_expression >>= \r1 -> try (operator "<<" <|> operator ">>") >>= \m -> case m of
                                                                                                    Nothing     -> return [("",r1)]
                                                                                                    (Just val)  -> m_shift >>= \m -> return $ (val, r1):m)

shift_expression = (m_shift >>= \ms -> case ms of
                                           ((v,r):[])         -> return $ (Shift_Additive r) 
                                           ((v,r):ms)         -> return $ foldl (\acc (v,r) -> case v of
                                                                                                  v | v == "<<"    -> Shift_Left  acc r
                                                                                                    | v == ">>"    -> Shift_Right  acc r
                                                                                                    | otherwise    -> error $ v ++ " does not match shift operators") (Shift_Additive r)  (tail $ (\(xs,ys) -> zip ("":xs) (ys)) (unzip ((v,r):ms))  ))





-- Additive Expression Definiton
----------------------------------------
data Additive_Expression = Additive_Multiplicative                       Multiplicative_Expression
                         | Additive_Add              Additive_Expression Multiplicative_Expression
                         | Additive_Subtract         Additive_Expression Multiplicative_Expression
                         deriving Show


instance Pretty_Print Additive_Expression where
    stringify n (Additive_Multiplicative a)   = (n_spaces n) ++ (stringify 0 a)
    stringify n (Additive_Add            a b) = (n_spaces n) ++ (stringify 0 a) ++ "+" ++ (stringify 0 b)
    stringify n (Additive_Subtract       a b) = (n_spaces n) ++ (stringify 0 a) ++ "-" ++ (stringify 0 b)


m_multiplicative = (multiplicative_expression >>= \r1 -> try (operator "+" <|> operator "-") >>= \m -> case m of
                                                                                                    Nothing     -> return [("",r1)]
                                                                                                    (Just val)  -> m_multiplicative >>= \m -> return $ (val, r1):m)

additive_expression = (m_multiplicative >>= \ms -> case ms of
                                           ((v,r):[])         -> return $ (Additive_Multiplicative r) 
                                           ((v,r):ms)         -> return $ foldl (\acc (v,r) -> case v of
                                                                                                  v | v == "+"    -> Additive_Add       acc r
                                                                                                    | v == "-"    -> Additive_Subtract  acc r
                                                                                                    | otherwise    -> error $ v ++ " does not match shift operators") (Additive_Multiplicative r)  (tail $ (\(xs,ys) -> zip ("":xs) (ys)) (unzip ((v,r):ms))  ))



-- Multiplicative Expression Definiton
----------------------------------------
data Multiplicative_Expression = Multiplicative_Cast                                Cast_Expression
                               | Multiplicative_Multiply  Multiplicative_Expression Cast_Expression
                               | Multiplicative_Divide    Multiplicative_Expression Cast_Expression
                               | Multiplicative_Modulo    Multiplicative_Expression Cast_Expression
                               deriving Show

instance Pretty_Print Multiplicative_Expression where
           stringify n (Multiplicative_Cast      a  ) = (n_spaces n) ++ (stringify 0 a)
           stringify n (Multiplicative_Multiply  a b) = (n_spaces n) ++ (stringify 0 a) ++ "*" ++ (stringify 0 b)
           stringify n (Multiplicative_Divide    a b) = (n_spaces n) ++ (stringify 0 a) ++ "/" ++ (stringify 0 b) 
           stringify n (Multiplicative_Modulo    a b) = (n_spaces n) ++ (stringify 0 a) ++ "%" ++ (stringify 0 b)


m_cast = (cast_expression >>= \r1 -> try (operator "*" <|> operator "/" <|> operator "%") >>= \m -> case m of
                                                                                                    Nothing     -> return [("",r1)]
                                                                                                    (Just val)  -> m_cast >>= \m -> return $ (val, r1):m)

multiplicative_expression = (m_cast >>= \ms -> case ms of
                                           ((v,r):[])         -> return $ (Multiplicative_Cast r) 
                                           ((v,r):ms)         -> return $ foldl (\acc (v,r) -> case v of
                                                                                                  v | v == "*"     -> Multiplicative_Multiply acc r
                                                                                                    | v == "/"     -> Multiplicative_Divide   acc r
                                                                                                    | v == "%"     -> Multiplicative_Modulo   acc r
                                                                                                    | otherwise    -> error $ v ++ " does not match shift operators") (Multiplicative_Cast r)  (tail $ (\(xs,ys) -> zip ("":xs) (ys)) (unzip ((v,r):ms))  ))



-- Cast Expression Definiton
----------------------------------------
data Cast_Expression = Cast_Unary Unary_Expression
                     | Cast_Cast Type_Name Cast_Expression
                     deriving Show

instance Pretty_Print Cast_Expression where
    stringify n (Cast_Unary a) = (n_spaces n) ++ (stringify 0 a)
    stringify n (Cast_Cast t a)= (n_spaces n) ++ "(" ++ (stringify 0 t) ++ ")" ++ (stringify 0 a)

cast_expression = (dropWhiteSpace (parens type_name) >>= \t -> cast_expression >>= \c -> return $ Cast_Cast t c)
              <|> (unary_expression >>= \u -> return $ Cast_Unary u)







-- Unary Expression Definiton
----------------------------------------
data Unary_Expression = Unary_Postfix Postfix_Expression
                      | Unary_Increment Unary_Expression
                      | Unary_Decrement Unary_Expression
                      | Unary_Unary_Sizeof Unary_Expression
                      | Unary_Type_Sizeof Type_Name
                      | Unary_Cast Unary_Operator Cast_Expression
                      deriving Show

instance Pretty_Print Unary_Expression where
      stringify n (Unary_Postfix      a  ) = (n_spaces n) ++ (stringify 0 a)
      stringify n (Unary_Increment    a  ) = (n_spaces n) ++ "++" ++ (stringify 0 a) 
      stringify n (Unary_Decrement    a  ) = (n_spaces n) ++ "--" ++ (stringify 0 a)
      stringify n (Unary_Unary_Sizeof a  ) = (n_spaces n) ++ "sizeof" ++ (stringify 0 a) 
      stringify n (Unary_Type_Sizeof  a  ) = (n_spaces n) ++ "sizeof" ++ (stringify 0 a)
      stringify n (Unary_Cast         a b) = (n_spaces n) ++ (stringify 0 a) ++ " " ++ (stringify 0 b)

unary_expression = (operator "++" >> unary_expression          >>= \e -> return $ Unary_Increment e)
               <|> (operator "--" >> unary_expression          >>= \e -> return $ Unary_Decrement e)
               <|> (keyword "sizeof" >> unary_expression       >>= \e -> return $ Unary_Unary_Sizeof e)
               <|> (keyword "sizeof" >> type_name              >>= \e -> return $ Unary_Type_Sizeof e)
               <|> (unary_operator >>= \op -> cast_expression  >>= \c -> return $ Unary_Cast op c)
               <|> (postfix_expression                         >>= \p -> return $ Unary_Postfix p)





-- Postfix Expression Definition
----------------------------------------
data Postfix_Expression = Postfix_Primary   Primary_Expression
                        | Postfix_Array     Postfix_Expression Expression
                        | Postfix_Function  Postfix_Expression [Assignment_Expression]
                        | Postfix_Dot       Postfix_Expression String
                        | Postfix_Arrow     Postfix_Expression String
                        | Postfix_Increment Postfix_Expression
                        | Postfix_Decrement Postfix_Expression
                        deriving Show

instance Pretty_Print Postfix_Expression where
        stringify n (Postfix_Primary   a  ) = (n_spaces n) ++ (stringify 0 a) 
        stringify n (Postfix_Array     a b) = (n_spaces n) ++ (stringify 0 a) ++ "[" ++ (stringify 0 b) ++ "]"
        stringify n (Postfix_Function  a b) = (n_spaces n) ++ (stringify 0 a) ++ "(" ++ (concat . intersperse ", ". fmap (stringify 0) $ b) ++ ")"
        stringify n (Postfix_Dot       a b) = (n_spaces n) ++ (stringify 0 a) ++ "." ++  b
        stringify n (Postfix_Arrow     a b) = (n_spaces n) ++ (stringify 0 a) ++ "->" ++ b
        stringify n (Postfix_Increment a  ) = (n_spaces n) ++ (stringify 0 a) ++ "++"
        stringify n (Postfix_Decrement a  ) = (n_spaces n) ++ (stringify 0 a) ++ "--"

data Temp_Postfix_Expression = St String
                             | Er Expression
                             | Ae [Assignment_Expression]

extSt (St i) = i
extEr (Er e) = e
extAe (Ae a) = a

m_assignments = ((try ((operator "."     >>= \d -> identifier   >>= \i -> return $ (d,St i))
                     <|> (operator "->" >>= \d -> identifier   >>= \i -> return $ (d,St i))
                     <|> (operator "++"  >>= \d -> identifier   >>= \i -> return $ (d  ,St ""))
                     <|> (operator "--"  >>= \d -> identifier   >>= \i -> return $ (d  ,St ""))
                     <|> (brackets expression                   >>= \e -> return $ ("e",Er  e))
                     <|> (parens   (sepBy assignment_expression (operator ", ")) >>= \e -> return $ ("a",Ae  e)))) >>= \m -> case m of
                                                                          Nothing     -> return []
                                                                          (Just val)  -> m_assignments >>= \more -> return $ val:more)

postfix_expression = (primary_expression >>= \p -> m_assignments >>= \ms -> case ms of
                                           ([])         -> return $ (Postfix_Primary p) 
                                           (ms)         -> return $ foldl (\acc (id,v) -> case id of
                                                                                    id | id == "++"   -> Postfix_Increment acc
                                                                                       | id == "--"   -> Postfix_Decrement acc
                                                                                       | id == "."    -> Postfix_Dot acc (extSt v)
                                                                                       | id == "->"   -> Postfix_Arrow acc (extSt v)
                                                                                       | id == "e"    -> Postfix_Array acc (extEr v)
                                                                                       | id == "a"    -> Postfix_Function acc (extAe v)) (Postfix_Primary p) ms)




-- Expression Definition
----------------------------------------
data Expression = Expression_Assignment Assignment_Expression
                | Expression_Comma_Sep Expression Assignment_Expression
                deriving Show

instance Pretty_Print Expression where
    stringify n (Expression_Assignment a)  = (n_spaces 0) ++ (stringify 0 a)
    stringify n (Expression_Comma_Sep e a) = (n_spaces 0) ++ (stringify 0 e) ++ "," ++ (stringify 0 a)


expression = sepBy1 assignment_expression (operator ",") >>= \es -> case es of
                                                                    (x:[])     -> return $ (Expression_Assignment x)
                                                                    (x:xs)       -> return $ (foldl (Expression_Comma_Sep) (Expression_Assignment x) xs)




-- Assignmet Expression Definition
----------------------------------------
data Assignment_Expression = Assignment_Conditional Conditional_Expression
                          | Assignment_Assignment Unary_Expression Assignment_Operator Assignment_Expression
                          deriving Show

instance Pretty_Print Assignment_Expression where
    stringify n (Assignment_Conditional e)    = (n_spaces n) ++ (stringify 0 e)
    stringify n (Assignment_Assignment u o e) = (n_spaces n) ++ (stringify 0 u) ++ (stringify 0 o) ++ (stringify 0 e)

assignment_expression = (unary_expression >>= \u -> assignment_operator >>= \o -> assignment_expression >>= \e -> return $ Assignment_Assignment u o e)
                    <|> (conditional_expression >>= \c -> return $ Assignment_Conditional c)





-- Assignment Operator Definition
----------------------------------------
data Assignment_Operator = Assignment
                         | Mult_Assignment
                         | Div_Assignment
                         | Mod_Assignment
                         | Add_Assignment
                         | Sub_Assignment
                         | LShift_Assignment
                         | RShift_Assignment
                         | Bin_And_Assignment
                         | Bin_Xor_Assignment
                         | Bin_Or_Assignment
                         deriving Show

instance Pretty_Print Assignment_Operator where
        stringify n Assignment                = (n_spaces n) ++ "="
        stringify n Mult_Assignment           = (n_spaces n) ++ "*="
        stringify n Div_Assignment            = (n_spaces n) ++ "/="
        stringify n Mod_Assignment            = (n_spaces n) ++ "%="
        stringify n Add_Assignment            = (n_spaces n) ++ "+="
        stringify n Sub_Assignment            = (n_spaces n) ++ "-="
        stringify n LShift_Assignment         = (n_spaces n) ++ "<<="
        stringify n RShift_Assignment         = (n_spaces n) ++ ">>="
        stringify n Bin_And_Assignment        = (n_spaces n) ++ "&="
        stringify n Bin_Xor_Assignment        = (n_spaces n) ++ "^="
        stringify n Bin_Or_Assignment         = (n_spaces n) ++ "|="

assignment_operator = (operator "=" >> return Assignment)
                  <|> (operator "*=" >> return Mult_Assignment)
                  <|> (operator "/=" >> return Div_Assignment)
                  <|> (operator "%=" >> return Mod_Assignment)
                  <|> (operator "+=" >> return Add_Assignment)
                  <|> (operator "-=" >> return Sub_Assignment)
                  <|> (operator "<<=" >> return LShift_Assignment)
                  <|> (operator ">>=" >> return RShift_Assignment)
                  <|> (operator "&=" >> return Bin_And_Assignment)
                  <|> (operator "^=" >> return Bin_Xor_Assignment)
                  <|> (operator "|=" >> return Bin_Or_Assignment)





-- Unary Operator Definition
----------------------------------------
data Unary_Operator = Unary_And
                    | Unary_Multiply
                    | Unary_Add
                    | Unary_Subtract
                    | Unary_Invert
                    | Unary_Negate
                    deriving Show

instance Pretty_Print Unary_Operator where
    stringify n (Unary_And)      = (n_spaces n) ++ "&"
    stringify n (Unary_Multiply) = (n_spaces n) ++ "*"
    stringify n (Unary_Add)      = (n_spaces n) ++ "+"
    stringify n (Unary_Subtract) = (n_spaces n) ++ "-"
    stringify n (Unary_Invert)   = (n_spaces n) ++ "~"
    stringify n (Unary_Negate)   = (n_spaces n) ++ "!"

unary_operator = (operator "&" >> (return Unary_And))
             <|> (operator "*" >> (return Unary_Multiply))
             <|> (operator "+" >> (return Unary_Add))
             <|> (operator "-" >> (return Unary_Subtract))
             <|> (operator "~" >> (return Unary_Invert))
             <|> (operator "!" >> (return Unary_Negate))




-- Type Qualifier Definition
----------------------------------------
data Type_Qualifier = Qualifier_Const
                    | Qualifier_Volatile
                    deriving Show

instance Pretty_Print Type_Qualifier where
    stringify n Qualifier_Const = (n_spaces n) ++ "const"
    stringify n Qualifier_Volatile = (n_spaces n) ++ "volatile"

type_qualifier = (keyword "const" >> (return $ Qualifier_Const))
             <|> (keyword "volatile" >> (return $ Qualifier_Volatile))






-- Specifier Qualifier Definition
----------------------------------------
data Specifier_Qualifier = Specifier_Specifier Type_Specifier
                         | Specifier_Qualifier Type_Qualifier
                         deriving Show

instance Pretty_Print Specifier_Qualifier where
    stringify n (Specifier_Specifier t) = (n_spaces n) ++ (stringify 0 t)
    stringify n (Specifier_Qualifier t) = (n_spaces n) ++ (stringify 0 t)

specifier_qualifier = (type_specifier >>= \i -> return $ Specifier_Specifier i)
                  <|> (type_qualifier >>= \i -> return $ Specifier_Qualifier i)



-- Enumerator Definition
----------------------------------------
data Enumerator = Enumerator_Identifier String
                | Enumerator_Assignment String Constant_Expression
                deriving Show

instance Pretty_Print Enumerator where
    stringify n (Enumerator_Identifier i) =  (n_spaces n) ++ i
    stringify n (Enumerator_Assignment i c) = (n_spaces n) ++ i ++ " = " ++ (stringify 0 c)

enumerator = (identifier >>= \i -> operator "=" >> constant_expression >>= \e -> return $ Enumerator_Assignment i e)
         <|> (identifier >>= \i -> return $ Enumerator_Identifier i)





-- Typedef Name definition
----------------------------------------
data Typedef_Name = Typedef_Name String deriving Show

instance Pretty_Print Typedef_Name where
    stringify n (Typedef_Name i) = (n_spaces n) ++ i

typedef_name = (identifier >>= \i -> return $ Typedef_Name i)



-- Pointer Definition
----------------------------------------
data Pointer = Pointer [Type_Qualifier] (Maybe Pointer) deriving Show

instance Pretty_Print Pointer where
    stringify n (Pointer ts Nothing)   = (n_spaces n) ++ "*" ++ " " ++ (concat . intersperse " " . fmap (stringify 0) $ ts)
    stringify n (Pointer ts (Just p)) = (n_spaces n) ++ "*" ++ " " ++ (concat . intersperse " " . fmap (stringify 0) $ ts) ++ " " ++ (stringify 0 p)

pointer = (operator "*" >> many type_qualifier >>= \ts -> try pointer >>= \p -> return $ Pointer ts p)




-- Delcaration Specifier Defintion
----------------------------------------
data Declaration_Specifier = Declaration_Storage_Class Storage_Class_Specifier
                           | Declaration_Type_Specifier Type_Specifier
                           | Declaration_Type_Qualifier Type_Qualifier
                           deriving Show

instance Pretty_Print Declaration_Specifier where
    stringify n (Declaration_Storage_Class c)  = (n_spaces n) ++ (stringify 0 c)
    stringify n (Declaration_Type_Specifier c) = (n_spaces n) ++ (stringify 0 c)
    stringify n (Declaration_Type_Qualifier c) = (n_spaces n) ++ (stringify 0 c)

declaration_specifier = (storage_class_specifier >>= \s -> return $ Declaration_Storage_Class s)
                    <|> (type_specifier          >>= \s -> return $ Declaration_Type_Specifier s)
                    <|> (type_qualifier          >>= \s -> return $ Declaration_Type_Qualifier s)



-- Delcarator Definition
----------------------------------------
data Declarator = Declarator (Maybe Pointer) Direct_Declarator deriving Show

instance Pretty_Print Declarator where
    stringify n (Declarator Nothing d)  = (n_spaces n) ++ " " ++ (stringify 0 d)
    stringify n (Declarator (Just m) d) = (n_spaces n) ++ " " ++ (stringify 0 m) ++ " " ++ (stringify 0 d)

declarator = (try pointer >>= \p -> direct_declarator >>= \d -> return $ Declarator p d)




-- Direct Declarator Definition
----------------------------------------
data Direct_Declarator = Direct_Declarator_Identifier String
                       | Direct_Declarator_Declarator Declarator
                       | Direct_Declarator_Arr Direct_Declarator (Maybe Constant_Expression)
                       | Direct_Declarator_Func_Def Direct_Declarator (Parameter_Type_List)
                       | Direct_Declarator_Func Direct_Declarator [String]
                       deriving Show

instance Pretty_Print Direct_Declarator where
    stringify n (Direct_Declarator_Identifier a)  = (n_spaces n) ++ a
    stringify n (Direct_Declarator_Declarator a)  = (n_spaces n) ++ "(" ++ (stringify 0 a) ++ ")"
    stringify n (Direct_Declarator_Arr a Nothing) = (n_spaces n) ++ (stringify 0 a) ++ "[]"
    stringify n (Direct_Declarator_Arr a (Just b))= (n_spaces n) ++ (stringify 0 a) ++ "[" ++ (stringify 0 b) ++ "]"
    stringify n (Direct_Declarator_Func_Def a b)  = (n_spaces n) ++ (stringify 0 a) ++ "(" ++ (stringify 0 b) ++ ")"
    stringify n (Direct_Declarator_Func a b)      = (n_spaces n) ++ (stringify 0 a) ++ "(" ++ (concat . intersperse ", "  $ b) ++ ")"

data Temp_Direct_Declarator = CE (Maybe Constant_Expression) | DD (Parameter_Type_List) | SS [String] | NA
extCE (CE x) = x
extDD (DD a)   = a
extSS (SS a)   = a

m_declarator = ((try  (  (brackets (try constant_expression)           >>= \d -> return $ ("[]"  ,CE d) )
                     <|> (parens   (parameter_type_list)               >>= \p -> return $ ("f()" ,DD p) )
                     <|> (parens   (sepBy identifier (operator ","))   >>= \i -> return $ ("f(s)",SS i) ))) >>= \m -> case m of
                                                                           Nothing     -> return []
                                                                           (Just val)  -> m_declarator >>= \more -> return $ val:more)
f_declarator = (parens declarator >>= \i -> return $ Direct_Declarator_Declarator i)  <|> (identifier >>= \i -> return $ Direct_Declarator_Identifier i)

direct_declarator = (f_declarator >>= \b -> m_declarator >>= \ms -> case ms of
                                           ([])         -> return $ (b) 
                                           (ms)         -> return $ foldl (\acc (id,v) -> case id of
                                                                                    id | id == "[]"      -> Direct_Declarator_Arr      acc (extCE v)
                                                                                       | id == "f()"     -> Direct_Declarator_Func_Def acc (extDD v)
                                                                                       | id == "f(s)"    -> Direct_Declarator_Func     acc (extSS v)) b ms)




-- Parameter Type List Definition
----------------------------------------
data Parameter_Type_List = Parameter_Type_List Parameter_List
                         | Parameter_Type_List_Ellipses Parameter_List
                         deriving Show

instance Pretty_Print Parameter_Type_List where
    stringify n (Parameter_Type_List a) = (n_spaces n) ++ (stringify 0 a)
    stringify n (Parameter_Type_List_Ellipses a) = (n_spaces n) ++ (stringify 0 a) ++ ", ..." 


parameter_type_list = (parameter_list >>= \i -> operator "," >> operator "..." >> (return $ Parameter_Type_List_Ellipses i))
                  <|> (parameter_list >>= \i -> return $ Parameter_Type_List i)



-- Parameter List Definition
----------------------------------------
data Parameter_List = Parameter_List_Declaration Parameter_Declaration
                    | Parameter_List_Sequence Parameter_List Parameter_Declaration
                    deriving Show

instance Pretty_Print Parameter_List where
    stringify n (Parameter_List_Declaration i) = (n_spaces n) ++ (stringify 0 i)
    stringify n (Parameter_List_Sequence a b)  = (n_spaces n) ++ (stringify 0 a) ++ " " ++ (stringify 0 b)

parameter_list = (sepBy1 parameter_declaration (operator ",")) >>= \ms -> case ms of
                                                                        (x:[])           -> return $ Parameter_List_Declaration x
                                                                        (x:xs)           -> return $ foldl (\acc x -> Parameter_List_Sequence acc x) (Parameter_List_Declaration x) xs




-- Parameter Declaration Definition
----------------------------------------
data Parameter_Declaration = Parameter_Concrete_Declarator [Declaration_Specifier] Declarator
                           | Parameter_Abstract_Declarator [Declaration_Specifier] Abstract_Declarator
                           | Parameter_Specifier           [Declaration_Specifier]
                           deriving Show

instance Pretty_Print Parameter_Declaration where
    stringify n (Parameter_Concrete_Declarator xs x) = (n_spaces n) ++ (concat . intersperse " " . fmap (stringify 0) $ xs) ++ " " ++ (stringify 0 x)
    stringify n (Parameter_Abstract_Declarator xs x) = (n_spaces n) ++ (concat . intersperse " " . fmap (stringify 0) $ xs) ++ " " ++ (stringify 0 x)
    stringify n (Parameter_Specifier           xs  ) = (n_spaces n) ++ (concat . intersperse " " . fmap (stringify 0) $ xs)


parameter_declaration = (some declaration_specifier >>= \ss -> declarator >>= \d -> return $ Parameter_Concrete_Declarator ss d)
                    <|> (some declaration_specifier >>= \ss -> abstract_declarator >>= \d -> return $ Parameter_Abstract_Declarator ss d)
                    <|> (some declaration_specifier >>= \ss -> return $ Parameter_Specifier ss)






-- Abstract Declarator Definition
----------------------------------------
data Abstract_Declarator = Abstract_Pointer Pointer
                         | Abstract_Pointer_Direct_Declarator Pointer Direct_Abstract_Declarator
                         | Abstract_Direct_Declarator Direct_Abstract_Declarator
                         deriving Show

instance Pretty_Print Abstract_Declarator where
    stringify n (Abstract_Pointer p) = (n_spaces n) ++ (stringify 0 p)
    stringify n (Abstract_Pointer_Direct_Declarator p a) = (n_spaces n) ++ (stringify 0 p) ++ " " ++ (stringify 0 a)
    stringify n (Abstract_Direct_Declarator a) = (n_spaces n) ++ (stringify 0 a)

abstract_declarator = (pointer >>= \p -> direct_abstract_declarator >>= \d -> return $ Abstract_Pointer_Direct_Declarator p d)
                  <|> (pointer >>= \p -> return $ Abstract_Pointer p)
                  <|> (direct_abstract_declarator >>= \d -> return $ Abstract_Direct_Declarator d)





-- Direct Abstract Declarator Definition
----------------------------------------
data Direct_Abstract_Declarator = Direct_Abstract_Parens    Abstract_Declarator
                                | Direct_Abstract_Arr      (Maybe Direct_Abstract_Declarator) (Maybe Constant_Expression)
                                | Direct_Abstract_Function (Maybe Direct_Abstract_Declarator) (Maybe Parameter_Type_List)
                                deriving Show

instance Pretty_Print Direct_Abstract_Declarator where
    stringify n (Direct_Abstract_Parens a)                       = (n_spaces n) ++ "(" ++ (stringify 0 a) ++ ")"
    stringify n (Direct_Abstract_Arr (Just a) (Just b))          = (n_spaces n) ++ (stringify 0 a) ++ "[" ++ (stringify 0 b) ++ "]" 
    stringify n (Direct_Abstract_Arr (Just a) Nothing )          = (n_spaces n) ++ (stringify 0 a) ++ "[]"
    stringify n (Direct_Abstract_Arr Nothing  (Just b))          = (n_spaces n) ++ "[" ++ (stringify 0 b) ++ "]"
    stringify n (Direct_Abstract_Arr Nothing  Nothing )          = (n_spaces n) ++ "[]"
    stringify n (Direct_Abstract_Function (Just a) (Just b))     = (n_spaces n) ++ (stringify 0 a) ++ "(" ++ (stringify 0 b) ++ ")"
    stringify n (Direct_Abstract_Function Nothing  (Just b))     = (n_spaces n) ++ "(" ++ (stringify 0 b) ++ ")"
    stringify n (Direct_Abstract_Function (Just a) Nothing )     = (n_spaces n) ++ (stringify 0 a) ++ "()"
    stringify n (Direct_Abstract_Function Nothing  Nothing )     = (n_spaces n) ++ "()"

data Temp_Direct_Abstract_Declarator = Cexpr (Maybe Constant_Expression) | ParList (Maybe Parameter_Type_List) deriving Show
extCexpr (Cexpr a) = a
extParList (ParList a) = a
unjust (Just a) = a

f_direct_abstract_declarator = (parens abstract_declarator >>= \p -> return $ Direct_Abstract_Parens p)
m_direct_abstract_declarator = ( try ( (brackets (try constant_expression) >>= \e -> return $ ("e",Cexpr e) ) <|> (parens (try parameter_type_list) >>= \p -> return $ ("p", ParList p)) ) >>= \c -> case c of
                                                                                                                                                                                               Nothing          -> return []
                                                                                                                                                                                               Just v           -> m_direct_abstract_declarator >>= \m -> return (v:m)) 

direct_abstract_declarator = (try f_direct_abstract_declarator) >>= \f -> m_direct_abstract_declarator >>= \xs -> case xs of
                                                                                                            []       -> case f of
                                                                                                                        Nothing   -> empty
                                                                                                                        (Just v)  -> return $ v
                                                                                                            xs       -> return $ unjust $ foldl (\acc (id,v) -> case id of
                                                                                                                                                "e"      -> Just (Direct_Abstract_Arr acc (extCexpr v))
                                                                                                                                                "p"      -> Just (Direct_Abstract_Function acc (extParList v))) f xs




-- Declaration Definition
----------------------------------------
data Declaration = Declaration [Declaration_Specifier] (Maybe Init_Declarator) deriving Show

instance Pretty_Print Declaration where
    stringify n (Declaration xs (Just decl)) = (n_spaces n) ++ (concat . intersperse " " . fmap (stringify 0) $ xs) ++ " " ++ (stringify 0 decl)
    stringify n (Declaration xs Nothing) = (n_spaces n) ++ (concat . intersperse " " . fmap (stringify 0) $ xs)

declaration = (some declaration_specifier) >>= \s -> (try init_declarator) >>= \a -> return $ Declaration s a





-- Init Declarator Definition
----------------------------------------
data Init_Declarator = Init_Declarator Declarator
                    | Init_Assignment_Declarator Declarator Initializer
                    deriving Show

instance Pretty_Print Init_Declarator where
    stringify n (Init_Declarator a) = (n_spaces n) ++ (stringify 0 a)
    stringify n (Init_Assignment_Declarator a b) = (n_spaces n) ++ (stringify 0 a) ++ " " ++ (stringify 0 b)

init_declarator = (declarator >>= \d -> operator "=" >> initializer >>= \i -> return $ Init_Assignment_Declarator d i)
              <|> (declarator >>= \d -> return $ Init_Declarator d)






-- Initializer Definition
----------------------------------------
data Initializer = Initializer_Assignment Assignment_Expression
                 | Initializer_Sequence Initializer_List
                 | Initializer_Sequence_Comma Initializer_List
                 deriving Show

instance Pretty_Print Initializer where
    stringify n (Initializer_Assignment a)     = (n_spaces n) ++ (stringify 0 a)
    stringify n (Initializer_Sequence a)       = (n_spaces n) ++ "{" ++ (stringify 0 a) ++ "}"
    stringify n (Initializer_Sequence_Comma a) = (n_spaces n) ++ "{" ++ (stringify 0 a) ++ ",}"

initializer = (braces (initializer_list >>= \ls -> operator "," >> (return $ ls)) >>= \ls -> (return $ Initializer_Sequence_Comma ls))
          <|> (braces initializer_list >>= \ls -> (return $ Initializer_Sequence ls))
          <|> (assignment_expression >>= \a -> return $ Initializer_Assignment a)




-- Initializer List Definition
----------------------------------------
data Initializer_List = Initializer_List_Single Initializer
                      | Initializer_List_Sequence Initializer_List Initializer
                      deriving Show
instance Pretty_Print Initializer_List where
    stringify n (Initializer_List_Single a) = (n_spaces n) ++ (stringify 0 a)
    stringify n (Initializer_List_Sequence a b) = (n_spaces n) ++ (stringify 0 a) ++ ", " ++ (stringify 0 b)

initializer_list = (sepBy1 initializer (operator ",")) >>= \ls -> case ls of
                                                                (x:[])      -> return $ Initializer_List_Single x
                                                                (x:xs)      -> return $ foldl Initializer_List_Sequence (Initializer_List_Single x) xs



-- Type Name Definition
----------------------------------------
data Type_Name = Type_Name [Specifier_Qualifier] (Maybe Abstract_Declarator) deriving Show

instance Pretty_Print Type_Name where
    stringify n (Type_Name ls (Just x)) =  (n_spaces n) ++ (concat . intersperse " " . fmap (stringify 0) $ ls) ++ " " ++ (stringify 0 x)
    stringify n (Type_Name ls Nothing)  =  (n_spaces n) ++ (concat . intersperse " " . fmap (stringify 0) $ ls)

type_name = (some specifier_qualifier) >>= \ls -> (try abstract_declarator) >>= \m -> return $ Type_Name ls m




-- Enum Specifier Definition
----------------------------------------
data Enum_Specifier = Full_Enum String Enumerator_List
                    | Annonymous_Enum Enumerator_List
                    | Partial_Enum String
                    deriving Show

instance Pretty_Print Enum_Specifier where
    stringify n (Full_Enum a b)     = (n_spaces n) ++ "enum " ++ a ++ " {\n" ++ (stringify n b) ++ (n_spaces n) ++ "}"
    stringify n (Annonymous_Enum b) = (n_spaces n) ++ "enum " ++ "{\n" ++ (stringify n b) ++ (n_spaces n) ++ "}"
    stringify n (Partial_Enum a)    = (n_spaces n) ++ "enum " ++ a

enum_specifier = (keyword "enum" >> identifier >>= \i -> braces enumerator_list >>= \ls -> return $ Full_Enum i ls)
             <|> (keyword "enum" >> braces enumerator_list >>= \ls -> return $ Annonymous_Enum ls)
             <|> (keyword "enum" >> identifier >>= \i -> return $ Partial_Enum i)



-- Enumerator List Definition
----------------------------------------
data Enumerator_List = Enumerator_List_Single Enumerator
                     | Enumerator_List_Sequence Enumerator_List Enumerator
                     deriving Show

instance Pretty_Print Enumerator_List where
    stringify n (Enumerator_List_Single a) = (n_spaces (2*n)) ++ (stringify 0 a) ++ "\n"
    stringify n (Enumerator_List_Sequence a b) = (stringify n a) ++ "," ++ (stringify (2*n) b) ++ "\n"

enumerator_list = (sepBy1 enumerator (operator ",")) >>= \es -> case es of
                                                            (x:[])     -> return $ (Enumerator_List_Single x)
                                                            (x:xs)     -> return $ foldl Enumerator_List_Sequence (Enumerator_List_Single x) xs



-- Struct Specifier Definition
----------------------------------------
data Struct_Specifier = Full_Struct String [Struct_Declaration]
                    | Annonymous_Struct [Struct_Declaration]
                    | Partial_Struct String
                    deriving Show

instance Pretty_Print Struct_Specifier where
    stringify n (Full_Struct a b)     = (n_spaces n) ++ "struct " ++ a ++ " {\n" ++ (concat . intersperse "\n" . fmap (stringify (2*n)) $ b) ++ "\n" ++ (n_spaces n) ++ "}"
    stringify n (Annonymous_Struct b) = (n_spaces n) ++ "struct " ++ "{\n" ++ (concat . intersperse "\n" . fmap (stringify (2*n)) $ b) ++ "\n" ++ (n_spaces n) ++ "}"
    stringify n (Partial_Struct a)    = (n_spaces n) ++ "struct " ++ a

struct_specifier = (keyword "struct" >> identifier >>= \i -> braces (sepBy1 struct_declaration (operator ";")) >>= \ls -> return $ Full_Struct i ls)
             <|> (keyword "struct" >> braces (sepBy1 struct_declaration (operator ";")) >>= \ls -> return $ Annonymous_Struct ls)
             <|> (keyword "struct" >> identifier >>= \i -> return $ Partial_Struct i)




-- Union Specifier Definition
----------------------------------------
data Union_Specifier = Full_Union String [Struct_Declaration]
                    | Annonymous_Union [Struct_Declaration]
                    | Partial_Union String
                    deriving Show

instance Pretty_Print Union_Specifier where
    stringify n (Full_Union a b)     = (n_spaces n) ++ "union " ++ a ++ " {\n" ++ (concat . intersperse "\n" . fmap (stringify (2*n)) $ b) ++ "\n" ++ (n_spaces n) ++ "}"
    stringify n (Annonymous_Union b) = (n_spaces n) ++ "union " ++ "{\n" ++ (concat . intersperse "\n" . fmap (stringify (2*n)) $ b) ++ "\n" ++ (n_spaces n) ++ "}"
    stringify n (Partial_Union a)    = (n_spaces n) ++ "union " ++ a

union_specifier = (keyword "union" >> identifier >>= \i -> braces (some struct_declaration) >>= \ls -> return $ Full_Union i ls)
             <|> (keyword "union" >> braces (some struct_declaration) >>= \ls -> return $ Annonymous_Union ls)
             <|> (keyword "union" >> identifier >>= \i -> return $ Partial_Union i)



--Struct Declaration Definition
----------------------------------------
data Struct_Declaration = Struct_Declaration [Specifier_Qualifier] Struct_Declarator_List deriving Show

instance Pretty_Print Struct_Declaration where
    stringify n (Struct_Declaration ls x) = (n_spaces n) ++ (concat . intersperse " " . fmap (stringify 0) $ ls) ++ (stringify 0 x)

struct_declaration = ((many specifier_qualifier) >>= \p -> struct_declarator_list >>= \l -> return $ Struct_Declaration p l)
                 <|> (struct_declarator_list >>= \l -> return $ Struct_Declaration [] l)




--Struct Declarator List Definition
----------------------------------------
data Struct_Declarator_List = Struct_Declarator_List_Single Struct_Declarator
                            | Struct_Declarator_Sequence Struct_Declarator_List Struct_Declarator
                            deriving Show

instance Pretty_Print Struct_Declarator_List where
    stringify n (Struct_Declarator_List_Single a) = (n_spaces n) ++ (stringify 0 a)
    stringify n (Struct_Declarator_Sequence a b ) = (n_spaces n) ++ (stringify 0 a) ++ ", " ++ (stringify 0 b)

struct_declarator_list = sepBy1 (struct_declarator) (operator ",") >>= \ls -> case ls of 
                                                                                (x:[])          -> return $ Struct_Declarator_List_Single x
                                                                                (x:xs)          -> return $ foldl Struct_Declarator_Sequence (Struct_Declarator_List_Single x) xs



--Struct Declarator List Definition
----------------------------------------
data Struct_Declarator = Full_Struct_Field Declarator
                       | Bit_Struct_Field Declarator Constant_Expression
                       | Empty_Struct_Field Constant_Expression
                       deriving Show

instance Pretty_Print Struct_Declarator where
    stringify n (Full_Struct_Field a) = (n_spaces n) ++ (stringify 0 a)
    stringify n (Bit_Struct_Field a b)  = (n_spaces n) ++ (stringify 0 a) ++ ": " ++ (stringify 0 b)
    stringify n (Empty_Struct_Field a)  = (n_spaces n) ++ ": " ++ (stringify 0 a)

struct_declarator = (declarator >>= \d -> operator ":" >> constant_expression >>= \e -> return $ Bit_Struct_Field d e)
                <|> (declarator >>= \d -> return $ Full_Struct_Field d)
                <|> (operator ":" >> constant_expression >>= \e -> return $ Empty_Struct_Field e)




-- Compound Statement Definition
----------------------------------------
data Compound_Statement = Compound_Statement [Declaration] [Statement] deriving Show


instance Pretty_Print Compound_Statement where
    stringify n (Compound_Statement d s) = "{" ++ (concat . intersperse ";\n" . fmap (stringify (2*n)) $ d) ++ ";\n " ++ (concat . intersperse "\n" . fmap (stringify (2*n)) $ s) ++ "}"


compound_statement = (braces (sepBy ((declaration >>= \d -> return $ Left d) <|> (statement >>= \s -> return $ Right s)) (whiteSpace <|> some (operator ";"))) >>= \ls -> return $ (let (ds,ss) = foldr (\x (a,b) -> case x of
                                                                                                                                                                                                            (Left y) -> (y:a, b)
                                                                                                                                                                                                            (Right y)-> (a, y:b)) ([],[]) ls in Compound_Statement ds ss))

-- Labelled Statement Definition
----------------------------------------
data Labeled_Statement = Label String Statement
                       | Case Constant_Expression Statement
                       | Default Statement
                       deriving Show

instance Pretty_Print Labeled_Statement where
    stringify n (Label i s) = (n_spaces n) ++ i ++ ": " ++ (stringify 0 s)
    stringify n (Case e s)  = (n_spaces n) ++ "case " ++ (stringify 0 e) ++ ": " ++ (stringify 0 s)
    stringify n (Default s) = (n_spaces n) ++ "default: " ++ (stringify 0 s)

labeled_statement = (keyword "default:" >> statement >>= \s -> return $ Default s)
                <|> (keyword "case" >> constant_expression >>= \e -> operator ":" >> statement >>= \s -> return $ Case e s)
                <|> (identifier >>= \i -> operator ":" >> statement >>= \s -> return $ Label i s)



data Expression_Statement = Expression_Statement (Maybe Expression) deriving Show

expression_statement = (try (expression) >>= \e -> operator ";" >> (return $ Expression_Statement e))

instance Pretty_Print Expression_Statement where
    stringify n (Expression_Statement (Just e)) = (stringify n e) ++ ";"
    stringify n (Expression_Statement Nothing)  = (n_spaces n) ++ ";"



data Selection_Statement = If_Statement Expression Statement
                         | If_Else_Statement Expression Statement Statement
                         | Switch_Statement Expression Statement
                         deriving Show

selection_statement = (keyword "if" >> parens expression >>= \e -> statement >>= \s1 -> keyword "else" >> statement >>= \s2 -> return $ If_Else_Statement e s1 s2)
                  <|> (keyword "if" >> parens expression >>= \e -> statement >>= \s1 -> return $ If_Statement e s1)
                  <|> (keyword "switch" >> parens expression >>= \e -> statement >>= \s1 -> return $ Switch_Statement e s1)

instance Pretty_Print Selection_Statement where
    stringify n (If_Statement e st) = (n_spaces n) ++ "if(" ++ (stringify 0 e) ++ ")\n" ++ (stringify n st)
    stringify n (If_Else_Statement e st1 st2) = (n_spaces n) ++ "if(" ++ (stringify 0 e) ++ ")\n" ++ (stringify n st1) ++ "\nelse " ++ (stringify n st2)
    stringify n (Switch_Statement e st1) = (n_spaces n) ++ "switch(" ++ (stringify 0 e) ++ ")\n" ++ (stringify n st1)



data Iteration_Statement = While_Statement Expression Statement
                         | Do_While_Statement Expression Statement
                         | For_Statement (Maybe Expression) (Maybe Expression) (Maybe Expression) Statement
                         deriving Show

iteration_statement = (keyword "while" >> parens expression >>= \e -> statement >>= \s -> return $ While_Statement e s)
                  <|> (keyword "do" >> statement >>= \s -> keyword "while" >> parens expression >>= \e -> operator ";" >> (return $ Do_While_Statement e s))
                  <|> (keyword "for" >> parens (try expression >>= \e1 -> operator ";" >> try expression >>= \e2 -> operator ";" >> try expression >>= \e3 -> return $ (e1,e2,e3)) >>= \(e1,e2,e3) -> statement >>= \s -> (return $ For_Statement (e1) (e2) (e3) s))


instance Pretty_Print Iteration_Statement where
    stringify n (While_Statement e st) = "while (" ++ (stringify 0 e) ++ ")\n " ++ (stringify (n) st)
    stringify n (Do_While_Statement e st) = "do\n" ++ (stringify n st) ++ " while (" ++ (stringify 0 e) ++ ");"
    stringify n (For_Statement  Nothing   Nothing   Nothing  st) = (n_spaces n) ++ "for(;;)" ++ (stringify n st)
    stringify n (For_Statement (Just e1)  Nothing   Nothing  st) = (n_spaces n) ++ "for(" ++ (stringify 0 e1) ++ ";;)" ++ (stringify n st)
    stringify n (For_Statement (Just e1) (Just e2)  Nothing  st) = (n_spaces n) ++ "for(" ++ (stringify 0 e1) ++ ";" ++ (stringify 0 e2) ++";)" ++ (stringify n st)
    stringify n (For_Statement (Just e1)  Nothing  (Just e3) st) = (n_spaces n) ++ "for(" ++ (stringify 0 e1) ++ ";;" ++ (stringify 0 e3) ++")" ++ (stringify n st)
    stringify n (For_Statement  Nothing  (Just e2)  Nothing  st) = (n_spaces n) ++ "for(;"++ (stringify 0 e2) ++";)" ++ (stringify n st)
    stringify n (For_Statement  Nothing  (Just e2) (Just e3) st) = (n_spaces n) ++ "for(;"++ (stringify 0 e2) ++";"++ (stringify 0 e3) ++")" ++ (stringify n st)
    stringify n (For_Statement  Nothing   Nothing  (Just e3) st) = (n_spaces n) ++ "for(;;"++ (stringify 0 e3) ++")" ++ (stringify 0 st)
    stringify n (For_Statement (Just e1) (Just e2) (Just e3) st) = (n_spaces n) ++ "for(" ++ (stringify 0 e1) ++ ";" ++ (stringify 0 e2) ++ ";" ++ (stringify 0 e3) ++ ")" ++ (stringify n st)


data Jump_Statement = Goto_Statement String
                    | Continue_Statement
                    | Break_Statement
                    | Return_Statement (Maybe Expression)
                    deriving Show

jump_statement = (keyword "goto" >> identifier >>= \i -> operator ";"  >> (return $ Goto_Statement i))
             <|> (keyword "continue" >> operator ";" >> (return $ Continue_Statement))
             <|> (keyword "break" >> operator ";"  >> (return $ Break_Statement))
             <|> (keyword "return" >> try expression >>= \i -> operator ";"  >> (return $ Return_Statement i))

instance Pretty_Print Jump_Statement where
    stringify n (Goto_Statement i) = (n_spaces n) ++ "goto " ++ i ++ ";"
    stringify n (Continue_Statement) = (n_spaces n) ++ "continue;"
    stringify n (Break_Statement) = (n_spaces n) ++ "break;"
    stringify n (Return_Statement (Just e)) = (n_spaces n) ++ "return " ++ (stringify 0 e) ++ ";"
    stringify n (Return_Statement Nothing) = (n_spaces n) ++ "return;"













data Statement = Statement_Labeled     Labeled_Statement
               | Statement_Expression  Expression_Statement
               | Statement_Compound    Compound_Statement
               | Statement_Selection   Selection_Statement
               | Statement_Iteration   Iteration_Statement
               | Statement_Jump        Jump_Statement
               deriving Show

instance Pretty_Print Statement where
    stringify n (Statement_Labeled s) = stringify n s
    stringify n (Statement_Expression s) = stringify n s
    stringify n (Statement_Compound s) = stringify n s
    stringify n (Statement_Selection s) = stringify n s
    stringify n (Statement_Iteration s) = stringify n s
    stringify n (Statement_Jump s) = stringify n s

statement = (labeled_statement >>= \l -> return $ Statement_Labeled l)
        <|> (expression_statement >>= \l -> return $ Statement_Expression l)
        <|> (compound_statement >>= \l -> return $ Statement_Compound l)
        <|> (selection_statement >>= \l -> return $ Statement_Selection l)
        <|> (iteration_statement >>= \l -> return $ Statement_Iteration l)
        <|> (jump_statement >>= \l -> return $ Statement_Jump l)

extract ((Right x), _) = x