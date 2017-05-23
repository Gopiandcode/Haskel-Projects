module C_Parser where

import Data.Char
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
             <|> (typedef_name >>= \s -> return $ Type_Typedef s)



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




type Additive_Expression = Expression
type Struct_Specifier = Expression
type Union_Specifier  = Expression
type Enum_Specifier   = Expression
type Typedef_Name     = Expression
data Expression = E deriving Show
instance Pretty_Print Expression where
    stringify n e = undefined

expression = undefined
struct_specifier = undefined
union_specifier = undefined
enum_specifier = undefined
typedef_name   = undefined
additive_expression = undefined