module PreProcessor where
import Control.Applicative
import Parser
import Data.Char
import Data.List

type IsPointer = Bool
type Identifier = String
type Body = String
type IsStatic = Bool
type SuperClass = String
type SuperMetaClass = String

data Type = Type String IsPointer deriving Show 


data Attr = Attr Type Identifier deriving Show 

data Func = Func Type Identifier [Attr] IsStatic deriving Show 

data Class = Class Identifier String SuperClass [Attr] deriving Show 

data MetaClass = MetaClass Identifier SuperMetaClass [Func] deriving Show 

keyword :: String -> Parser String
keyword c = dropWhiteSpace $ string c


-- What we'll be parsing
-- Point : Object with PointClass <
--	int x;
--	int y;
--	int z;
-- >
--
-- PointClass : Class <
-- 	static int *something(int x, int y);
--	dynamic int *nothing(int x, int y);
-- 	>
--

replace :: Char -> Char -> String -> String
replace wit to (c:cs)
		| c == wit      = to  : replace wit to cs
		| otherwise     = c   : replace wit to cs
replace _   _   []      = []
p_ident :: Parser String
p_ident = dropWhiteSpace $ whileParse (\c -> isDigit c || c `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['_']))

p_type :: Parser Type
p_type = dropWhiteSpace $ do
        type_name <- p_ident
        pointers <- dropWhiteSpace $ someParse (char '*')
        if null pointers
            then return (Type (replace '_' ' ' type_name) False)
            else return (Type (replace '_' ' ' type_name) True)
p_attr :: Parser Attr
p_attr = dropWhiteSpace $ do
		typ <- p_type
		ident <- p_ident 
		return (Attr typ ident)

f_attr :: Parser Attr
f_attr = dropWhiteSpace $ do
		atr <- p_attr
		keyword ";"
		return atr

p_func :: Parser Func
p_func = dropWhiteSpace $ do
		x <- (keyword "static" <|> keyword "dynamic")
		typ <- p_type
		ident <- p_ident 
		param <- dropWhiteSpace $ encloseIn '(' ')' $ sepBy (char ',') p_attr
		if x == "static"
			then return (Func typ ident param True)
			else return (Func typ ident param False)

p_class :: Parser Class 
p_class = dropWhiteSpace $ do
		clsname <- p_ident	
		keyword ":"
		super <- p_ident
		keyword "with"
		meta <- p_ident
		attrs <- encloseIn '<' '>' (sepBy (keyword ";") p_attr)
		return (Class clsname meta super attrs) 

p_metaclass :: Parser MetaClass
p_metaclass = dropWhiteSpace $ do
			mtclsnm <- p_ident
			keyword ":"
			sprclsnm <- p_ident
			funcs <- dropWhiteSpace $ encloseIn '<' '>' $ sepBy (char ';') p_func
			return (MetaClass mtclsnm sprclsnm funcs)
