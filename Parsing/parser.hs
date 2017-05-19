module Parser where

import Data.Char
import Control.Applicative

type Error = String
newtype Parser a = P { extract :: (String -> (String, Either String a))} 

instance Functor Parser where
	fmap f (P x) = P $ \stream -> case x stream of
				(rest, Left err) -> (rest, Left err)
				(rest, Right x)  -> (rest, Right (f x))

instance Applicative Parser where
	pure x = P(\string -> (string,Right x))
	P f <*> P x = P $ \stream -> case f stream of
				(rest, Left err) -> (rest, Left err)
				(rest, Right f) -> case x rest of
					(remaining, Left err) -> (remaining, Left err)
					(remaining, Right x) -> (remaining, Right (f x)) 

instance Alternative Parser where
	empty = P $ (\string -> (string, Left "empty"))
	(<|>) = orElse	

instance Monad Parser where
	return = pure
	P x >>= f = P $ \stream -> case x stream of
			(rest, Left err)  -> (rest, Left err)
			(rest, Right x)   -> extract (f x) rest

-- Given a character predicate, parses it
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = P $ \stream -> case stream of
					(x:xs)     | p x       -> (xs, Right x)
							   | otherwise -> (x:xs, Left "did not satisfy")
					[]                     -> ([], Left "empty string")

-- Attempts two parsers, if the first one fails, try the second
orElse :: Parser a -> Parser a -> Parser a
orElse (P g) (P f) = P $ \string -> case g string of
							(remaining, Right x)  -> (remaining, Right x)
							(remaining, Left err) -> f string

-- Attempts a parser, if it fails, restore stream
try :: Parser a -> Parser a
try (P f) = P $ \stream -> case f stream of 
		(remaining, Left err) -> (stream, Left err)
		(remaining, Right x)  -> (remaining, Right x)

-- Parse something multiple times
manyParse :: Parser a -> Parser [a]
manyParse (P f) = P $ \stream -> case f stream of
			(remaining, Left err) -> (remaining, Left err)
			(remaining, Right x)  -> let (rest, Right xs) = go remaining in (rest, Right (x:xs))
					where go = (\string -> case f string of
							(remaining, Left err) -> (string, Right [])
							(remaining, Right x) -> let (rest, Right xs) = go remaining in (rest, Right (x:xs)))		
-- Parse something 0 or more times
someParse :: Parser a -> Parser [a]
someParse (P f) = P $ \stream -> case f stream of
			(remaining, Left err) -> (remaining, Right [])
			(remaining, Right x)  -> let (rest, Right xs) = go remaining in (rest, Right (x:xs))
					where go = (\string -> case f string of
							(remaining, Left err) -> (string, Right [])
							(remaining, Right x) -> let (rest, Right xs) = go remaining in (rest, Right (x:xs)))		



sepBy :: Parser a -> Parser b -> Parser [b]
sepBy (P sep) (P v) = P $ \string -> case v string of
			(remaining, Left err) -> (remaining, Left err)
			(remaining, Right val) -> let (rest, Right vals) = go remaining in (rest, Right (val:vals))
					where go = (\string -> case sep string of
								(rest, Left err) -> (string, Right [])
								(rest, Right xs) -> case v rest of
										(remaining, Left err) -> (remaining, Right [])
										(remaining, Right x) -> let (stream, Right xs) = go remaining in (stream, Right (x:xs)))

dropWhiteSpace :: Parser a -> Parser a
dropWhiteSpace (P f) = P go
		where go =  \string -> case string of
							(c:cs)     | isSpace c     -> go cs 
									   | otherwise     -> f (c:cs)
							[]                         -> ([], Left "end of input")

encloseIn :: Char -> Char -> Parser a -> Parser a
encloseIn st ed p = dropOuter <$> char st <*> p <*> (dropWhiteSpace $ char ed)
			where dropOuter _ a _ = a

whileParse :: (Char -> Bool) -> Parser [Char]
whileParse p = P $ \string -> case string of
				(c:cs)          | p c       -> let (rest, Right str) = go cs in (rest, Right (c:str))												
								| otherwise -> ((c:cs), Left "did not satisfy")
								where go = \string -> case string of
												(c:cs)    | p c     -> let (rest, Right str) = go cs in (rest, Right (c:str))
														  | otherwise -> (c:cs, Right [])
												[]     -> ([], Right [])								
				[]                    -> ([], Left "end of stream")

									 	
char :: Char -> Parser Char
char c = satisfy (==c)

string :: String -> Parser String
string [] = pure []
string (c:cs) = (:) <$> char c <*> string cs

integer :: Parser Integer
integer = fmap (read) (whileParse isDigit)


