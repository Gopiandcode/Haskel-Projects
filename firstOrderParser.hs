module C_Parser where

import Data.Char
import Data.List
import Control.Applicative
type Error = String

newtype Parser a = P { parse :: (String -> (Either Error a, String))}

instance Functor Parser where

instance Applicative Parser where

instance Alternative Parser where

instance Monad Parser where

satisfy :: (Char -> Bool) -> Parser Char

char :: Char -> Parser Char

string :: String -> Parser String
