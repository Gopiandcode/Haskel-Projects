{-# LANGUAGE FlexibleInstances #-}

module Main where
import Data.Functor
import Data.List
import Control.Monad
import Data.Char

import HashMap
import Parser
import AST

main :: IO ()
main = do
  putStrLn "hello world"

newtype Custom a = C { run :: (SymbolTable, String) -> (Either String a, (SymbolTable, String)) } 

instance Functor Custom where
  fmap f (C a) = C $ \value -> case a value of
                            (Left err, result) -> (Left err, value)
                            (Right v,  result) -> (Right $ f v, result)

instance Applicative Custom  where
  pure a = C $ \value -> (Right a, value)
  (C f) <*> (C g) = C $ (\value -> case f value of
                          (Left err, result) -> (Left err, value)
                          (Right f, result)  -> case g result of
                                            (Left err, result1) -> (Left err, value)
                                            (Right g, remain)   -> (Right $ f g, remain))


instance Monad Custom where
  (C f) >>= g = C $(\value -> case f value of
                (Left err, result) -> (Left err, value)
                (Right f,  result) -> run (g f) result)
  (C f) >> (C g) = C $ (\value -> case f value of
                (Left err, result) -> (Left err, value)
                (Right err, result) -> g result)
  return = pure


bestName = return "kiran" :: Custom String

alpha :: Custom Char
alpha = C $ \(table, str) -> case str of 
          []                  -> (Left "Ran out of input", (table, []))
          (s:remain)          -> if isAlpha s then (Right s, (table, remain)) else (Left $ "Expecting alpha character, found " ++ [s], (table, (s:remain)))

alphaNum :: Custom String
alphaNum = C $ \(table, str) -> case str of
          []                  -> (Left "Ran out of input", (table, []))
          (s:remain) | (isAlpha s || isDigit s)       -> let (Right others, (newtable, notalphaNum)) = (run alphaNum) (table, remain) in (Right (s:others), (newtable, notalphaNum)) 
                     | otherwise                      -> (Right [], (table, s:remain))

skipWhiteSpace :: Custom a -> Custom a
skipWhiteSpace f = C $ \(table, str) -> case str of
        []                     -> (Left "Ran out of input", (table, []))
        (s:remain) | (isSpace s) -> (run $ skipWhiteSpace f) (table, remain)
                   | otherwise   -> (run f) (table, s:remain)

recordIdentifier :: String -> Custom Identifier
recordIdentifier idString = C $ \(table, str) -> let (identifier, newtable) = getIdentifierFor (idString, table) in (Right identifier, (newtable, str))

internalIdentifier :: Custom Identifier
internalIdentifier = do
  x  <- alpha
  xs <- alphaNum
  id <- recordIdentifier (x:xs)
  return id

identifier :: Custom Identifier
identifier = skipWhiteSpace internalIdentifier

example = do
  id1 <- identifier
  id2 <- identifier
  return (id1, id2)

runExample = (run example) (createSymbolTable 20, "kiran kiran kiran")