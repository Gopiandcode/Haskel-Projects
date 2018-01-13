{-# LANGUAGE FlexibleInstances #-}

module Parser where
import Data.Functor
import Data.List
import Control.Monad




type Error = String
newtype Parser b a = P { parse :: (b, String) -> (Either String a, (b, String))}

type ParserState = String


instance Functor (Parser ParserState) where
  fmap f (P a) = P $ \value -> case a value of
                            (Left err, result) -> (Left err, value)
                            (Right v,  result) -> (Right $ f v, result)

instance Applicative (Parser ParserState) where
  pure a = P $ \value -> (Right a, value)
  (P f) <*> (P g) = P $ (\value -> case f value of
                          (Left err, result) -> (Left err, value)
                          (Right f, result)  -> case g result of
                                            (Left err, result1) -> (Left err, value)
                                            (Right g, remain)   -> (Right $ f g, remain))


instance Monad (Parser ParserState) where
  (P f) >>= g = P $(\value -> case f value of
                (Left err, result) -> (Left err, value)
                (Right f,  result) -> parse (g f) result)
  (P f) >> (P g) = P $ (\value -> case f value of
                (Left err, result) -> (Left err, value)
                (Right err, result) -> g result)
  return = pure
