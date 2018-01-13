{-# LANGUAGE FlexibleInstances #-}

module Main where
import Data.Functor
import Data.List
import Control.Monad
import Data.Char

main :: IO ()
main = do
  putStrLn "hello world"

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


class Hashable a where
  hash :: a -> Int

instance Hashable String where
  hash = foldl' f golden
    where f m c = fromIntegral (ord c) * magic + m
          magic = 0xdeadbeef
          golden = round((sqrt 5 - 1) * 2 ^ 32)

data HashMap a b = HashMap Int [[(a,b)]] deriving (Show)

contains :: (Eq a) => a -> [(a,b)] -> Bool
contains a = any (\(key,_) -> key == a)

createHashMap :: (Hashable a, Eq a) => Int -> HashMap a b
createHashMap n = HashMap n [[] | i <- [0..n]]

putHashMap :: (Hashable a, Eq a) => a -> b -> HashMap a b -> HashMap a b
putHashMap key value (HashMap size table) = HashMap size (before ++ addedBucket : rest)
            where position                = (hash key) `mod` size
                  (before, notAdded:rest) = splitAt position table
                  addedBucket             = if contains key notAdded then notAdded else (key,value) : notAdded

removeHashMap :: (Hashable a, Eq a) => a -> HashMap a b -> HashMap a b
removeHashMap key (HashMap size table) = HashMap size (before ++ removedBucket : rest)
              where position                  = (hash key) `mod` size
                    (before, notRemoved:rest) = splitAt position table
                    removedBucket             = if contains key notRemoved then [(a,b) | (a,b) <- notRemoved, a /= key] else notRemoved

resizeHashMap :: (Hashable a, Eq a) => Int -> HashMap a b -> HashMap a b
resizeHashMap newSize (HashMap oldSize table) = foldr (uncurry putHashMap) (createHashMap newSize) (join table)