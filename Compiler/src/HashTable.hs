{-# LANGUAGE FlexibleInstances #-}

module HashTable where
import Data.List
import Control.Monad
import Data.Char

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
createHashMap n = HashMap n [[] | i <- [0..n-1]]

putHashMap :: (Hashable a, Eq a) => a -> b -> HashMap a b -> HashMap a b
putHashMap key value (HashMap size table) = HashMap size (before ++ addedBucket : rest)
            where position                = (hash key) `mod` size
                  (before, notAdded:rest) = splitAt position table
                  addedBucket             = if contains key notAdded then notAdded else (key,value) : notAdded

updateHashMap :: (Hashable a, Eq a) => a -> b -> HashMap a b -> HashMap a b
updateHashMap key value (HashMap size table) = HashMap size (before ++ (updatedBucket : rest))
              where position                  = (hash key) `mod` size
                    (before, notUpdated:rest) = splitAt position table
                    updatedBucket             = if contains key notUpdated then ((key,value) : [(a,b) | (a,b) <- notUpdated, a /= key]) else notUpdated

putOrUpdateHashMap :: (Hashable a, Eq a) => a -> b -> HashMap a b -> HashMap a b
putOrUpdateHashMap key value (HashMap size table) = HashMap size (before ++ updatedBucket : rest)
              where position                  = (hash key) `mod` size
                    (before, notUpdated:rest) = splitAt position table
                    updatedBucket             = if contains key notUpdated then ((key,value) : [(a,b) | (a,b) <- notUpdated, a /= key]) else (key,value) : notUpdated



removeHashMap :: (Hashable a, Eq a) => a -> HashMap a b -> HashMap a b
removeHashMap key (HashMap size table) = HashMap size (before ++ removedBucket : rest)
              where position                  = (hash key) `mod` size
                    (before, notRemoved:rest) = splitAt position table
                    removedBucket             = if contains key notRemoved then [(a,b) | (a,b) <- notRemoved, a /= key] else notRemoved

containsHashMap :: (Hashable a, Eq a) => a -> HashMap a b -> Bool
containsHashMap key (HashMap size table) = doesContains
              where position                    = (hash key) `mod` size
                    (before, mightContain:rest) = splitAt position table
                    doesContains                = contains key mightContain 

getHashMap :: (Hashable a, Eq a) => a -> HashMap a b -> Maybe b
getHashMap key (HashMap size table) = maybeResult
             where position                     = (hash key) `mod` size
                   (before, mightContain:rest)  = splitAt position table
                   maybeResult                  = getKey key mightContain
                   getKey key []                = Nothing
                   getKey key ((a,b):xs)        = if (key == a) then (Just b) else getKey key xs

resizeHashMap :: (Hashable a, Eq a) => Int -> HashMap a b -> HashMap a b
resizeHashMap newSize (HashMap oldSize table) = foldr (uncurry putHashMap) (createHashMap newSize) (join table)
