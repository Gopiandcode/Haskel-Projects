{-# LANGUAGE FlexibleInstances #-}

module AST where
import HashMap

data Identifier = Id Int deriving (Show)


data SymbolTable = SymbolTable Int (HashMap String Int) deriving (Show)

createSymbolTable :: Int -> SymbolTable
createSymbolTable tableSize = SymbolTable 0 (createHashMap tableSize)

getIdentifierFor :: (String, SymbolTable) -> (Identifier, SymbolTable)
getIdentifierFor (string, SymbolTable nextIdentifier hashtable) = case (getHashMap string hashtable) of
                                            Just identifier       -> (Id identifier, SymbolTable nextIdentifier hashtable)
                                            Nothing               -> (Id nextIdentifier, SymbolTable (nextIdentifier+1) (putHashMap string nextIdentifier hashtable))