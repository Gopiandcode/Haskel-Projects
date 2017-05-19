module C_Lang where
import Control.Applicative
import Data.Char
import Parser

data Op = Add
        | Sub
        | Mul
        | Div
        | Mod
        | Eq
        | Neg
        | Gt
        | Geq
        | Lt
        | Leq
        | And
        | Or deriving (Eq, Show)

operator :: Parser Op
operator = P $(\string -> case string of
                ('+':cs)           -> (cs, Right Add)
                ('-':cs)           -> (cs, Right Sub)
                ('*':cs)           -> (cs, Right Mul)
                ('/':cs)           -> (cs, Right Div)
                ('%':cs)           -> (cs, Right Mod) 
                ('=':'=':cs)       -> (cs, Right Eq)
                ('!':'=':cs)       -> (cs, Right Neg)
                ('>':'=':cs)       -> (cs, Right Geq)
                ('<':'=':cs)       -> (cs, Right Leq)
                ('>':cs)           -> (cs, Right Gt)
                ('<':cs)           -> (cs, Right Lt)
                ('&':'&':cs)       -> (cs, Right And)
                ('|':'|':cs)       -> (cs, Right Or)
                _                  -> (string, Left "did not satisfy"))


data Expr = Var String
            | Num Integer
            | BinOp Op Expr Expr deriving (Eq, Show)


expression' :: Parser Expr
expression' = (fmap Num integer) <|> (fmap Var (whileParse (`elem` ['a'..'z'])))

expression :: Parser Expr
expression = dropWhiteSpace $ do
        exp1 <- expression'
        bc <- someParse (dropWhiteSpace operator)
        if null bc
            then return exp1
            else do
                exp2 <- expression
                return (BinOp (head bc) (exp1) (exp2))

binOpExpr :: Parser Expr
binOpExpr = dropWhiteSpace $ do
        a <- expression
        b <- operator
        c <- expression
        return (BinOp b a c)

data Stmt = Skip
        | Assign Expr Expr
        | Colon Stmt Stmt
        | WhileLoop Expr Stmt
        | IfCond Expr Stmt Stmt deriving (Eq, Show)


keyword :: String -> Parser String
keyword c = dropWhiteSpace $ string c

ifStmt :: Parser Stmt
ifStmt = dropWhiteSpace $ do
        keyword "if"
        cond <- encloseIn '(' ')' expression
        keyword "then"
        action1 <- statement
        keyword "else"
        action2 <- statement
        return $ IfCond cond action1 action2

whileStmt :: Parser Stmt
whileStmt = dropWhiteSpace $ do
        keyword "while"
        cond <- encloseIn '(' ')' expression
        action <- statement
        return $ WhileLoop cond action

assignStmt :: Parser Stmt
assignStmt = dropWhiteSpace $ do
        var <- expression
        keyword ":="
        expr <- expression
        return $ Assign var expr

skipStmt :: Parser Stmt
skipStmt = dropWhiteSpace $ do
        keyword "skip"
        return Skip

colonStmt :: Parser Stmt
colonStmt = dropWhiteSpace $ do
        s1 <- statement
        keyword ";"
        s2 <- statement
        return $ Colon s1 s2

statement :: Parser Stmt
statement = dropWhiteSpace $ do
            statements <- sepBy (keyword ";") (statement')
            if length statements == 1
                then return $ head statements
                else return $ foldr1 Colon statements

statement' :: Parser Stmt
statement' = whileStmt <|> assignStmt <|> ifStmt <|> skipStmt