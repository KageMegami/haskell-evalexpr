{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Control.Applicative
import Control.Monad
import Data.Maybe

import Prelude hiding (lookup)
import qualified Data.Map as M
import Control.Monad.State

newtype Variable = Variable String deriving (Ord, Eq, Show)


data Expr =   Add Expr Expr
            | Sub Expr Expr
            | Mul Expr Expr
            | Div Expr Expr
            | Pow Expr Expr
            | Less Expr Expr
            | Greater Expr Expr
            | Equal Expr Expr
            | LessOrEqual Expr Expr
            | GreaterOrEqual Expr Expr
            | Or Expr Expr
            | And Expr Expr
            | Num Double
            | Var Variable
            | Assign Variable Expr

eval :: [Expr] -> [Double]
eval es = evalState (mapM eval' es) M.empty
    where
        eval' (Num n) = return n
        eval' (Var v) = do
            vs <- get
            case M.lookup v vs of
                Just x -> return x
                _ -> error $ "variable " ++ show v ++ " is undefined!"
        eval' (Assign v ex) = do
            x <- eval' ex
            modify (M.insert v x)
            return x
        eval' (Add a b) = do
            x <- eval' a
            y <- eval' b
            return (x + y)
        eval' (Sub a b) = do
            x <- eval' a
            y <- eval' b
            return (x - y)
        eval' (Mul a b) = do
            x <- eval' a
            y <- eval' b
            return (x * y)
        eval' (Div a b) = do
            x <- eval' a
            y <- eval' b
            return (x / y)
        eval' (Pow a b) = do
            x <- eval' a
            y <- eval' b
            return (x ** y)
        eval' (Less a b) = do
            x <- eval' a
            y <- eval' b
            if (x < y) then return 1 else return 0
        eval' (LessOrEqual a b) = do
            x <- eval' a
            y <- eval' b
            if (x <= y) then return 1 else return 0
        eval' (Greater a b) = do
            x <- eval' a
            y <- eval' b
            if (x > y) then return 1 else return 0
        eval' (GreaterOrEqual a b) = do
            x <- eval' a
            y <- eval' b
            if (x >= y) then return 1 else return 0
        eval' (Equal a b) = do
            x <- eval' a
            y <- eval' b
            if (x == y) then return 1 else return 0
        eval' (Or a b) = do
            x <- eval' a
            y <- eval' b
            if (x == 1 || y == 1) then return 1 else return 0
        eval' (And a b) = do
            x <- eval' a
            y <- eval' b
            if (x == 1 && y == 1) then return 1 else return 0

newtype Parser a = Parser { runParser :: String -> Either String (String , a) }

pSatisfy :: (Char -> Bool) -> Parser Char
pSatisfy fcb = Parser fun
    where
        fun :: String -> Either String (String, Char)
        fun [] = Left "Error empty string"
        fun (x:xs)
            | fcb x = Right (xs, x)
            | otherwise = Left "Error while parsing"

pChar :: Char -> Parser Char
pChar c = pSatisfy (== c)

pSpaces :: Parser String
pSpaces = many (pOneOf " \t")

pOperator :: Char -> Parser Char
pOperator op = pSpaces *> (pChar op) <* pSpaces

pOneOf :: String -> Parser Char
pOneOf str = pSatisfy (\s -> elem s str)

pDigit :: Parser Char
pDigit = pOneOf "1234567890"

pDigits :: Parser String
pDigits = some pDigit

pFloat :: Parser String
pFloat = do
    lv <- pDigits
    dot <- pChar '.'
    rv <- pDigits
    return $ lv ++ (dot:rv)
    <|>
    pDigits

pNum :: Parser Expr
pNum = fmap Num $ (fmap read) (pSpaces *>  pFloat <* pSpaces)

pValue :: Parser Expr
pValue = pNum
        <|> do
        pOperator '(' *> pExpr <* pOperator ')'
        <|>
        pVar

pOperand :: Parser Expr
pOperand = do
    sign <- pChar '-' <|> pChar '+'
    v <- pValue
    case sign of
        '-' -> return (Sub (Num 0) v)
        '+' -> return v
    <|>
    pValue

pPow ::Parser Expr
pPow = do
    lv <- pOperand
    rv <- many (pOperator '^' >>= (\op -> pOperand >>= (\value -> return (op, value))))
    foldM (\s1 (op, s2) -> return $ Pow s1 s2) lv rv

pMul :: Parser Expr
pMul = do
    lv <- pPow
    rv <- many ((pOperator '*' <|> pOperator '/') >>= (\op -> pPow >>= (\pow -> return (op, pow))))
    foldM (\s1 (op, s2) -> case op of
        '*' -> return $ Mul s1 s2
        '/' -> return $ Div s1 s2) lv rv

pAdd :: Parser Expr
pAdd = do
    lv <- pMul
    rv <- many ((pOperator '+' <|> pOperator '-') >>= (\op -> pMul >>= (\mul -> return (op, mul))))
    foldM (\s1 (op, s2) -> case op of
        '+' -> return $ Add s1 s2
        '-' -> return $ Sub s1 s2) lv rv 

pVariable :: Parser Variable
pVariable = do
    pSpaces
    n <- some (pOneOf "abcdefghtjklmnopqrstuvwxyz")
    pSpaces
    return $ Variable n

pVar :: Parser Expr
pVar = do
    v <- pVariable
    return $ Var v

pAssign :: Parser Expr
pAssign = do
    v <- pVariable
    pOperator '='
    ex <- pCmps <|> pAdd <|> pVar
    return $ Assign v ex

pString :: String -> Parser String
pString [] = return []
pString (x:xs) = do
    c <- pChar x
    str <- pString xs
    return $ c:str

pCmp :: Parser Expr
pCmp = do
    lv <- pAdd <|> pVar
    pSpaces
    op <- pString "<=" <|> pString ">=" <|> pString "<" <|> pString ">" <|> pString "=="
    pSpaces
    rv <- pAdd <|> pVar
    case op of
        "<" -> return $ Less lv rv
        "<=" -> return $ LessOrEqual lv rv
        ">" -> return $ Greater lv rv
        ">=" -> return $ GreaterOrEqual lv rv
        "==" -> return $ Equal lv rv
    <|> do
    pOperator '('
    cmp <- pCmps
    pOperator ')'
    return cmp


pCmps :: Parser Expr
pCmps = do
    lv <- pCmp
    rv <- many ((pString "||" <|> pString "&&") >>= (\op -> pCmp >>= (\cmp -> return (op, cmp))))
    foldM (\s1 (op, s2) -> case op of
        "||" -> return $ Or s1 s2
        "&&" -> return $ And s1 s2) lv rv 

pExpr :: Parser Expr
pExpr = pCmps <|> pAssign <|> pAdd

pExprs :: Parser [Expr]
pExprs = some (do
    expr <- pExpr
    pOperator ';' <|> return ' '
    return expr)


evalExpr :: String -> Maybe [Double]
evalExpr str = case (runParser pExprs str) of
    Left msg -> Nothing
    Right ([],x) -> Just $ eval x
    Right (xs, x') -> Nothing

pFmap :: (a -> b) -> Parser a -> Parser b
pFmap fab pa = Parser fun
    where
        fun str = case (runParser pa str) of
            Left msg -> Left msg
            Right (str', x) -> Right (str', fab x)

pReturn :: a -> Parser a
pReturn arg = Parser fun
    where
            fun str = Right (str, arg)            

pBind :: Parser a -> (a -> Parser b) -> Parser b
pBind pa fapb = Parser fun
    where
        fun str = case (runParser pa str) of
            Left msg -> Left msg
            Right (str', x) -> runParser (fapb x) str'

pSequential :: Parser (a -> b) -> Parser a -> Parser b
pSequential pfab pa = Parser fun
    where
        fun str = case (runParser pfab str) of
            Left msg -> Left msg
            Right (str', fab) -> case (runParser pa str') of
                Left msg -> Left msg
                Right (str'', a) -> Right (str'', fab a)


pSequentialLeft :: Parser a -> Parser b -> Parser b
pSequentialLeft pa pb = Parser fun
    where
        fun str = case (runParser pa str) of
            Left msg -> Left msg
            Right (str', x) -> runParser pb str'

pSequentialRight :: Parser a -> Parser b -> Parser a
pSequentialRight pa pb = Parser fun
    where
        fun str = case (runParser pa str) of
            Left msg -> Left msg
            Right (str', x) -> case (runParser pb str') of
                Left msg -> Left msg
                Right (str'', x') -> Right (str'', x)

pOr :: Parser a -> Parser a -> Parser a
pOr pa1 pa2 = Parser fun
    where
        fun str = case (runParser pa1 str) of
            Left msg -> runParser pa2 str
            Right (str', x) -> Right (str', x)

pEmpty :: Parser a
pEmpty = Parser (\s -> Left "Paring Failed")

instance Functor Parser where
    fmap = pFmap

instance Applicative Parser where
    pure = return
    (<*>) = pSequential
    (<*) = pSequentialRight
    (*>) = pSequentialLeft

instance Monad Parser where
    return = pReturn
    (>>=) = pBind

instance Alternative Parser where
    empty = pEmpty
    (<|>) = pOr