module Parse where

import Control.Applicative
import Control.Monad
import Data.Maybe

data Result d v = Parsed v d
                | NoParse

newtype Parser d v = Parser {runParser :: d -> Result d v}

class Derivs d where
    dvChar :: d -> Result d Char

infixl 2 <|>

pReturn :: a -> Parser d a
pReturn x = Parser fun
    where
        fun dvs = Parsed x dvs

pBind :: Parser d a -> (a -> Parser d b) -> Parser d b
pBind pa fapb = Parser fun
    where
        fun dvs = case (runParser pa dvs) of        
            (Parsed val rem) -> runParser (fapb val) rem
            NoParse -> NoParse

pFmap :: (a -> b) -> Parser d a -> Parser d b
pFmap fab pa = Parser fun
    where
        fun dvs = case (runParser pa dvs) of
            (Parsed val rem) -> Parsed (fab val) rem
            NoParse -> NoParse


pSequencer :: Parser d (a -> b) -> Parser d a -> Parser d b
pSequencer pfab pa = Parser fun
    where
        fun dvs = case (runParser pfab dvs) of
            NoParse -> NoParse
            (Parsed val rem) -> case (runParser pa rem) of
                NoParse -> NoParse
                (Parsed val' rem') -> Parsed (val(val')) rem'

pEmpty :: Parser d a
pEmpty = Parser fun
    where
        fun dvs = NoParse

pOr :: Derivs d => Parser d a -> Parser d a -> Parser d a
pOr pa1 pa2 = Parser fun
    where
        fun dvs = case (runParser pa1 dvs) of
            NoParse -> runParser pa2 dvs
            (Parsed val rem) -> (Parsed val rem)

instance Derivs d => Monad (Parser d) where 
    return = pReturn
    (>>=) = pBind

instance Derivs d => Functor (Parser d) where
    fmap = pFmap

instance Derivs d => Applicative (Parser d) where
    pure = return
    (<*>) = pSequencer

instance Derivs d => Alternative (Parser d) where
    empty = pEmpty
    (<|>) = pOr


(<|>) :: Derivs d => Parser d v -> Parser d v -> Parser d v
pa1 <|> pa2 = pOr pa1 pa2

pSatisfy :: Derivs d => Parser d a -> (a -> Bool) -> Parser d a
pSatisfy pa verify = Parser fun
    where
        fun dvs = case (runParser pa dvs) of
            NoParse -> NoParse
            (Parsed val rem) -> if verify val then (Parsed val rem) else NoParse

pAnyChar :: Derivs d => Parser d Char
pAnyChar = Parser dvChar

pChar :: Derivs d => Char -> Parser d Char
pChar c = pSatisfy pAnyChar (==c)

pOneOf :: Derivs d => String -> Parser d Char
pOneOf str = pSatisfy pAnyChar (\s -> elem s str)

pDigit :: Derivs d => Parser d Char
pDigit = pOneOf "1234567890"

pDigits :: Derivs d => Parser d String
pDigits = some pDigit

pInt :: Derivs d => Parser d Int
pInt = read <$> pDigits

pDouble :: Derivs d => Parser d Double
pDouble = read <$> ((pDigits >>= (\l -> pChar '.' >>= (\dot -> pDigits >>= (\r -> return (l ++ (dot:r)))))) Parse.<|> pDigits)

pMany :: Derivs d => Parser d a -> Parser d [a]
pMany pa = many pa

pFoldM :: (Foldable t, Derivs d) => (b -> a -> Parser d b) -> b -> t a -> Parser d b
pFoldM f b t  =  foldM f b t

pSpace :: Derivs d => Parser d a -> Parser d a
pSpace pa = (many (pOneOf " \t")) *> pa <* (many (pOneOf " \t"))