module Packrat where

import Parse

data ArithDerivs = ArithDerivs {
    dvAdditive :: Result ArithDerivs Double,
    dvMultitive :: Result ArithDerivs Double,
    dvPower :: Result ArithDerivs Double,
    dvExpression :: Result ArithDerivs Double,
    dvPrimary :: Result ArithDerivs Double,
    dvDecimal :: Result ArithDerivs Double,
    advChar :: Result ArithDerivs Char
}

instance Derivs ArithDerivs where
    dvChar d = advChar d


evalExpr :: String -> Maybe Double
evalExpr s = case dvAdditive (parse s) of
    Parsed v d -> case (advChar d) of
        NoParse -> if isInfinite v then Nothing else Just v
        (Parsed _ _) -> Nothing
    NoParse -> Nothing

parse :: String -> ArithDerivs
parse s = d where
    d = ArithDerivs add mult pow expr prim dec chr
    add = pAdditive d
    mult = pMultitive d
    pow = pPower d
    expr = pExpression d
    prim = pPrimary d
    dec = pDecimal d
    chr = case s of
        (c:s') -> Parsed c (parse s')
        [] -> NoParse

pAdditive :: ArithDerivs -> Result ArithDerivs Double
Parser pAdditive = do
    lv <- Parser dvMultitive
    rv <- pMany ((pSpace (pChar '+' <|> pChar '-')) >>= (\op -> Parser dvMultitive >>= (\mul -> return (op, mul))))
    pFoldM (\s1 (op, s2) -> case op of
        '+' -> return $ s1 + s2
        '-' -> return $ s1 - s2) lv rv 

pMultitive :: ArithDerivs -> Result ArithDerivs Double
Parser pMultitive = do
    lv <- Parser dvPower
    rv <- pMany ((pSpace (pChar '*' <|> pChar '/')) >>= (\op -> Parser dvPower >>= (\pow -> return (op, pow))))
    pFoldM (\s1 (op, s2) -> case op of
        '*' -> return $ s1 * s2
        '/' -> return $ s1 / s2) lv rv 


pPower :: ArithDerivs -> Result ArithDerivs Double
Parser pPower = do
    lv <- Parser dvExpression
    rv <- pMany (pSpace (pChar '^') >>= (\op -> Parser dvExpression >>= (\dec -> return (op, dec))))
    pFoldM (\s1 (op, s2) -> return (s1 ** s2)) lv rv 


pExpression :: ArithDerivs -> Result ArithDerivs Double
Parser pExpression = do
    op <- pSpace(pChar '-' <|> pChar '+')
    ex <- Parser dvPrimary
    case op of
        '+' -> return ex
        '-' -> return (-ex)
    <|>
    Parser dvPrimary

pPrimary :: ArithDerivs -> Result ArithDerivs Double
Parser pPrimary = do
    pSpace $ pChar '('
    expr <- Parser dvAdditive
    pSpace $ pChar ')'
    return expr
    <|> 
    Parser dvDecimal

pDecimal :: ArithDerivs -> Result ArithDerivs Double
Parser pDecimal = do
    val <- pSpace pDouble
    return val


