module Main where

import Packrat
import System.Environment
import System.Exit
import Text.Printf
import Data.Maybe

main :: IO ()
main = do
    args <- getArgs
    case args of
        [expr] -> case (evalExpr expr) of
            Nothing -> exitWith(ExitFailure 84)
            Just expr -> printf "%.2f\n" (expr :: Double)
        _ -> putStrLn "Error"