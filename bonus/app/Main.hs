module Main where

import Parser
import System.Environment
import System.Exit
import Text.Printf
import Data.Maybe

displayRes :: [Double] -> IO()
displayRes [] = return ()
displayRes (x:xs) = do
    printf "%.2f\n" (x :: Double)
    displayRes xs

main :: IO ()
main = do
    args <- getArgs
    case args of
        [expr] -> case (evalExpr expr) of
            Nothing -> exitWith(ExitFailure 84)
            Just expr -> displayRes [last expr]
        [expr, "--all"] -> case (evalExpr expr) of
            Nothing -> exitWith(ExitFailure 84)
            Just expr -> displayRes expr
        _ -> putStrLn "Error"