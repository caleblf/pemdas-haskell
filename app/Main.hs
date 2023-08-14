module Main where

import System.Environment (getArgs)
import System.Exit (die)

import Pemdas.Definitions (doubleLanguage)
import Pemdas.Types (evaluateIn, render, Result (getEither))
import Pemdas.Parse (parseExpression)

-- TODO set up a real CLI with help text, etc
main :: IO ()
main = do
    args <- getArgs
    exprText <- case args of
        [exprText] -> return exprText
        clargs ->
            die $ "Exactly one argument is required; got "
            ++ show (length clargs)

    case parseExpression doubleLanguage exprText of
        Left err -> die err
        Right expr ->
            do
                putStrLn $ render expr
                case getEither $ evaluateIn doubleLanguage expr of
                    Left errMsg -> die $ "Error: " ++ errMsg
                    Right result -> putStrLn $ "= " ++ show result
