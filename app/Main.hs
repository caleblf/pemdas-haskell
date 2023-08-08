module Main (main) where

import System.Environment (getArgs)
import System.Exit (die)

import Text.Parsec (eof, parse)

import Pemdas.Functions (doubleLanguage)
import Pemdas.Types (evaluateIn, Language (binOps))
import qualified Pemdas.Parse as P

-- TODO set up a real CLI with help text, etc
main :: IO ()
main = do
    args <- getArgs
    exprText <- case args of
        [exprText] -> return exprText
        clargs ->
            die $ "Exactly one argument is required; got "
            ++ show (length clargs)

    let parseResult =
            parse
                (do
                    parsedExpr <- P.makeExprParser $ binOps doubleLanguage
                    eof
                    return parsedExpr)
                "" exprText
    expr <- case parseResult of
        Left err ->
            die $ "Parsing failed " ++ show err
        Right expr -> return expr
    print expr
    result <- case evaluateIn doubleLanguage expr of
        Left errMsg -> die $ "Error: " ++ errMsg
        Right result -> return result
    putStrLn $ "= " ++ show result
    return ()
