{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Data.Either (isLeft)

import Test.QuickCheck (quickCheckAll)

import Pemdas.Types
import Control.Monad (when)


prop_renderLiteral :: Double -> Bool
prop_renderLiteral x = render (Literal x) == show x

prop_renderVariable :: String -> Bool
prop_renderVariable name = render (Variable name) == name


basicLanguage :: Definitions Integer
basicLanguage = Definitions
    { binOps =
        [ ("/"
          , BinOp 7 (\m n -> do
                        when (n == 0) $ fail "division by 0"
                        return $ div m n)
          )
        , ("-"
          , BinOp 6 (\m n -> return $ m - n)
          )
        ]
    , functions = [("succ", \_ _ n -> return $ succ n)]
    , aggregations = [("Sigma", return . sum . map snd)]
    , env = []
    }


prop_evalVarEmptyEnv :: String -> Bool
prop_evalVarEmptyEnv name =
    isLeft $ getEither $ evaluateIn basicLanguage (Variable name)

prop_evalQuantification :: String -> Integer -> [(String, Integer)] -> Bool
prop_evalQuantification name domainMin env =
    let expr =
            Quantified $
            AggregationApplication
                { aggName   = "Sigma"
                , varName   = name
                , domainMin = domainMin
                , domainMax = domainMin + 1
                , aggBody   = Variable name
                }
        actualResult = evaluateIn basicLanguage expr
        expectedResult = return (domainMin + domainMin + 1)
    in actualResult == expectedResult

prop_fullEvaluation :: String -> String -> Integer -> Integer -> Bool
prop_fullEvaluation nameX nameY x domainMin =
    -- x + -\Sigma_{y \in [domainMin..domainMin+5]}(\succ(x + y))
    let expr =
            Infix "+" (Variable nameX) $
            Negated $
            Quantified $
            AggregationApplication
                { aggName   = "Sigma"
                , varName   = nameY
                , domainMin = domainMin
                , domainMax = domainMin + 5
                , aggBody   = ApplyFunc
                    FunctionApplication
                        { funcName        = "succ"
                        , subscriptExpr   = Nothing
                        , superscriptExpr = Nothing
                        , argumentExpr =
                            Infix "+" (Variable nameX) (Variable nameY)
                        }
                }
        actualResult = evaluateIn (basicLanguage { env = [(nameX, x)] }) expr
        expectedResult =
            return (x - sum [succ (x + y) | y <- [domainMin..domainMin+5]])
    in actualResult == expectedResult


return [] -- TemplateHaskell splice for quickCheckAll

main = $quickCheckAll
