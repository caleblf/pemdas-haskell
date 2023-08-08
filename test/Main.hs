{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Data.Either (isLeft)

import Test.QuickCheck (quickCheckAll)

import Pemdas.Types


prop_showLiteral :: Double -> Bool
prop_showLiteral x = show (Literal x) == show x

prop_showVariable :: String -> Bool
prop_showVariable name = show (Variable name) == name


basicLanguage :: Language Integer
basicLanguage = Language
    { binOps       = [ ("/", (7, \m n -> if n == 0 then Left "division by 0"
                                                   else Right $ m `div` n))
                     , ("-", (6, \m n -> Right $ m - n))
                     ]
    , functions    = [("succ", \_ _ n -> Right $ succ n)]
    , aggregations = [("Sigma", Right . sum . map snd)]
    , env          = []
    }


prop_evalVarEmptyEnv :: String -> Bool
prop_evalVarEmptyEnv name = isLeft $ evaluateIn basicLanguage (Variable name)

prop_evalQuantification :: String -> Integer -> [(String, Integer)] -> Bool
prop_evalQuantification name domainMin env =
    evaluateIn basicLanguage (Quantified
        AggregationApplication
        { aggName="Sigma"
        , varName=name
        , domainMin=domainMin
        , domainMax=domainMin + 1
        , aggBody=Variable name
        }
    ) == Right (domainMin + domainMin + 1)

prop_fullEvaluation :: String -> String -> Integer -> Integer -> Bool
prop_fullEvaluation nameX nameY x domainMin =
    -- x + -\Sigma_{y \in [domainMin..domainMin+5]}(\succ(x + y))
    evaluateIn
        (basicLanguage { env = [(nameX, x)] })
        (Infix "+" (Variable nameX)
        $ Negated
        $ Quantified
        $ AggregationApplication
            { aggName="Sigma"
            , varName=nameY
            , domainMin=domainMin
            , domainMax=domainMin + 5
            , aggBody=ApplyFunc
                FunctionApplication
                    { funcName="succ"
                    , subscriptExpr=Nothing
                    , superscriptExpr=Nothing
                    , argumentExpr=Infix "+" (Variable nameX) (Variable nameY)
                    }
            })
    == Right (x - sum [succ (x + y) | y <- [domainMin..domainMin+5]])


return [] -- TemplateHaskell splice for quickCheckAll

main = $quickCheckAll
