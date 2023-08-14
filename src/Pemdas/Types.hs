{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pemdas.Types
    ( Expr(..)
    , BinOp(..)
    , Function
    , FunctionApplication(..)
    , Aggregation
    , AggregationApplication(..)
    , Definitions(..)
    , Result(..)
    , evaluateIn
    , render
    )
where
import Control.Monad (forM)


newtype Result a = Result { getEither :: Either String a }
    deriving (Eq, Show, Functor, Applicative, Monad)
instance MonadFail Result where fail s = Result $ Left s


data Num a => Definitions a = Definitions
    { binOps :: [(String, BinOp a)]
    , functions :: [(String, Function a)]
    , aggregations :: [(String, Aggregation a)]
    , env :: [(String, a)]
    }


data Num a => Expr a =
    Literal a
    | Variable String
    | Negated (Expr a)
    | Infix String (Expr a) (Expr a)
    | ApplyFunc (FunctionApplication a)
    | Quantified (AggregationApplication a)

data BinOp a = BinOp { opPrecedence :: Int, opFunc :: a -> a -> Result a }
type Function a =
    Maybe a     -- ^ Subscript argument
    -> Maybe a  -- ^ Superscript argument
    -> a        -- ^ Argument
    -> Result a -- ^ Result of function, or failure if arguments were bad
data FunctionApplication a = FunctionApplication
    { funcName :: String
    , subscriptExpr :: Maybe (Expr a)
    , superscriptExpr :: Maybe (Expr a)
    , argumentExpr :: Expr a
    }
type Aggregation a = [(Integer, a)] -> Result a
data AggregationApplication a = AggregationApplication
    { aggName :: String
    , varName :: String
    , domainMin :: Integer
    , domainMax :: Integer
    , aggBody :: Expr a
    }


-- TODO Implementing this w/ a pretty printer library would offer more
-- efficiency and options (e.g. whether to show parentheses)
render :: (Num a, Show a) => Expr a -> String
render (Literal x) = show x
render (Variable name) = name
render (Negated expr) = "-" ++ render expr
render (Infix opText leftExpr rightExpr) =
    "(" ++ render leftExpr ++ " " ++ opText ++ " " ++ render rightExpr ++ ")"
render
    (ApplyFunc FunctionApplication
        { funcName
        , subscriptExpr
        , superscriptExpr
        , argumentExpr
        }) =
    "\\"
    ++ funcName
    -- TODO maybe showsPrec can simplify this?
    ++ case subscriptExpr of
        Nothing -> ""
        Just (Literal x) -> "_" ++ show x
        Just (Variable name) -> "_" ++ name
        Just expr -> "_{" ++ render expr ++ "}"
    ++ case superscriptExpr of
        Nothing -> ""
        Just (Literal x) -> "^" ++ show x
        Just (Variable name) -> "^" ++ name
        Just expr -> "^{" ++ render expr ++ "}"
    ++ "(" ++ render argumentExpr ++ ")"
render
    (Quantified AggregationApplication
        { aggName
        , varName
        , domainMin
        , domainMax
        , aggBody
        }) =
    "\\" ++ aggName ++ "_{" ++ varName ++ " \\in ["
    ++ show domainMin ++ ".." ++ show domainMax ++ "]}("
    ++ render aggBody ++ ")"


evaluateIn :: Num a => Definitions a -> Expr a -> Result a
evaluateIn _ (Literal x) = return x
evaluateIn (Definitions {env}) (Variable name) =
    case lookup name env of
        Just x -> return x
        Nothing -> fail $ "Undefined variable: " ++ name
evaluateIn defs (Negated expr) = negate <$> evaluateIn defs expr
evaluateIn defs@(Definitions {binOps}) (Infix opText leftExpr rightExpr) =
    do
        opFunc <- case lookup opText binOps of
            Just (BinOp {opFunc}) -> return opFunc
            Nothing -> fail $ "Unknown infix operator: " ++ opText
        leftArg <- evaluateIn defs leftExpr
        rightArg <- evaluateIn defs rightExpr
        opFunc leftArg rightArg
evaluateIn defs@(Definitions {functions}) (ApplyFunc funcApp) =
    do
        func <- case lookup (funcName funcApp) functions of
            Just f -> return f
            Nothing -> fail $ "Unknown function: " ++ funcName funcApp
        let evalMaybeExpr Nothing = return Nothing
            evalMaybeExpr (Just expr) = return <$> evaluateIn defs expr
        subscript <- evalMaybeExpr $ subscriptExpr funcApp
        superscript <- evalMaybeExpr $ superscriptExpr funcApp
        arg <- evaluateIn defs $ argumentExpr funcApp
        func subscript superscript arg
evaluateIn defs@(Definitions {aggregations, env}) (Quantified aggregationApp) =
    do
        aggFunc <- case lookup (aggName aggregationApp) aggregations of
            Just f -> return f
            Nothing -> fail $ "Unknown aggregation: " ++ aggName aggregationApp
        let domain = [domainMin aggregationApp..domainMax aggregationApp]
            name = varName aggregationApp
            bodyExpr = aggBody aggregationApp
        bodyValues <-
            forM domain $ \arg ->
                evaluateIn (defs {env = (name, fromIntegral arg):env}) bodyExpr
        aggFunc $ zip domain bodyValues
