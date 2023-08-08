{-# LANGUAGE NamedFieldPuns #-}
module Pemdas.Types
    ( Expr(..)
    , BinOp
    , Function
    , FunctionApplication(..)
    , Aggregation
    , AggregationApplication(..)
    , evaluateIn
    , Language(..)
    )
where


data Num a => Language a = Language
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

type BinOp a = (Int, a -> a -> Either String a)  -- (precedence, logic)
type Function a = (Maybe a -> Maybe a -> a -> Either String a)
data FunctionApplication a = FunctionApplication
    { funcName :: String
    , subscriptExpr :: Maybe (Expr a)
    , superscriptExpr :: Maybe (Expr a)
    , argumentExpr :: Expr a
    }
type Aggregation a = ([(Integer, a)] -> Either String a)
data AggregationApplication a = AggregationApplication
    { aggName :: String
    , varName :: String
    , domainMin :: Integer
    , domainMax :: Integer
    , aggBody :: Expr a
    }

instance (Num a, Show a) => Show (Expr a) where
    -- Implementing this w/ showsPrec could be more efficient
    show (Literal x) = show x
    show (Variable name) = name
    show (Negated expr) = "-" ++ show expr
    show (Infix opText leftExpr rightExpr) =
        "(" ++ show leftExpr ++ " " ++ opText ++ " " ++ show rightExpr ++ ")"
    show
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
            Just expr -> "_{" ++ show expr ++ "}"
        ++ case superscriptExpr of
            Nothing -> ""
            Just (Literal x) -> "^" ++ show x
            Just (Variable name) -> "^" ++ name
            Just expr -> "^{" ++ show expr ++ "}"
        ++ "(" ++ show argumentExpr ++ ")"
    show
        (Quantified AggregationApplication
            { aggName
            , varName
            , domainMin
            , domainMax
            , aggBody
            }) =
        "\\" ++ aggName ++ "_{" ++ varName ++ " \\in ["
        ++ show domainMin ++ ".." ++ show domainMax ++ "]}("
        ++ show aggBody ++ ")"


evaluateIn :: Num a => Language a -> Expr a -> Either String a
evaluateIn (Language
    { binOps
    , functions
    , aggregations
    , env
    }) = evalInEnv env
    where
    evalInEnv env' =
        let evalExpr (Literal x) = Right x
            evalExpr (Variable name) =
                case lookup name env' of
                    Just x -> Right x
                    Nothing -> Left $ "Undefined variable: " ++ name
            evalExpr (Negated expr) = negate <$> evalExpr expr
            evalExpr (Infix opText leftExpr rightExpr) =
                do
                    opFunc <- case lookup opText binOps of
                        Just (_, op) -> Right op
                        Nothing -> Left $ "Unknown infix operator: " ++ opText
                    leftArg <- evalExpr leftExpr
                    rightArg <- evalExpr rightExpr
                    opFunc leftArg rightArg
            evalExpr
                (ApplyFunc FunctionApplication
                    { funcName
                    , subscriptExpr
                    , superscriptExpr
                    , argumentExpr
                    }) =
                do
                    func <- case lookup funcName functions of
                        Just f -> Right f
                        Nothing -> Left $ "Unknown function: " ++ funcName
                    subscript <- evalMaybeExpr subscriptExpr
                    superscript <- evalMaybeExpr superscriptExpr
                    arg <- evalExpr argumentExpr
                    func subscript superscript arg
            evalExpr
                (Quantified AggregationApplication
                    { aggName
                    , varName
                    , domainMin
                    , domainMax
                    , aggBody
                    }) =
                do
                    aggFunc <- case lookup aggName aggregations of
                        Just f -> Right f
                        Nothing -> Left $ "Unknown aggregation: " ++ aggName
                    let domain = [domainMin..domainMax]
                    bodyValues <-
                        sequence
                            [evalInEnv ((varName, fromIntegral arg):env')
                                aggBody | arg <- domain]
                    aggFunc $ zip domain bodyValues

            evalMaybeExpr Nothing = Right Nothing
            evalMaybeExpr (Just expr) = Just <$> evalExpr expr
        in evalExpr
