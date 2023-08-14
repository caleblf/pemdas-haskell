module Pemdas.Definitions
    ( coreBinOps
    , floatingBinOps
    , integralBinOps
    , floatingFunctions
    , floatingEnv
    , coreAggregations
    , doubleLanguage
    )
where
import Pemdas.Types
    ( BinOp(BinOp)
    , Function
    , Aggregation
    , Definitions(Definitions, binOps, functions, aggregations, env)
    , Result
    )
import Control.Monad (when)

-- Helpers

cantFail2 :: (a -> b -> c) -> (a -> b -> Result c)
cantFail2 f x y = return $ f x y

withOptionalSubscript ::
    String -> (Maybe a -> a -> Result a) -> (String, Function a)
withOptionalSubscript name wrappedFunc =
    (name, inner)
    where
        inner subscript Nothing arg =
            wrappedFunc subscript arg
        inner _ (Just _) _ =
            fail $ "\\" ++ name ++ " does not accept superscript"

withOptionalSuperscript ::
    String -> (Maybe a -> a -> Result a) -> (String, Function a)
withOptionalSuperscript name wrappedFunc =
    (name, inner)
    where
        inner Nothing superscript arg =
            wrappedFunc superscript arg
        inner (Just _) _ _ =
            fail $ "\\" ++ name ++ " does not accept subscript"

plainOnly :: String -> (a -> Result a) -> (String, Function a)
plainOnly name wrappedFunc =
    (name, inner)
    where
        inner Nothing Nothing arg = wrappedFunc arg
        inner _ _ _ =
            fail $ "\\" ++ name ++ " does not accept subscript or superscript"


-- Function and global definitions for calculators

-- Binary operators
coreBinOps :: Num a => [(String, BinOp a)]
coreBinOps =
    [ ("+", BinOp 6 (cantFail2 (+)))
    , ("-", BinOp 6 (cantFail2 (-)))
    , ("*", BinOp 7 (cantFail2 (*)))
    ]

floatingBinOps :: Floating a => [(String, BinOp a)]
floatingBinOps =
    [ ("/",  BinOp 7 (cantFail2 (/)))
    , ("**", BinOp 8 (cantFail2 (**)))
    ]

integralBinOps :: Integral a => [(String, BinOp a)]
integralBinOps =
    [ ("%"
      , BinOp 7 (\m n -> do
                    when (n == 0) $ fail "modulo 0"
                    return $ mod m n))
    , ( "//"
      , BinOp 7 (\m n -> do
                    when (n == 0) $ fail "division by 0"
                    return $ div m n)
      )
    , ( "^"
      , BinOp 8 (\m n -> do
                    when (n < 0) $ fail "negative exponent"
                    return (m ^ n))
      )
    ]

-- Functions

-- TODO pull out some repeated logic here
floatingFunctions :: Floating a => [(String, Function a)]
floatingFunctions =
    [ withOptionalSuperscript "sin"
        (\maybeSuper arg ->
            return $ case maybeSuper of
                Nothing -> sin arg
                Just sub -> sin arg ** sub
        )
    , withOptionalSuperscript "cos"
        (\maybeSuper arg ->
            return $ case maybeSuper of
                Nothing -> cos arg
                Just sub -> cos arg ** sub
        )
    , withOptionalSuperscript "tan"
        (\maybeSuper arg ->
            return $ case maybeSuper of
                Nothing -> tan arg
                Just sub -> tan arg ** sub
        )
    , plainOnly "exp" (return <$> exp)
    , withOptionalSuperscript "sqrt"
        (\maybeSuper arg ->
            return $ case maybeSuper of
                Nothing -> sqrt arg
                Just super -> arg ** (1 / super)
        )
    , withOptionalSubscript "log"
        (\maybeSub arg ->
            return $ case maybeSub of
                Nothing -> log arg
                Just sub -> logBase sub arg
        )
    ]

floatingEnv :: Floating a => [(String, a)]
floatingEnv =
    [ ("pi", pi)
    , ("e", exp 1)
    ]

-- TODO add more of these (max, argmax, etc)

coreAggregations :: Num a => [(String, Aggregation a)]
coreAggregations =
    [ ("Sigma", return . sum . map snd)
    , ("Pi",    return . product . map snd)
    ]


doubleLanguage :: Definitions Double
doubleLanguage = Definitions
    { binOps       = coreBinOps ++ floatingBinOps
    , functions    = floatingFunctions
    , aggregations = coreAggregations
    , env          = floatingEnv
    }
