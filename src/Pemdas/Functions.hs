module Pemdas.Functions
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
    ( BinOp
    , Function
    , Aggregation
    , Language(Language, binOps, functions, aggregations, env)
    )

-- Helpers

cantFail2 :: (a -> b -> c) -> (a -> b -> Either String c)
cantFail2 f x y = Right $ f x y


withOptionalSubscript ::
    String -> (Maybe a -> a -> Either String a) -> (String, Function a)
withOptionalSubscript name wrappedFunc =
    (name, inner)
    where
        inner _ (Just _) _ =
            Left $ "\\" ++ name ++ " does not accept superscript"
        inner subscript Nothing arg =
            wrappedFunc subscript arg

withOptionalSuperscript ::
    String -> (Maybe a -> a -> Either String a) -> (String, Function a)
withOptionalSuperscript name wrappedFunc =
    (name, inner)
    where
        inner (Just _) _ _ =
            Left $ "\\" ++ name ++ " does not accept subscript"
        inner Nothing superscript arg =
            wrappedFunc superscript arg

plainOnly :: String -> (a -> Either String a) -> (String, Function a)
plainOnly name wrappedFunc =
    (name, inner)
    where
        inner Nothing Nothing arg = wrappedFunc arg
        inner _ _ _ =
            Left $ "\\" ++ name ++ " does not accept subscript or superscript"


-- Function and global definitions for calculators

-- Binary operators
coreBinOps :: Num a => [(String, BinOp a)]
coreBinOps =
    [ ("+", (6, cantFail2 (+)))
    , ("-", (6, cantFail2 (-)))
    , ("*", (7, cantFail2 (*)))
    ]
floatingBinOps :: Floating a => [(String, BinOp a)]
floatingBinOps =
    [ ("/",  (7, cantFail2 (/)))
    , ("**", (8, cantFail2 (**)))
    ]
integralBinOps :: Integral a => [(String, BinOp a)]
integralBinOps =
    [ ("%", (7, \m n -> if n == 0 then Left "modulo 0" else Right $ mod m n))
    , ( "//"
      , (7, \m n -> if n == 0 then Left "division by 0" else Right $ div m n)
      )
    , ( "^"
      , (8, \m n -> if n < 0 then Left "negative exponent" else Right (m ^ n))
      )
    ]

-- Functions

floatingFunctions :: Floating a => [(String, Function a)]
floatingFunctions =
    [ withOptionalSuperscript "sin"
        (\maybeSub arg ->
            Right $ case maybeSub of
                Nothing -> sin arg
                Just sub -> sin arg ** sub
        )
    , withOptionalSuperscript "cos"
        (\maybeSub arg ->
            Right $ case maybeSub of
                Nothing -> cos arg
                Just sub -> cos arg ** sub
        )
    , withOptionalSuperscript "tan"
        (\maybeSub arg ->
            Right $ case maybeSub of
                Nothing -> tan arg
                Just sub -> tan arg ** sub
        )
    , plainOnly "exp" (Right <$> exp)
    , withOptionalSuperscript "sqrt"
        (\maybeSuper arg ->
            Right $ case maybeSuper of
                Nothing -> sqrt arg
                Just super -> arg ** (1 / super)
        )
    , withOptionalSubscript "log"
        (\maybeSub arg ->
            Right $ case maybeSub of
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
    [ ("Sigma", Right . sum . map snd)
    , ("Pi",    Right . product . map snd)
    ]


doubleLanguage :: Language Double
doubleLanguage = Language
    { binOps       = coreBinOps ++ floatingBinOps
    , functions    = floatingFunctions
    , aggregations = coreAggregations
    , env          = floatingEnv
    }
