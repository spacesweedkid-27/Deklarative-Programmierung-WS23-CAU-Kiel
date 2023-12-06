data Error a = Failure String | Value a
    deriving (Show, Eq)

-- holds identity and composition
instance Functor Error where
    fmap f (Value input) = Value (f input) 
    fmap _ (Failure stacktr) = Failure stacktr

-- holds identity (bc of pure v = Value v),
-- composition (bc of def. (.)),
-- homomorphism (bc of pure = Value, def. (<*>)),
-- interchange (bc of (<*>) returning a failure only if there is any, else there is only composition)
-- basically same implementation like with Maybe but we have (A String) and not (A) (Failure compared to Nothing)
instance Applicative Error where
    pure = Value    -- straightforward

    (<*>) (Value f) (Value input) = Value (f input)     -- pass if ok
    (<*>) _ (Failure input) = Failure input     -- stop if not
    (<*>) (Failure txt) _ = Failure txt         -- again

