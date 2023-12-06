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

-- holds left identity, because return = pure, which enables return x >>= f = pure x >>= f = Value x >>= f = f x
-- holds right identity, because errA >>= return = errA >>= Value = (Value val | Failure str) >>= Value = (Value val | Failure str)
-- holds Associativity, similar proof like right identity
instance Monad Error where
    return = pure

    (>>=) (Value input) func = func input   -- pass if ok
    (>>=) (Failure txt) func = Failure txt  -- if not then don't