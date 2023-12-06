{-
DEFINITIONS.:
    DA)
        pure :: Monad m => a -> m a
        pure = return
    
    DB)
        (<*>) :: Monad m => m (a -> b) -> m a ->  m b
        ff <*> fx = ff >>= \f -> fx >>= \x -> return (f x)

ASSUMPTIONS.:
    A) return x  >>= f      = f x                     -- Left Identity
    B) m         >>= return = m                       -- Right Identity
    C) (m >>= f) >>= g      = m >>= (\x -> f x >>= g) -- Associativity

TO PROVE.:
    1) pure id <*> v = v                              -- Identity
    2) pure f <*> pure x = pure (f x)                 -- Homomorphism

PROOF.:
    1)
          pure id <*> v                                 | DA
        = return id <*> v                               | DB
        = return id >>= \f -> v >>= \x -> return (f x)  | A
        = v >>= \x -> return (id x)                     | Calculate, id x = x
        = v >>= return                                  | B
        = v                                             ■
    2)
          pure f <*> pure x                                 | DB
        = pure f >>= \g -> pure >>= \a -> return (g a) x    | DA
        = return f >>= \g -> pure >>= \a -> return (g a) x  | A
        = \g -> pure >>= \a -> return (g a) x f             | Calculate
        = pure >>= \a -> return (f a) x                     | DA
        = return >>= \a -> return (f a) x                   | Calculate
        = return x >>= \a -> return (f a)                   | A
        = \a -> return (f a) x                              | Calculate
        = return (f x)                                      | DA
        = pure (f x)                                        ■
    
    With that everything that had to be proved has been proven. □
-}