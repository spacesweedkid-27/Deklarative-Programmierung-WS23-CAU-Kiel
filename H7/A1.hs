-- A1

data Tree n l = Leaf l | Node (Tree n l) n (Tree n l)
    deriving Show

-- A1.1
-- Geben Sie Functor, Applicative und Monad-Instanzen für Tree n an

instance Functor (Tree n) where
    fmap f (Leaf l)             = Leaf (f l)
    fmap f (Node left n right)  = Node (fmap f left) n (fmap f right)

instance Applicative (Tree n) where
    pure                            = Leaf
    (Leaf l) <*> tree               = fmap l tree
    (Node left n right) <*> tree    = Node (left <*> tree) n (right <*> tree)

instance Monad (Tree n) where
    return                  = pure
    Leaf l >>= f            = f l
    Node left n right >>= f = Node (left >>= f) n (right >>= f)

-- A1.2
{-
PROOF FOR MAYBE FUNCTOR

DEFINITIONS:
DA:  instance Functor Maybe where
        fmap = mapMaybe
DB:  mapMaybe :: (a -> b) -> Maybe a -> Maybe b
        mapMaybe f Nothing = Nothing             (DB 1)
        mapMaybe f (Just x) = Just (f x)         (DB 2)

TO PROOF:
(1) fmap id      = id
(2) fmap (f.g)   = fmap f . fmap g

PROOF:
(1)
For case 1: m = Nothing
  fmap id m             | definition of m
= fmap id Nothing       | DA 1
= mapMaybe id Nothing   | DB 1
= Nothing               | definition of m
= m

For case 2: m = Just x
  fmap id m
= fmap id (Just x)      | DA 1
= mapMaybe id (Just x)  | DB 2                  (*), remember for applicative proof)
= Just (id x)           | definition of id
= Just x                | definition of m
= m

(2)
For case 1: m = Nothing
firstly we show fmap (f . g) m = Nothing, then we show (fmap f . fmap g) m = Nothing.
This then proofs fmap (f . g) m = (fmap f . fmap g) m

  fmap (f . g) m            | definition of m
= fmap (f . g) Nothing      | DA 1
= mapMaybe (f . g) Nothing  | DB 1
= Nothing                   | definition of m
= m

  (fmap f . fmap g) m               | definition of m
= (fmap f . fmap g) Nothing         | DA 1
= (mapMaybe f . mapMaybe g) Nothing | (.)
= mapMaybe f (mapMaybe g Nothing)   | DB 1
= mapMaybe f Nothing                | DB 1
= Nothing                           | definition of m
= m

For case 2: m = Just x
as above, there is two parts to this proof.

  fmap (f . g) m                        | definition of m
= fmap (f . g) (Just x)                 | DA 1
= mapMaybe (f . g) (Just x)             | DB 2
= Just ((f . g) x)

  (fmap f . fmap g) m                   | definition of m
= (fmap f . fmap g) (Just x)            | DA 1
= (mapMaybe f . mapMaybe g) (Just x)    | (.)
= mapMaybe f (mapMaybe g (Just x))      | DB 2
= mapMaybe f (Just (g x))               | DB 2
= Just ((f . g) x)


PROOF FOR MAYBE APPLICATIVE

DEFINITION:
DA: instance Applicative Maybe where
        pure                    = Just          (DA 1)
        Nothing <*> _           = Nothing       (DA 2)
        (Just f) <*> Nothing    = Nothing       (DA 3)
        (Just f) <*> (Just x)   = Just (f x)    (DA 4)

DB: instance Applicative Maybe where
        pure            = Just                  (DB 1)
        Nothing <*> _   = Nothing               (DB 2)
        (Just f) <*> mx = fmap f mx             (DB 3)

in the following we will treat fmap as if it is defined like mapMaybe

TO PROOF:
(1) pure id <*> v       = v
(2) pure f <*> pure x   = pure (f x)

PROOF:
(1)
For case 1: v = Nothing

  pure id <*> v         | definition of v
= pure id <*> Nothing   | DB 1
= (Just id) <*> Nothing | DB 2
= Nothing               | definition of v
= v

For case 2: v = Just x

  pure id <*> v         | definition of v
= pure id <*> (Just x)  | DB 1
= (Just id) <*> (Just x)| DB 3
= fmap id (Just x)      | (*)
= Just (id x)           | definition of id
= Just x
= v

(2)
  pure f <*> pure x                 | DA 1
= (Just f) <*> (Just x)             | DA 4
= Just (f x)                        | DA 1
= pure (f x)
-}