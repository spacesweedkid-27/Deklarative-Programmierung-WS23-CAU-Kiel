-- ignore
type IntegerSet = Integer -> Bool

-- Heute mal paar Lambdas
-- 1.1)
empty :: IntegerSet
empty = \x -> False

insert :: Integer -> IntegerSet -> IntegerSet
insert = \int set -> (\int2 -> (set int2) || int == int2)

remove :: Integer -> IntegerSet -> IntegerSet
remove = \int set -> (\int2 -> (set int2) && not (int == int2))

isElem :: Integer -> IntegerSet -> Bool
isElem = \int set -> set int

union :: IntegerSet -> IntegerSet -> IntegerSet
union = \a b -> (\num -> a num || b num)

intersection :: IntegerSet -> IntegerSet -> IntegerSet
intersection = \a b -> (\num -> a num && b num)

difference :: IntegerSet -> IntegerSet -> IntegerSet
difference = \a b -> (\num -> a num && not (b num))

complement :: IntegerSet -> IntegerSet
complement = \set -> (\num -> not (set num))

-- 1.2)
-- Ersteres ist möglich, deklaration lässt sich ableiten aus dem Beispiel
listToSet :: [Integer] -> IntegerSet
listToSet = \list -> (\num -> elem num list)
-- Zweiteres ist mehr oder weniger nicht möglich,
-- denn es gibt keine wahrlich richtige Liste zu dem Komplement der leernen Menge,
-- jedoch könnte man auch akzeptieren, dass listen und Integer eine physische Grenze haben
-- und uns einfach auf alle Integer abbilden,
-- jedoch da unendlich existieren ist das Programm nicht deterministisch:
setToList :: IntegerSet -> [Integer]
setToList = \set -> filter set [0..]
-- Als Beispiel lässt sich setToList (complement empty) annähernd berechnen.

--3.1)
-- don't copy
data Rose a = Rose a [Rose a]
  deriving Show

instance (Eq a) => Eq (Rose a) where
    -- Adding type signatures suddently breaks it. Huh?
    -- (==) :: Eq a => Rose a -> Rose a -> Bool
    (Rose val1 rest1) == (Rose val2 rest2)
        | length rest1 /= length rest2 || val1 /= val2 = False
        | not (null rest1) = rest1 == rest2 -- this looks cursed
        | otherwise = True

-- create instance to make trees comparable, if the datatype a
instance Ord a => Ord (Rose a) where
    compare (Rose x xs) (Rose y ys)
        | compare x y == EQ = compare xs ys
        | otherwise         = compare x y

-- A3.2

class Pretty a where
  pretty :: a -> String
  preTree :: a -> String -> String

instance Pretty a => Pretty (Rose a) where
  preTree :: Rose a -> String -> String
  preTree (Rose x []) ind = ind ++ "+-- " ++ pretty x ++ "\n"
  preTree (Rose x children) ind =
      ind ++ "+-- " ++ pretty x ++ "\n" ++
      concatMap (preTree' (ind ++ "| ")) children
    where
      preTree' :: String -> Rose a -> String
      preTree' ind r = preTree r ind
  
  pretty :: a -> String
  pretty = show
{-
instance Pretty a => Pretty (Rose a) where
  -- for first rose print value as String
  -- for each child print "+--" and then the value as a string
  -- for each indentation after the first print "|" instead
  preTree :: String -> a -> String
  preTree ind (Rose x []) = ind ++ "+--" ++ pretty x ++ "\n"
  preTree (Rose x children) ind =
      ind ++ "+-- " ++ pretty x ++ "\n" ++ concatMap (preTree' "| " ind) children
    where
      preTree' :: String -> Rose a -> String
      preTree' ind r = preTree r ind
    
  -- pretty tree = pretty tree ""
  -}

example :: Rose Integer
example = Rose 4 [Rose 5 [Rose 1 [], Rose 2 [Rose 7 [], Rose 8 []], Rose 3 []], Rose 6 []]