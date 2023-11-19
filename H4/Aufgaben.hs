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



example :: Rose Integer
example = Rose 4 [Rose 5 [Rose 1 [], Rose 2 [Rose 7 [], Rose 8 []], Rose 3 []], Rose 6 []]