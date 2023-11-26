-- A2
-- Devides integers but returns 0 if there is remainder.
div2 :: Integer -> Integer -> Integer
div2 a b
    | mod a b == 0 = div a b
    | otherwise = 0

-- here we categorize the idea we had from the week before, we define a function
-- that answers to an integer, if it is an avalanche number.
-- this works because of the following thought:
-- 5, 7 and 11 are prime numbers and every number can be split into one set of prime factors
-- so to speak, we look for numbers that have 5, 7 OR 11 as prime factors.
-- Now we can define the set iteratively:
-- We say that 0 is not in the set (because there is no number for i,j and k so that n = 0)
-- And we say that 1 is in the set.
-- The inductive step is the following:
--      If a number a is in the Set,
--      then 5a, 7a and 11a are in the set.
-- These three categories we can abuse in the other direction.
-- We need div2 so that if a number is not divisable by one of the three, then we already know
-- that it is not in the set and we say then it's 0 to catch the avalanceSet 0 case.
avalancheSet :: Integer -> Bool
avalancheSet 0 = False
avalancheSet 1 = True
avalancheSet n
    | avalancheSet (div2 n 5) = True
    | avalancheSet (div2 n 7) = True
    | avalancheSet (div2 n 11) = True
    | otherwise = False

-- This is the same idea we had last week basically.
-- The cool thing with this is, that we don't even have to sort,
-- because the numbers are already sorted.
avalanche3 :: [Integer]
avalanche3 = filter avalancheSet [0..]
