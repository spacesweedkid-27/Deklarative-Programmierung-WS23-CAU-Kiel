import Data.Ratio

-- A1.1

-- infinite list of fibonacci numbers
-- call with take n $ fibonacci for a list of the first n fibonacci numbers
fibonacci :: [Integer]
fibonacci = fibgen 0 1

fibgen :: Integer -> Integer -> [Integer]
fibgen n1 n2 = n1 : fibgen n2 (n1 + n2)

nthfib :: Integer -> Integer
nthfib n = fibonacci !! (fromInteger n)

--nextfib :: Integer -> Integer
--nextfib n = nthfib (n+1)

-- A1.2

-- initite list of approximates for golden ratio
-- calls grgen for operation
goldenRatio :: [Rational]
goldenRatio = grgen 1 1

grgen :: Integer -> Integer -> [Rational]
grgen n1 n2 = n2 % n1 : grgen n2 (n1 + n2)

-- A1.3
approx :: Rational -> [Rational] -> Rational
-- ripple through list (x1:x2:xs) and calculate abs (x2 - x1) (-> abs: absolute value)
-- if abs (x2-x1) < eps, then result found
-- otherwise call approx for (x2:xs)
approx eps (x1:x2:xs) = if abs (x2 - x1) < eps then x2 else approx eps (x2:xs)