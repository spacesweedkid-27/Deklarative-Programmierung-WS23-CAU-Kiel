import Data.Ratio
import Data.List

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

-- A2
-- n is an avalanche number of i,j,k exist in a way so that n = 5^i*7^j*11^k

-- sort function, modified to remove duplicates as well (because I dont feel like checking
-- if duplicates can exist in avalanche list)
qsort :: [Integer] -> [Integer]
qsort [] = []
qsort (x:xs) =
    qsort (filter (< x) xs) ++ [x] ++ qsort (filter (> x) xs)


-- merge function in order to merge the lists w
merge :: [Integer] -> [Integer] -> [Integer]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | x > y = y : merge (x:xs) ys
    | otherwise = merge (x:xs) ys

merged :: [Integer] -> [Integer] -> [Integer]
merged [] [] = []
merged [] ys = ys
merged xs [] = xs
merged (x:xs) (y:ys)
    | x < y = x : merged xs (y:ys)
    | x >= y = y : merged (x:xs) ys
    | otherwise = merged (x:xs) ys

--avalanche :: [Integer]
--avalanche = 

---- I HAVE NO IDEA WHAT I AM DOING ANYMORE

avpow :: Integer -> [Integer]
avpow n = map (n^) [0..]

avfives :: [Integer]
avfives = map (5^) [0..]

listfive :: Integer -> [Integer]
listfive n = take (fromInteger n) avfives

nthfive :: Integer -> Integer
nthfive n = avfives !! (fromInteger n)

avseven :: [Integer]
avseven = map (7^) [0..]

listseven :: Integer -> [Integer]
listseven n = take (fromInteger n) avseven

nthseven :: Integer -> Integer
nthseven n = avseven !! (fromInteger n)

aveleven :: [Integer]
aveleven = map (11^) [0..]

listeleven :: Integer -> [Integer]
listeleven n = take (fromInteger n) aveleven

ntheleven :: Integer -> Integer
ntheleven n = aveleven !! (fromInteger n)

-- berechnet das spezifische Produkt
avprod :: Integer -> Integer -> Integer -> Integer
avprod n1 n2 n3 = nthfive n1 * nthseven n2 * ntheleven n3

-- avprod startet mit 0 0 0
-- mit counter hoch iterieren
-- das minimum von den nächsten n^2 ergebnissen ausrechnen

-- berechnet alle Produkte bis zu einem festgelegten n
avlist :: Integer -> [Integer]
avlist n = avmerge (map nthfive [0..n]) (map nthseven [0..n]) (map ntheleven [0..n])

avlistdupl :: Integer -> [Integer]
avlistdupl n = avmergedupl (map nthfive [0..n]) (map nthseven [0..n]) (map ntheleven [0..n])

-- funktion bekommt ein n und er berechnet alle av-produkte bis zu diesem Punkt

avmerge :: [Integer] -> [Integer] -> [Integer] -> [Integer]
avmerge [] [] [] = []
avmerge xs [] [] = xs
avmerge [] ys [] = ys
avmerge [] [] zs = zs
avmerge xs ys [] = merge xs ys
avmerge xs [] zs = merge xs zs
avmerge [] ys zs = merge ys zs
avmerge xs ys zs = merge xs (merge ys zs)

avmergedupl :: [Integer] -> [Integer] -> [Integer] -> [Integer]
avmergedupl [] [] [] = []
avmergedupl xs [] [] = xs
avmergedupl [] ys [] = ys
avmergedupl [] [] zs = zs
avmergedupl xs ys [] = merged xs ys
avmergedupl xs [] zs = merged xs zs
avmergedupl [] ys zs = merged ys zs
avmergedupl xs ys zs = merged xs (merged ys zs)

-- A3

-- untilizing monad in this case, because why not
allCombinations :: Eq a => [a] -> [[a]]
allCombinations xs = sequence =<< iterate (xs :) []