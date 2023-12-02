import Prelude hiding (filter, map, lookup, replicate)
import Data.Maybe (listToMaybe)

-- 1)
list1 :: [[Int]]
list1 = [[(2*i)+1] | i <- [1 .. 5]]

list2 :: [(Int, Bool)]
list2 = [(5 * i, even (5 * i)) | i <- [1 .. 5], i `elem` [1, 4, 5]]

list3 :: [Maybe Int]
list3 = [Just (1+ 4 * (i-1)^2 + 4 * (i - 1)) | i <- [1 .. 5], i < 4]    -- created a polynomial² for that

list4 :: [(Int, Int)]
list4 = [(i, 5 - j) | i <- [1 .. 5], j <- [0 .. 4], i < (5-j)]

-- 2)
map :: (a -> b) -> [a] -> [b]
map f subdom = [f x | x <- subdom]  -- names should explain

lookup :: Eq a => a -> [(a,b)] -> Maybe b
lookup key map = listToMaybe [snd e | e <- map, fst e == key]   -- snd and fst to decide between key and value of the (Hash)map

replicate :: Int -> a -> [a]
replicate num elem = [elem | _ <- [1 .. num]]   -- this feels illegal


filter :: (a -> Bool) -> [a] -> [a]
filter cond list = [elem | elem <- list, cond elem] -- map but the other way around and easier
