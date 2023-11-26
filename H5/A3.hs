-- A3

-- untilizing monad in this case, because why not
allCombinations :: Eq a => [a] -> [[a]]
allCombinations xs = sequence =<< iterate (xs :) []