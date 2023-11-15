type IntegerSet = Integer -> Bool

-- A1.1


-- function that maps a specific list of IntegerElemets into a set
lostSet :: IntegerSet
lostSet = \x -> elem x [4, 8, 15, 16, 23, 42]

unionSet1 :: IntegerSet
unionSet1 = \x -> elem x [1,2,3]

unionSet2 :: IntegerSet
unionSet2 = \x -> elem x [3,4,5]


--------------------------------------------------------------------------------
-- hand in from here on

-- empty represents the empty set
empty :: IntegerSet
-- if the set is empty, it always returns false
empty _ = False

-- inserts an integer into a IntegerSet and returns an IntegerSet
insert :: Integer -> IntegerSet -> IntegerSet
insert x ogSet = \y -> if y == x then True else ogSet x

-- removes an integer from an Integerset and returns the IntegerSet
remove :: Integer -> IntegerSet -> IntegerSet
remove x ogSet = \y -> if y == x then False else ogSet y

-- checks whether an Integer is element of an IntegerSet
-- which is kinda dumb, since IntegerSet itself returns
-- a bool value if called with an integer
isElem :: Integer -> IntegerSet -> Bool
isElem x ogSet = ogSet x

union :: IntegerSet -> IntegerSet -> IntegerSet
-- for safety reasons defined for empty sets
union s1 empty = s1
union empty s2 = s2
-- given two IntegerSets
-- for each value in first set
    -- check each value in second set
    -- if a value is found that matches
        -- add it to a new set
    -- otherwise return empty
-- why u wont work
-- i have no thoughts left in my brain
union s1 s2 = \x -> s1 x || s2 x

--TODO
intersection :: IntegerSet -> IntegerSet -> IntegerSet

difference :: IntegerSet -> IntegerSet -> IntegerSet

complement :: IntegerSet -> IntegerSet -> IntegerSet

-- testing data, no need to hand in this part
main :: IO ()
main = do
    putStrLn $ show (lostSet 2) -- false
    putStrLn $ show (lostSet 42) -- true
    let updatedSet = insert 2 lostSet
        removeSet = remove 42 lostSet
        wasemptySet = insert 2 empty
        unionSet3 = union unionSet1 unionSet2

    putStrLn $ show (updatedSet 2)  -- true
    putStrLn $ show (lostSet 3) -- False
    putStrLn $ show (lostSet 16) -- True
    putStrLn $ show (removeSet 16) -- True
    putStrLn $ show (removeSet 42) -- False
    putStrLn $ show (wasemptySet 2) -- true
    putStrLn $ show (wasemptySet 3) -- False
    putStrLn $ show (unionSet3 2) -- False
    putStrLn $ show (unionSet3 3) -- True

-- A1.2