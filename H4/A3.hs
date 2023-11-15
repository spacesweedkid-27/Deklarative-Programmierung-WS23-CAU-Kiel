-- A3 Rose Trees
data Rose a = Rose a [Rose a]
    deriving Show

-- create instance so that we can find out whether two rose trees are equal or not
instance Eq a => Eq (Rose a) where
    -- no different cases needed since the data type only has one constructor
    (Rose x xs) == (Rose y ys) = x == y && xs == ys

-- create instance to make trees comparable, if the datatype a
instance Ord a => Ord (Rose a) where
    compare (Rose x xs) (Rose y ys)
        | compare x y == EQ = compare xs ys
        | otherwise         = compare x y

-- testing data
tree1 :: Rose Int
tree1 = Rose 1 [Rose 2 [], Rose 3 [Rose 4 [], Rose 5 []], Rose 6 []]

tree2 :: Rose Int
tree2 = Rose 1 [Rose 2 [], Rose 3 [Rose 4 [], Rose 5 []], Rose 6 []]

tree3 :: Rose Int
tree3 = Rose 1 [Rose 2 [], Rose 3 [Rose 4 [], Rose 5 []], Rose 7 []]