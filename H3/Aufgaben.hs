--import Text.XHtml (base, background, name)
--import System.Posix (DL(Null))
-- import Test.QuickCheck
--import Text.XHtml (base)

-- 1.1
-- define data type JSON
-- don't include in submission
data JSON = JNull
          | JBool Bool
          | JInt Int
          | JFloat Float
          | JString String
          | JArray [JSON]
          | JObject [(String, JSON)]
    deriving Show

-- generate test data
exampleJSON :: JSON
exampleJSON = JArray [
    JObject [
            ("name", JString "meier"),
            ("besuchte_kurse", JArray [JString "Logik", JString "Programmierung", JString "Compilerbau"]),
            ("bachelor_note", JNull),
            ("zugelassen", JBool True)
        ],
        JObject [
            ("name", JString "schmidt"),
            ("besuchte_kurse", JArray [JString "Programmierung", JString "Informationssysteme"]),
            ("bachelor_note", JFloat 2.7),
            ("zugelassen", JBool False)
        ]
    ]

-- 1.2
-- foldJSON
foldJSON :: a -> (Bool -> a) -> (Int -> a) -> (Float -> a) -> (String -> a) -> ([a] -> a) -> ([(String, a)] -> a) -> JSON -> a
foldJSON jnull jbool jint jfloat jstring jarray jobject j = case j of
    JNull -> jnull
    JBool b -> jbool b
    JInt n -> jint n
    JFloat f -> jfloat f
    JString s -> jstring s
    JArray arr -> jarray (map (foldJSON jnull jbool jint jfloat jstring jarray jobject) arr)
    JObject objList -> jobject (map (\(key, value) -> (key, foldJSON jnull jbool jint jfloat jstring jarray jobject value)) objList)

-- test objecet for foldJSON
-- foldJSON (0 :: Int) (\b -> if b then 1 else 0) (const 0) (const 0) (const 0) sum(\xs -> sum (map snd xs)) j
-- expected output: 2
j :: JSON
j = JArray [
    JBool True,
    JBool True,
    JBool False
    ]

-- A2
data Tree a = Leaf a | Branch (Tree a) (Tree a)
    deriving (Show, Eq)

-- flatTree flattens a tree of trees to a tree of elements
flatTree :: Tree (Tree a) -> Tree a
flatTree (Leaf t) = t
flatTree (Branch left right) = Branch (flatTree left) (flatTree right)

-- mapTree applies function to each element of a given tree
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf a) = Leaf (f a)
mapTree f (Branch left right) = Branch (mapTree f left) (mapTree f right)

-- foldTree folds given Tree to single value
foldTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b
foldTree f _ (Leaf a) = f a
foldTree f g (Branch left right) = g (foldTree f g left) (foldTree f g right)

-- extendTree extends a given tree with new branches at its leafs
extendTree :: (a -> Tree b) -> Tree a -> Tree b
extendTree f (Leaf a) = f a
extendTree f (Branch left right) = Branch (extendTree f left) (extendTree f right)


-- testing
ex1, ex2, ex3, ex4 :: Bool
ex1 = flatTree (Branch (Leaf (Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3)))) (Leaf (Leaf 4))) == Branch (Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))) (Leaf 4)
ex2 = mapTree (*2) (Branch (Leaf (-2)) (Leaf 1)) == Branch (Leaf (-4)) (Leaf 2)
ex3 = foldTree id max (Branch (Leaf 42) (Leaf 72)) == 72
ex4 = extendTree Leaf (Branch (Branch (Leaf 3) (Leaf 2)) (Leaf 1)) == Branch (Branch (Leaf 3) (Leaf 2)) (Leaf 1)

-- A3
-- modified functions from H2 for Int type
-- could have left it as is, but in case they test with other datatypes and check
-- for compiler error I didn't wanna risk it

reverse1 :: [Int] -> [Int]
reverse1 l = rev l []
    where
        rev []     acc = acc
        rev (x:xs) acc = rev xs (x:acc)

indexOf :: Int -> [Int] -> Maybe Int
indexOf z zs = index 0 z zs
    where
        index _ _ []     = Nothing
        index i x (y:ys) = if x == y then Just i else index (i + 1) x ys

add :: Int -> [[Int]] -> [[Int]]
add x [] = []
add x (xs:xss) = (x:xs) : add x xss

inits :: [Int] -> [[Int]]
inits []     = [[]]
inits (x:xs) = [] : add x (inits xs)

tails :: [Int] -> [[Int]]
tails []     = [[]]
tails (x:xs) = (x:xs) : tails xs
-- fragment of testing
-- tails (x:xs) = (xs) :tails xs

insert :: Int -> [Int] -> [[Int]]
insert x []   = [[x]]
insert x (y:ys) = (x:y:ys) : add y (insert x ys)

perms :: [Int] -> [[Int]]
perms []     = [[]]
perms (x:xs) = permInsert [] (perms xs)
    where
        permInsert acc []       = acc
        permInsert acc (ys:yss) = permInsert (acc ++ insert x ys) yss

-- properties per function

-- perms
-- number of the elements in the returned list n must be the same as the length of the given list l with n = l!
prop_NumberOfPermutations :: [Int] -> Bool
prop_NumberOfPermutations xs = fac (length xs) == length (perms xs)
    where
        fac :: Int -> Int
        fac a
            | a < 2 = 1
            | otherwise = a * fac (a-1)

-- inits
-- number of elements in the returned list must be the same as the length of the given list + 1
--(since the empty list always exists in the result as well)
prop_LengthOfListPlusOne :: [Int] -> Bool
prop_LengthOfListPlusOne xs = length (inits xs) == length xs + 1

-- tails
-- checks if the original list is an element of the output list
prop_ContainsOriginal :: [Int] -> Bool
prop_ContainsOriginal xs = xs `elem` (tails xs)

-- indexOf
-- element at returned index must be equal to element given
prop_elemAtReturnedIndex :: Int -> [Int] -> Bool
prop_elemAtReturnedIndex x xs =
    case indexOf x xs of
        Nothing -> True
        Just y -> xs !! y == x

-- reverse
-- returned list must be exactly as long as the given list
prop_revSameLength :: [Int] -> Bool
prop_revSameLength xs = length (reverse1 xs) == length xs

-- insert
-- result list contains given element
prop_elemInResult :: Int -> [Int] -> Bool
prop_elemInResult x xs = all (elem x) (insert x xs)