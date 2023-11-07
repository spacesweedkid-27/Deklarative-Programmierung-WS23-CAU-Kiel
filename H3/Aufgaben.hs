import Text.XHtml (base, background, name)
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
-- egh, keinen Bock mehr.
