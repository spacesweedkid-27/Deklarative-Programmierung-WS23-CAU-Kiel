import Text.XHtml (base, background, name)
--import Text.XHtml (base)

-- 1.1
-- define data type JSON
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
