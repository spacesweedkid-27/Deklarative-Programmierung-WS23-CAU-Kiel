crown :: Int -> Int -> String
crown num pos = replicate (num - pos) ' ' ++ replicate (2 * pos - 1) '*'    -- use only odd numbers as length of the '*'-chains

putStrArLn :: [String] -> IO ()
putStrArLn = foldr ((>>) . putStrLn) (do return ())                         -- use foldr instead of going through recursively manual, "concat" 2 IO () functions to concat with the left.

tree :: Int -> IO ()
tree num
    | num < 3 = do return ()
    | otherwise = do
        putStrArLn [crown num pos | pos <- [1 .. num]]                      -- print all lines of the crown
        putStrArLn (replicate (num - 2) (replicate (num - 1) ' ' ++ "*"))   -- print a stump '*' centered num - 2 times