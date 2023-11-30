-- that moment when it all finally makes sense...

-- 1)
getNonEmptyLine :: IO String
getNonEmptyLine = getLine >>= \x ->
    if null x then putStrLn "Please enter a non-empty string." >> getNonEmptyLine
    else putStrLn x >> return x

-- 2)
quitOrInput :: IO (Maybe String)
quitOrInput = do
    putStrLn "Your input"
    input <- getLine
    if input == ":q" then return Nothing
    else return (Just input)