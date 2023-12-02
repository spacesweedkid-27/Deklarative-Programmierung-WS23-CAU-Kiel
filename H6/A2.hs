import Data.Char (isAlpha, toLower)

-- may have issues displaying in a shell, but the duplication error has been fixed.

-- "converts" string to star version while not hiding known chars
filterSecretString :: String -> String -> String
filterSecretString [] _ = []    -- induction start
filterSecretString (k : ks) known
    | k `elem` known = k : filterSecretString ks known  -- if in alphabet, do use that char
    | otherwise = '*' : filterSecretString ks known     -- if not, then use '*'

-- implemented iteratively
hangman :: String -> IO ()
hangman word = routine word "" 0
    where
        routine :: String -> String -> Integer -> IO ()     -- we define a helper function that stores the word to guess, the known alphabet and the number of turns
        routine "" _ _ = putStrLn "Please enter a word."    -- basically show args
        routine word guessed steps = do
            if filterSecretString word guessed == word      -- if the string that is known by guessing is the same is it should,
                then putStrLn ("Solved in " ++ show steps ++ " tries.") -- end the game
            else do
                putStrLn ("Secret: " ++ filterSecretString word guessed)    -- display the currently guessed word
                putStrLn "Enter a character: "  -- and give instructions
                charGuessed <- getChar          -- read from stdin
                if not (isAlpha charGuessed)    -- if not valid input
                    then putStrLn "Exited because you don't know the rules, or because you want to." -- print and exit
                else if charGuessed == '\n'                            -- this
                    then routine word (guessed ++ [charGuessed]) steps -- and this fix the double receiving issue, but in my shell at least it's still being printed double
                else if toLower charGuessed `elem` word     -- if the user guesses a letter right
                    then routine word (guessed ++ [charGuessed]) (steps + 1)    -- increase step-count and add the char to the known alphabet
                else routine word guessed (steps + 1)       -- if they miss, then increase, but don't add anything to the alphabet