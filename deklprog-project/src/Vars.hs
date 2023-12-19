module Vars
  (
    -- A1)
    Vars (allVars),
    -- A2)
    freshVars,
    testVars
  )
where
    import Base.Type
    import Data.List (nub)
    
    class Vars a where
      -- Calculate all Variablenames from a Type
      allVars :: a -> [VarName]

    instance Vars Term where
      allVars input = nub (allVars' input)   -- somehow does not remove duplicates
        where
          allVars' :: Term -> [VarName]
          allVars' (Comb _ terms) = concatMap allVars' terms  -- used concat for more elegant implementation
          allVars' (Var varName) = [varName]                  -- stays the same

    instance Vars Rule where
      allVars (Rule term restTerms) = nub (allVars term ++ concatMap allVars restTerms) -- we again have to remove duplicates because of the form that is the Rule datastructure.

    instance Vars Prog where
      allVars (Prog lst) = nub (concatMap allVars lst)  -- same concept as in the instance for Rule

    instance Vars Goal where
      allVars (Goal lst) = nub (concatMap allVars lst)  -- same ......

    
    -- made this a while ago, I knew I could use it again :)
    tuples :: String -> [String]
    tuples alphabet = "" : [ c : s | s <- "" : tuples alphabet, c <- alphabet]

    -- get all tuples from the numbers and append a letter left
    freshVars :: [VarName]
    freshVars = [VarName (c : s) | s <- tuples "0123456789", c <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"]


    testVars :: IO Bool
    testVars = do
      let t = termVars == map allVars testTerms
      let r = ruleVars == map allVars testRules
      let p = progVars == allVars testProg
      let g = goalVars == allVars testGoal

      print t
      print r
      print p
      print g

      return (t && r && p && g)
      where
        termVars = [[VarName "A"],[],[VarName "A"],[VarName "Abbot",VarName "B"]]
        testTerms = [Var (VarName "A"), Comb "true" [], Comb "f" [Var (VarName "A"), Comb "true" []], Comb "f" [Comb "1" [], Comb "h" [Comb "g" [Var (VarName "Abbot"), Var (VarName "B")], Comb "[]" []]]]
        ruleVars = [[VarName "X"],[VarName "X"],[VarName "[]",VarName "Ys"],[VarName "X",VarName "Xs",VarName "Ys",VarName "Zs"]]
        testRules = [Rule (Comb "f" [Var (VarName "X"), Comb "true" []]) [], Rule (Comb "f" [Var (VarName "X"), Comb "true" []]) [Comb "g" [Var (VarName "X")], Comb "h" []], Rule (Comb "append" [Var (VarName "[]"), Var (VarName "Ys"), Var (VarName "Ys")]) [], Rule (Comb "append" [Comb "." [Var (VarName "X"), Var (VarName "Xs")], Var (VarName "Ys"), Comb "." [Var (VarName "X"), Var (VarName "Zs")]]) [Comb "append" [Var (VarName "Xs"), Var (VarName "Ys"), Var (VarName "Zs")]]]
        progVars = [VarName "X",VarName "[]",VarName "Ys",VarName "Xs",VarName "Zs"]
        testProg = Prog testRules
        goalVars = [VarName "A",VarName "Abbot",VarName "B"]
        testGoal = Goal testTerms
