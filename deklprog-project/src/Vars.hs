module Vars
  (
    Vars (allVars),
    -- freshVars,
    testVars
  )
where
    import Base.Type
    import Data.Set (toList, fromList)
    
    class Vars a where
      -- Calculate all Variablenames from a Type
      allVars :: a -> [VarName]
    
    removeDuplicates :: Ord a => [a] -> [a]
    removeDuplicates = toList . fromList    -- works

    instance Vars Term where
      allVars input = removeDuplicates (allVars' input)   -- somehow does not remove duplicates
        where
          allVars' :: Term -> [VarName]
          allVars' (Comb str (nextTerm : restTerm)) = allVars' nextTerm ++ allVars' (Comb str restTerm)  -- case Comb constructor, induction step, search for more VarNames and ignore the CombName
          allVars' (Var (VarName varname)) = [VarName varname] -- case Var constructor, induction start, just append this varname
          allVars' (Comb _ []) = []  -- case Comb constructor 2 in case the [Term] is empty, then just append empty list, - also induction start.

    instance Vars Rule where
      allVars (Rule term restTerms) = allVars term ++ concatMap allVars restTerms


    testVars :: IO Bool
    testVars = do
      let t = termVars == map allVars testTerms
      let r = ruleVars == map allVars testRules
      --let p = progVars == allVars testProg
      --let g = goalVars == allVars testGoal
      return (t && r)-- && p && g)
      where
        termVars = [[VarName "A"],[],[VarName "A"],[VarName "Abbot",VarName "B"]]
        testTerms = [Var (VarName "A"), Comb "true" [], Comb "f" [Var (VarName "A"), Comb "true" []], Comb "f" [Comb "1" [], Comb "h" [Comb "g" [Var (VarName "Abbot"), Var (VarName "B")], Comb "[]" []]]]
        ruleVars = [[VarName "X"],[VarName "X"],[VarName "[]",VarName "Ys"],[VarName "X",VarName "Xs",VarName "Ys",VarName "Zs"]]
        testRules = [Rule (Comb "f" [Var (VarName "X"), Comb "true" []]) [], Rule (Comb "f" [Var (VarName "X"), Comb "true" []]) [Comb "g" [Var (VarName "X")], Comb "h" []], Rule (Comb "append" [Var (VarName "[]"), Var (VarName "Ys"), Var (VarName "Ys")]) [], Rule (Comb "append" [Comb "." [Var (VarName "X"), Var (VarName "Xs")], Var (VarName "Ys"), Comb "." [Var (VarName "X"), Var (VarName "Zs")]]) [Comb "append" [Var (VarName "Xs"), Var (VarName "Ys"), Var (VarName "Zs")]]]
        --progVars = [VarName "X",VarName "[]",VarName "Ys",VarName "Xs",VarName "Zs"]
        --testProg = Prog testRules
        --goalVars = [VarName "A",VarName "Abbot",VarName "B"]
        --testGoal = Goal testTerms
