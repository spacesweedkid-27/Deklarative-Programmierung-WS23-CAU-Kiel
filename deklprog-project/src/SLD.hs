module SLD
  ( SLDTree (..),
    sld,
    -- Strategy,
    -- dfs,
    -- bfs,
    -- solveWith,
  )
where

import Base.Type
import Subst
import Unification
import Vars
import Rename

-- Data type for an SLD tree
data SLDTree = SLDTree Goal [(Subst, SLDTree)]
  deriving (Show)

sld :: Prog -> Goal -> SLDTree
sld (Prog rules) (Goal goals) = SLDTree (Goal goals) (build rules goals (allVars (Goal goals)))
  where
    build :: [Rule] -> [Term] -> [VarName] -> [(Subst, SLDTree)]
    build _ [] bl = []
    build [] _ bl = []
    build (rule:rs) mg@(g:gs) bl =
      let r@(Rule t ts) = rename bl rule in
        let bl1 = bl ++ allVars r in
          case unify t g of
            Nothing -> build rs mg bl -- ignore ununified braches
            Just s ->                 -- if unifiable
              let ags = map (apply s) (ts ++ gs) in
                (s, SLDTree (Goal ags) (build rules ags bl1)) -- 
                  : build rs mg bl1 -- append rest