module Pretty (
  Pretty(pretty)
  )
where

import Data.List (intercalate)

import Base.Type

-- Type class for pretty printing
class Pretty a where
  pretty :: a -> String

-- Pretty printing of terms
instance Pretty Term where
  pretty (Var (VarName x)) = x
  pretty (Comb f []) = f
  pretty (Comb "." [t1, t2]) = '[' : prettyList t1 t2 ++ "]"
    where prettyList t1' t2' = pretty t1' ++ case t2' of
            Comb "[]" []           -> ""
            Comb "."  [t1'', t2''] -> ", " ++ prettyList t1'' t2''
            _                      -> "|" ++ pretty t2'
  pretty (Comb f ts) = f ++ '(' : intercalate ", " (map pretty ts) ++ ")"

-- Pretty printing of rules
instance Pretty Rule where
  pretty (Rule t []) = pretty t ++ "."
  pretty (Rule t ts) = pretty t ++ " :- " ++ intercalate ", " (map pretty ts) ++ "."

-- Pretty printing of programs
instance Pretty Prog where
  pretty (Prog rs) = intercalate "\n" (map pretty rs)

-- Pretty printing of goals
instance Pretty Goal where
  pretty (Goal ts) = "?- " ++ intercalate ", " (map pretty ts) ++ "."
