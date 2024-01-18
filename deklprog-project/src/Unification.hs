{-# LANGUAGE TemplateHaskell #-}

module Unification
  ( testUnification,
  ds,
  unify,
  ) where


import Data.Maybe
import Test.QuickCheck
import Base.Type
import Vars
import Subst

-- Does  a variable occur in a term?
occurs :: VarName -> Term -> Bool
occurs v t = v `elem` allVars t

ds :: Term -> Term -> Maybe (Term, Term)
ds t1 t2 = let res = Just (t1, t2) in if t1 == t2 then Nothing else case (t1, t2) of
  (Var _, _) -> res -- if one term is a Var then return both terms
  (_, Var _) -> res
  (Comb n1 t1l, Comb n2 t2l) -> if n1 /= n2 || length t1l /= length t2l then res else ds' t1l t2l where
    ds' (x:xs) (y:ys) = if x == y then ds' xs ys else ds x y -- find function parameter which is different and call ds recusively
    ds' _ _ = Nothing

unify :: Term -> Term -> Maybe Subst
unify a b = unify' a b empty where
  unify' t0 t1 s = case ds (apply s t0) (apply s t1)  of
    Nothing -> Just s
    Just (Var v, t) -> addSub v t -- if one is a var then return then call the function to run 
    Just (t, Var v) -> addSub v t
    Just (_, _) -> Nothing
    where
      addSub n t = if n `elem` allVars t then Nothing else unify' t0 t1 (compose (single n t) s) -- add substitution and calls unify recusively


-- Properties

-- Uncomment this to test the properties when all required functions are implemented

-- The disagreement set of a term with itself is empty
prop_1 :: Term -> Bool
prop_1 t = isNothing (ds t t)

-- The disagreement set of two different terms is not empty
prop_2 :: Term -> Term -> Property
prop_2 t1 t2 = isJust (ds t1 t2) ==> t1 /= t2

-- If a variable v occurs in a term t (other than the variable itself),
-- then the unification of v and t should fail due to the occur check
prop_3 :: VarName -> Term -> Property
prop_3 v t = occurs v t && t /= Var v ==> isNothing (unify (Var v) t)

-- If two terms t1 and t2 are unifiable, then the disagreement set of the mgu
-- applied to t1 and t2 is empty
prop_4 :: Term -> Term -> Property
prop_4 t1 t2 =
  let mMgu = unify t1 t2
  in isJust mMgu ==> let mgu = fromJust mMgu
                     in isNothing (ds (apply mgu t1) (apply mgu t2))

-- Run all tests
return []
testUnification :: IO Bool
testUnification = $quickCheckAll