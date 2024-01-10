{-# LANGUAGE TemplateHaskell #-}

module Subst
  ( Subst, -- don't export the constructor of the data type!
    domain,
    empty,
    single,
    isEmpty,  -- sussy of them to not add this
    compose,
    apply,
    -- restrictTo,
    testSubst,
  )
where

-- Hand in after this part... maybe... I don't know...
-- at least everything before this should not be handed in...

import Base.Type
import Data.List (intercalate, nub, sort)
import Test.QuickCheck
import Vars

-- Data type for substitutions
data Subst = Subst [(VarName, Term)]
  deriving (Show)


domain :: Subst -> [VarName]
domain (Subst ((name1, Var name2) : rest))        -- case that we have a variable and some rest
  | name1 /= name2 = name1 : domain (Subst rest)  -- name is not the same?, then add it and append the rest
  | otherwise      = domain (Subst rest)          -- else just append the rest
domain (Subst ((name1, Comb _ terms) : rest)) = name1 : domain (Subst rest) -- even if terms is just [name1] then still we accept name1!
domain (Subst []) = []  -- case for the last rest

empty :: Subst
empty = Subst []  -- easy

single :: VarName -> Term -> Subst
single name term = Subst [(name, term)]   -- still easy

isEmpty :: Subst -> Bool
isEmpty subst = null (domain subst) -- no curry :(, and also easy

apply :: Subst -> Term -> Term
apply (Subst ((name1, term) : rest)) (Var name2)
  | name1 == name2 = term                           -- found it? then apply
  | otherwise      = apply (Subst rest) (Var name2) -- if not, got play fetch
apply subst (Comb combname terms) = Comb combname (map (apply subst) terms)  -- huh? it can be so simple...
apply (Subst []) name = name -- if the varname has not been found, then just do nothing

compose :: Subst -> Subst -> Subst
compose (Subst list1) (Subst list2) =
  Subst
  ([(name, apply (Subst list1) term) | (name, term) <- list2, apply (Subst list1) term /= Var name]  -- left part of the definition
  ++ 
  [pair | pair <- list1, fst pair `notElem` domain (Subst list2)]) -- right part of the definition
  -- I don't think we have to filter duplicates...

-- Generator for substitutions
instance Arbitrary Subst where
  -- We use the `suchThat` combinator to filter out substitutions that are not valid,
  -- i.e. whose domain contains the same variable more than once.
  arbitrary = Subst <$> (arbitrary `suchThat` ((\vts -> length vts == length (nub vts)) . map fst))


-- Properties

{- Uncomment this to test the properties when all required functions are implemented

-- Applying the empty substitution to a term should not change the term
prop_1 :: Term -> Bool
prop_1 t = apply empty t == t

-- Applying a singleton substitution {X -> t} to X should return t
prop_2 :: VarName -> Term -> Bool
prop_2 x t = apply (single x t) (Var x) == t

-- Applying a composed substitution is equal to applying the two substitutions individually
prop_3 :: Term -> Subst -> Subst -> Bool
prop_3 t s1 s2 = apply (compose s1 s2) t == apply s1 (apply s2 t)

-- The domain of the empty substitution is empty
prop_4 :: Bool
prop_4 = null (domain empty)

-- The domain of a singleton substitution {X -> X} is empty
prop_5 :: VarName -> Bool
prop_5 x = null (domain (single x (Var x)))

-- The domain of a singleton substitution {X -> t} is [X]
prop_6 :: VarName -> Term -> Property
prop_6 x t = t /= Var x ==> domain (single x t) == [x]

-- The domain of a composed substitution is the union of the domains of the two substitutions
prop_7 :: Subst -> Subst -> Bool
prop_7 s1 s2 = all (`elem` (domain s1 ++ domain s2)) (domain (compose s1 s2))

-- The domain of a composed substitution does not contain variables that are mapped to themselves
prop_8 :: VarName -> VarName -> Property
prop_8 x1 x2 =
  x1
    /= x2
    ==> domain (compose (single x2 (Var x1)) (single x1 (Var x2)))
    == [x2]

-- The empty substitution does not contain any variables
prop_9 :: Bool
prop_9 = null (allVars empty)

-- The singleton substitution should not map a variable to itself
prop_10 :: VarName -> Bool
prop_10 x = null (allVars (single x (Var x)))

-- The variables occuring in a subsitution should be taken from both components of the individual substitutions
prop_11 :: VarName -> Term -> Property
prop_11 x t =
  t
    /= Var x
    ==> sort (nub (allVars (single x t)))
    == sort (nub (x : allVars t))

-- The variables occuring in a composed substitution are a subset of the variables occuring in the two substitutions
prop_12 :: Subst -> Subst -> Bool
prop_12 s1 s2 =
  all (`elem` (allVars s1 ++ allVars s2)) (allVars (compose s1 s2))

-- The composed subsitution should contain the left substitution unless its variables are mapped by the right substitution
prop_13 :: VarName -> VarName -> Property
prop_13 x1 x2 =
  x1
    /= x2
    ==> sort (allVars (compose (single x2 (Var x1)) (single x1 (Var x2))))
    == sort [x1, x2]

-- The domain of a substitution is a subset of all its variables
prop_14 :: Subst -> Bool
prop_14 s = all (`elem` allVars s) (domain s)

-- Restricting the empty substitution to an arbitrary set of variables should return the empty substitution
prop_15 :: [VarName] -> Bool
prop_15 xs = null (domain (restrictTo empty xs))

-- The domain of a restricted substitution is a subset of the given set of variables
prop_16 :: [VarName] -> Subst -> Bool
prop_16 xs s = all (`elem` xs) (domain (restrictTo s xs))

-}

-- Run all tests
testSubst :: IO Bool
testSubst = undefined