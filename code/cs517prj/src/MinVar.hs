-- | Variational relational algebra.
module MinVar where

import SAT
import Config
import Algebra

import Data.SBV

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-- returns the size of a query which is the dept of the query's AST.
querySize :: Algebra -> Int -> Int
querySize (Proj as q) n = max (querySize q (n+1)) (maximum $ map (flip attSize 0) as)
  where
    attSize :: VAtt -> Int -> Int
    attSize (Attr _) m = m+1
    attSize (AtChc _ l r) m = max (attSize l (m+1)) (attSize r (m+1))
querySize (Sel c q) n = max (querySize q (n+1)) (condSize c 0)
  where
    condSize :: Cond -> Int -> Int
    condSize (CLit _) m = m+1
    condSize (Comp _ _ _) m = m+1
    condSize (CNot c) m = condSize c (m+1)
    condSize (COr l r) m = max (condSize l (m+1)) (condSize r (m+1))
    condSize (CAnd l r) m = max (condSize l (m+1)) (condSize r (m+1))
    condSize (CChc _ l r) m = max (condSize l (m+1)) (condSize r (m+1))
querySize (AChc _ l r) n = max (querySize l (n+1)) (querySize r (n+1))
querySize (Prod l r) n = max (querySize l (n+1)) (querySize r (n+1))
querySize (TRef _) n = n+1
querySize Empty n = n

-- 
fexp2sat :: FeatureExpr -> Symbolic SBool
fexp2sat (FLit True) = return sTrue
fexp2sat (FLit False) = return sFalse
fexp2sat (Ref f) = sBool (featureName f)
fexp2sat (Not f) = fexp2sat f >>= return . sNot
fexp2sat (And l r) = 
  do sl <- fexp2sat l
     sr <- fexp2sat r
     return $ sl .&& sr
fexp2sat (Or l r) = 
  do sl <- fexp2sat l
     sr <- fexp2sat r
     return $ sl .|| sr

type AttEnv = M.Map Attribute SBool

-- 
updateAttEnv :: VAtt -> AttEnv -> Symbolic AttEnv
updateAttEnv (Attr a) env = 
  do sa <- sBool (attributeName a)
     return $ M.insert a (sTrue .&& sa) env
updateAttEnv (AtChc _ l r) env = 
  do envl <- updateAttEnv l env
     updateAttEnv r envl

-- 
att2sat :: VAtt -> AttEnv -> Symbolic SBool
att2sat (Attr a) env = return (env M.! a) 
att2sat (AtChc f l r) env = 
  do sf <- fexp2sat f 
     sl <- att2sat l env
     sr <- att2sat r env
     return $ (sf .&& sl) .|| ((sNot sf) .&& sr)

-- generates the sat formula for attributes.
atts2sat :: AttList -> Symbolic SBool
atts2sat as =
  do envs <- mapM (flip updateAttEnv M.empty) as
     let env = foldr M.union M.empty envs
     sas <- mapM (flip att2sat env) as
     return $ foldr (.&&) sTrue sas


-- generates the sat forumla for conditions.
conds2sat :: Cond -> SBool
conds2sat = undefined

-- generates the sat formula for query.
q2sat :: Algebra -> SBool
q2sat = undefined

-- generates a list propositional formual of the query.
q2allsat :: Algebra -> [SBool]
q2allsat = undefined

-- Given two queries determines if they are equivalent or not.
equivQs :: Algebra -> Algebra -> Bool
equivQs = undefined


-- given two queries are they equivalent and is the second one
-- smaller than the first one.
minimalQuery :: Algebra -> Algebra -> Bool
minimalQuery = undefined


-- isValid :: Algebra -> Bool
-- isValid = undefined

-- hasDeadBranch :: Algebra -> Bool
-- hasDeadBranch = undefined

-- hasRedundantBranch :: Algebra -> Bool
-- hasRedundantBranch = undefined