-- | Variational relational algebra.
module MinVar where

import SAT
import Config
import Algebra

isValid :: Algebra -> Bool
isValid = undefined

hasDeadBranch :: Algebra -> Bool
hasDeadBranch = undefined

hasRedundantBranch :: Algebra -> Bool
hasRedundantBranch = undefined