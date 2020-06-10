-- | Variational relational algebra.
module Algebra where

import SAT
import Config

import Prelude hiding (Ordering(..))

import Data.Data (Data,Typeable)
import Data.String (IsString)
-- import qualified Data.ByteString.Char8 as BC (pack)
import Data.Maybe (fromMaybe)

import Data.Convertible.Base

import Data.SBV 

import Database.HDBC (SqlValue(..))

import Data.Time.LocalTime (ZonedTime,zonedTimeToUTC)

-- import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

--
-- Names and references
--

type Name = String 

-- | A relation (i.e. table) name.
newtype Relation = Relation { relationName :: Name }
  -- deriving (Eq, IsString, Ord, Show)
  deriving (Data,Eq,IsString,Ord,Read,Show,Typeable)


-- | An attribute (i.e. column) name.
newtype Attribute = Attribute { attributeName :: Name }
  -- deriving (Eq, IsString, Ord, Show)
  deriving (Data,Eq,IsString,Ord,Read,Show,Typeable)

data VAtt = Attr Attribute
          | AtChc FeatureExpr VAtt VAtt
  deriving (Eq, Show, Ord, Data, Typeable)

type AttList = [VAtt]

-- 
-- Feature expression and instances.
-- 

-- | The set of features referenced in a feature expression.
features :: FeatureExpr -> Set.Set Feature
features (FLit _)  = Set.empty
features (Ref f)   = Set.singleton f
features (Not e)   = features e
features (And l r) = features l `Set.union` features r
features (Or  l r) = features l `Set.union` features r

-- | Boolean expressions over features.
data FeatureExpr
  = FLit Bool
  | Ref Feature 
  | Not FeatureExpr
  | And FeatureExpr FeatureExpr
  | Or FeatureExpr FeatureExpr
 -- deriving (Eq, Show)
 deriving (Data, Typeable, Eq)

-- | Pretty print a feature expression.
prettyFeatureExpr :: FeatureExpr -> String
prettyFeatureExpr = top
  where
    -- top (And l r) = sub l ++ "∧" ++ sub r
    -- top (Or  l r) = sub l ++ "∨" ++ sub r
    top (And l r) = sub l ++ " AND " ++ sub r
    top (Or  l r) = sub l ++ " OR " ++ sub r
    top e         = sub e
    -- sub (Lit b)   = if b then "#T" else "#F"
    sub (FLit b)   = if b then "TRUE" else "FALSE"

    sub (Ref f)   = featureName f
    -- sub (Not e)   = "¬" ++ sub e
    sub (Not e)   = "NOT " ++ sub e
    sub e         = "(" ++ top e ++ ")"

-- | Evaluate a feature expression against a configuration.
-- evalFeatureExpr :: Boolean b => Config b -> FeatureExpr -> b
-- evalFeatureExpr _ (FLit b)  = if b then true else false
-- evalFeatureExpr c (Ref f)   = c f
-- evalFeatureExpr c (Not e)   = bnot (evalFeatureExpr c e)
-- evalFeatureExpr c (And l r) = evalFeatureExpr c l &&& evalFeatureExpr c r
-- evalFeatureExpr c (Or  l r) = evalFeatureExpr c l ||| evalFeatureExpr c r

-- | Generate a symbolic predicate for a feature expression.
-- symbolicFeatureExpr :: FeatureExpr -> Predicate
-- symbolicFeatureExpr e = 
-- symbolicFeatureExpr e = do
--     let fs = Set.toList (features e)
--     syms <- fmap (Map.fromList . zip fs) (sBools (map featureName fs))
--     let look f = fromMaybe err (Map.lookup f syms)
--     return (evalFeatureExpr look e)
--   where err = error "symbolicFeatureExpr: Internal error, no symbol found."

-- | Less than equal for feature expressions.
leFexp :: FeatureExpr -> FeatureExpr -> Bool
leFexp (FLit False) _            = True
leFexp (FLit True)  (FLit False) = False
leFexp (FLit _)     _            = True
leFexp _           (FLit _)      = False
leFexp (Ref _)     (Ref _)       = True
leFexp (Ref _)     _             = True
leFexp _           (Ref _)       = False
leFexp (Not f)     (Not f')      = leFexp f f'
leFexp (Not _)     _             = False
leFexp (And l r)   (And l' r')   = leFexp l l' && leFexp r r'
leFexp _           (And _ _)     = False
leFexp (Or l r)    (Or l' r')    = leFexp l l' && leFexp r r'
leFexp _ _ = False


-- instance Boolean FeatureExpr where
--   true  = FLit True
--   false = FLit False
--   bnot  = Not
--   (&&&) = And
--   (|||) = Or

-- instance SAT FeatureExpr where
--   toPredicate = symbolicFeatureExpr

instance Show FeatureExpr where
  show = prettyFeatureExpr

-- instance Eq FeatureExpr where
--   l == r = equivalent l r

instance Ord FeatureExpr where
 (<=) = leFexp

-- 
-- Conditions.
-- 

-- | Comparison operations.
data CompOp = EQ | NEQ | LT | LTE | GTE | GT
  -- deriving (Eq, Ord)
  deriving (Data,Eq,Typeable,Ord)

-- | pretty print compOp
prettyCompOp :: CompOp -> String
prettyCompOp EQ  = " == "
prettyCompOp NEQ = " <> "
prettyCompOp LT  = " < "
prettyCompOp LTE = " <= "
prettyCompOp GTE = " >= "
prettyCompOp GT  = " > "

instance Show CompOp where 
  show = prettyCompOp

-- | Semantics of a comparison operation.
compOp :: Ord a => CompOp -> a -> a -> Bool
compOp EQ  = (==)
compOp NEQ = (/=)
compOp LT  = (<)
compOp LTE = (<=)
compOp GTE = (>=)
compOp GT  = (>)


data Cond 
  = CLit Bool
  | Comp CompOp Atom Atom
  | CNot Cond
  | COr Cond Cond
  | CAnd Cond Cond
  | CChc FeatureExpr Cond Cond
 deriving (Data,Typeable,Eq,Ord)

-- | pretty prints pure relational conditions.
prettyCond :: Cond -> String
prettyCond (CChc f l r) = prettyFeatureExpr f ++ " < " 
  ++ prettyCond l ++ " , " ++ prettyCond r ++ " > "
prettyCond c = top c
  where
    top (Comp o l r) = show l ++ show o ++ show r
    top (CAnd l r) = sub l ++ " AND " ++ sub r
    top (COr l r) = sub l ++ " OR " ++ sub r
    top c' = sub c'
    sub (CLit b) = if b then " true " else " false "
    sub (CNot c') = " NOT " ++ sub c'
    sub c' = " ( " ++ top c' ++ " ) "

instance Show Cond where
  show = prettyCond


-- instance Boolean Cond where
--   true  = CLit True
--   false = CLit False
--   bnot  = CNot
--   (&&&) = CAnd
--   (|||) = COr

-- 
-- Atoms.
-- 

-- | Atoms are the leaves of a condition.
data Atom
  = Val  SqlValue
  | Att Attribute
 -- deriving (Eq, Ord)
  deriving (Data,Eq,Typeable,Ord)

instance Eq ZonedTime where
  a == b = (zonedTimeToUTC a) == (zonedTimeToUTC b)

instance Ord ZonedTime where 
  compare a b = compare (zonedTimeToUTC a) (zonedTimeToUTC b)

deriving instance Ord SqlValue

deriving instance Data SqlValue

-- | pretty print atoms.
prettyAtom :: Atom -> String
prettyAtom (Val v)  =  case safeConvert v of 
  Right val -> val
  _ -> error "safeConvert resulted in error!!! showAtom"
prettyAtom (Att a) = attributeName a


instance Show Atom where
  show = prettyAtom

-- 
-- Query Algebra
-- 

data Algebra
  = Proj AttList Algebra
  | Sel Cond Algebra
  | AChc FeatureExpr Algebra Algebra
  | Prod Algebra Algebra
  | TRef Relation 
  | Empty
 deriving (Data,Eq,Show,Typeable)