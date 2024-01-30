module Automata.Internal
  ( computeClosure
  , Natural(..)
  , naturalSucc
  , naturalPredOrZero
  , naturalFromInteger
  , naturalToInteger ) where

import Data.Set
  ( Set
  , member )
import qualified Data.Set as Set
  ( empty
  , toList
  , filter
  , insert )
import qualified GHC.Natural as GHCNat
  ( Natural
  , naturalFromInteger
  , naturalToInteger )
import Test.QuickCheck
  ( Arbitrary
  , Gen
  , arbitrary
  , suchThat )

-- | Compute the set of values reachable from a specified starting value using some non-deterministic transition function.
-- WARNING: If the closure is infinite then this won't terminate
computeClosure :: Ord a => (a -> Set a) -> Set a -> Set a
computeClosure f xs = aux (Set.toList xs, Set.empty) where
  aux ([], acc) = acc
  aux (h:ts, acc) = if h `member` acc
    then aux (ts, acc)
    else aux (Set.toList newNexts ++ ts, Set.insert h acc) where
      nexts = f h
      newNexts = Set.filter (not . (`member` acc)) nexts

newtype Natural = Natural GHCNat.Natural
  deriving (Show, Eq, Ord)

naturalFromInteger :: Integer -> Natural
naturalFromInteger = Natural . GHCNat.naturalFromInteger

naturalToInteger :: Natural -> Integer
naturalToInteger (Natural n) = GHCNat.naturalToInteger n

naturalSucc :: Natural -> Natural
naturalSucc (Natural n) = (Natural . succ) n

naturalPredOrZero :: Natural -> Natural
naturalPredOrZero (Natural 0) = Natural 0
naturalPredOrZero (Natural n) = Natural (n-1)

instance Num Natural where
  (Natural a) + (Natural b) = Natural (a+b)
  (Natural a) - (Natural b) = Natural (a-b)
  (Natural a) * (Natural b) = Natural (a*b)
  abs (Natural x) = Natural (abs x)
  signum (Natural x) = Natural (signum x)
  fromInteger x = Natural (fromInteger x)

instance Arbitrary Natural where
  arbitrary = naturalFromInteger <$> suchThat (arbitrary :: Gen Integer) (> 0)
