module Automata.Nfa
  ( Nfa(..)
  , NfaConfig(..)
  , consume ) where

import Data.Set
  ( Set(..) )
import Test.QuickCheck
  ( Arbitrary
  , CoArbitrary
  , Gen
  , arbitrary
  , coarbitrary )

-- Types

newtype Bounded s => NfaConfig s = NfaConfig (Set s)
  deriving (Eq, Show)

data Bounded s => Nfa s c = Nfa {
    nfaInitial :: s
  , nfaTransition :: c -> NfaConfig s -> NfaConfig s
  , nfaStateAccept :: s -> Bool }

-- Basic instances

instance (Bounded s, Show s) => Show (Nfa s c) where
  show nfa = "NFA[init=" ++ show (nfaInitial nfa) ++ "]"

-- Arbitrary instances

instance (Bounded s, CoArbitrary s) => CoArbitrary (NfaConfig s) where
  coarbitrary (NfaConfig s) = coarbitrary s

instance (Ord s, Bounded s, Arbitrary s) => Arbitrary (NfaConfig s) where
  arbitrary = NfaConfig <$> arbitrary

instance (Ord s, Bounded s, Arbitrary s, Arbitrary c, CoArbitrary s, CoArbitrary c) => Arbitrary (Nfa s c) where
  arbitrary = do
    init <- arbitrary
    transition <- arbitrary
    accept <- arbitrary
    return $ Nfa
      { nfaInitial = init
      , nfaTransition = transition
      , nfaStateAccept = accept
      }

-- Functions

consume :: (Bounded s, Foldable t) => Nfa s c -> NfaConfig s -> t c -> NfaConfig s
consume = foldr . nfaTransition
