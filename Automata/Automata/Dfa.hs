module Automata.Dfa
  ( Dfa(..)
  , DfaConfig(..)
  , consume ) where

import Test.QuickCheck
  ( Arbitrary
  , CoArbitrary
  , Gen
  , arbitrary
  , coarbitrary )

-- Types

newtype Bounded s => DfaConfig s = DfaConfig s
  deriving (Eq, Show)

data Bounded s => Dfa s c = Dfa {
    dfaInitial :: DfaConfig s
  , dfaTransition :: c -> DfaConfig s -> DfaConfig s
  , dfaStateAccept :: s -> Bool }

-- Basic instances

instance (Bounded s, Show s) => Show (Dfa s c) where
  show dfa = "DFA[init=" ++ show (dfaInitial dfa) ++ "]"

-- Arbitrary instances

instance (Bounded s, CoArbitrary s) => CoArbitrary (DfaConfig s) where
  coarbitrary (DfaConfig s) = coarbitrary s

instance (Bounded s, Arbitrary s) => Arbitrary (DfaConfig s) where
  arbitrary = DfaConfig <$> arbitrary

instance (Bounded s, Arbitrary s, Arbitrary c, CoArbitrary s, CoArbitrary c) => Arbitrary (Dfa s c) where
  arbitrary = do
    init <- arbitrary
    transition <- arbitrary
    accept <- arbitrary
    return $ Dfa
      { dfaInitial = init
      , dfaTransition = transition
      , dfaStateAccept = accept
      }

-- Functions

consume :: (Bounded s, Foldable t) => Dfa s c -> DfaConfig s -> t c -> DfaConfig s
consume = foldr . dfaTransition
