module Automata.Dfa
  ( Dfa(..)
  , DfaConfig(..)
  , configTransition
  , DfaState(..)
  , consume ) where

import Test.QuickCheck
  ( Arbitrary
  , CoArbitrary
  , Gen
  , arbitrary
  , coarbitrary )

-- Types

data DfaState s l = DfaState
  { stateLabel :: l
  , stateTransition :: s -> DfaState s l
  , stateAccepting :: Bool }

newtype DfaConfig s l = DfaConfig (DfaState s l)
  deriving Show

configTransition :: DfaConfig s l -> s -> DfaConfig s l
configTransition (DfaConfig x) = DfaConfig . stateTransition x

newtype Dfa s l = Dfa (DfaState s l)

-- Basic instances

instance Show l => Show (DfaState s l) where
  show = show . stateLabel

instance Show l => Show (Dfa s l) where
  show (Dfa dfa) = "DFA[init=" ++ show dfa ++ "]"

-- Arbitrary instances

data ArbitraryDfaHelper s l = ArbitraryDfaHelper
  { arbDfaHelperInit :: l
  , arbDfaHelperDelta :: s -> l -> l
  , arbDfaHelperAccept :: l -> Bool }

instance (CoArbitrary s, Arbitrary l, CoArbitrary l) => Arbitrary (ArbitraryDfaHelper s l) where
  arbitrary = do
    init <- arbitrary
    delta <- arbitrary
    accept <- arbitrary
    return (ArbitraryDfaHelper
      { arbDfaHelperInit = init
      , arbDfaHelperDelta = delta
      , arbDfaHelperAccept = accept } )

instance (CoArbitrary s, Arbitrary l, CoArbitrary l) => Arbitrary (Dfa s l) where
  arbitrary = do
    helper <- arbitrary
    let initLabel = arbDfaHelperInit helper
    let helperState d l = DfaState {
        stateLabel = l
      , stateTransition = d l
      , stateAccepting = arbDfaHelperAccept helper l }
    let helperDelta x0 s = arbDfaHelperDelta helper s x0
    let delta s l = helperState delta (helperDelta s l)
    return (Dfa ( helperState delta initLabel ))

-- Functions

consume :: (Foldable t) => DfaConfig s l -> t s -> DfaConfig s l
consume = foldr (flip configTransition)
