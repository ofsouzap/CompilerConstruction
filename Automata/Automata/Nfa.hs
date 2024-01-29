module Automata.Nfa
  ( Nfa(..)
  , NfaConfig(..)
  , configTransition
  , NfaState(..)
  , consume ) where

import Data.Set
  ( Set(..)
  , empty
  , union
  , map )
import Test.QuickCheck
  ( Arbitrary
  , CoArbitrary
  , arbitrary
  , coarbitrary )

-- Types

data NfaState s l = NfaState
  { stateLabel :: l
  , stateTransition :: s -> Set (NfaState s l)
  , stateAccepting :: Bool }

newtype NfaConfig s l = NfaConfig (Set (NfaState s l))
  deriving Show

configTransition :: Ord l => NfaConfig s l -> s -> NfaConfig s l
configTransition (NfaConfig xs) s = NfaConfig ( foldr
  ( union . flip stateTransition s )
  empty
  xs )

newtype Nfa s l = Nfa (Set (NfaState s l))

-- Basic instances

instance Show l => Show (NfaState s l) where
  show = show . stateLabel

instance Eq l => Eq (NfaState s l) where
  x1 == x2 = stateLabel x1 == stateLabel x2

instance Ord l => Ord (NfaState s l) where
  x1 `compare` x2 = stateLabel x1 `compare` stateLabel x2

instance Show l => Show (Nfa s l) where
  show (Nfa nfa) = "NFA[init=" ++ show nfa ++ "]"

-- Arbitrary instances

data ArbitraryNfaHelper s l = ArbitraryNfaHelper
  { arbNfaHelperInit :: Set l
  , arbNfaHelperDelta :: s -> l -> Set l
  , arbNfaHelperAccept :: l -> Bool }

instance (Ord l, CoArbitrary s, Arbitrary l, CoArbitrary l) => Arbitrary (ArbitraryNfaHelper s l) where
  arbitrary = do
    init <- arbitrary
    delta <- arbitrary
    accept <- arbitrary
    return (ArbitraryNfaHelper
      { arbNfaHelperInit = init
      , arbNfaHelperDelta = delta
      , arbNfaHelperAccept = accept } )

instance (Ord l, CoArbitrary s, Arbitrary l, CoArbitrary l) => Arbitrary (Nfa s l) where
  arbitrary = do
    helper <- arbitrary
    let initLabels = arbNfaHelperInit helper
    let helperState d l = NfaState {
        stateLabel = l
      , stateTransition = d l
      , stateAccepting = arbNfaHelperAccept helper l }
    let helperDelta x s = arbNfaHelperDelta helper s x
    let delta x s = Data.Set.map (helperState delta) (helperDelta x s)
    return (Nfa ( Data.Set.map (helperState delta) initLabels ))

-- TODO - could make functor instance for DFA for manipulating values of labels

-- Functions

consume :: (Ord l, Foldable t) => NfaConfig s l -> t s -> NfaConfig s l
consume = foldr (flip configTransition)
