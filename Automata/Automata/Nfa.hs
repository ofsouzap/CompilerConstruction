module Automata.Nfa
  ( Nfa(..)
  , NfaConfig(..)
  , nfaInitialConfig
  , configTransition
  , NfaState(..)
  , consume
  , NfaBuilderHelper(..)
  , NoEpsNfa(..) ) where

import Data.Set
  ( Set
  , empty
  , union )
import qualified Data.Set as Set
  ( map
  , singleton )
import Test.QuickCheck
  ( Arbitrary
  , CoArbitrary
  , arbitrary )
import Automata.Internal
  ( computeClosure )

-- Types

data NfaState s l = NfaState
  { stateLabel :: l
  , stateEpsTransitions :: Set (NfaState s l)
  , stateDirectTransition :: s -> Set (NfaState s l)
  , stateAccepting :: Bool }

stateEpsClosure :: Ord l => NfaState s l -> Set (NfaState s l)
stateEpsClosure = computeClosure stateEpsTransitions . Set.singleton -- TODO - this doesn't terminate when testing

stateFullTransition :: Ord l => NfaState s l -> s -> Set (NfaState s l)
stateFullTransition state = foldr (union . Set.singleton) empty . stateDirectTransition state
-- stateFullTransition state = foldr (union . stateEpsClosure) empty . stateDirectTransition state

newtype NfaConfig s l = NfaConfig (Set (NfaState s l))
  deriving Show

configTransition :: Ord l => NfaConfig s l -> s -> NfaConfig s l
configTransition (NfaConfig xs) s = NfaConfig ( foldr
  ( union . flip stateFullTransition s )
  empty
  xs )

newtype Nfa s l = Nfa (NfaState s l)

nfaInitialConfig :: Ord l => Nfa s l -> NfaConfig s l
nfaInitialConfig (Nfa state) = NfaConfig (stateEpsClosure state)

-- Basic instances

instance Show l => Show (NfaState s l) where
  show = show . stateLabel

instance Eq l => Eq (NfaState s l) where
  x1 == x2 = stateLabel x1 == stateLabel x2

instance Ord l => Ord (NfaState s l) where
  x1 `compare` x2 = stateLabel x1 `compare` stateLabel x2

instance Show l => Show (Nfa s l) where
  show (Nfa nfa) = "NFA[init=" ++ show nfa ++ "]"

-- NFA Builder

data NfaBuilderHelper s l = NfaBuilderHelper
  { nfaBuilderHelperInit :: l
  , nfaBuilderHelperEpsDelta :: l -> Set l
  , nfaBuilderHelperDelta :: s -> l -> Set l
  , nfaBuilderHelperAccept :: l -> Bool }

nfaBuilderEpsClosure :: Ord l => NfaBuilderHelper s l -> l -> Set l
nfaBuilderEpsClosure h = computeClosure (nfaBuilderHelperEpsDelta h) . Set.singleton

buildWithHelper :: Ord l => NfaBuilderHelper s l -> Nfa s l
buildWithHelper helper =
  (Nfa . helperState helper) (initLabel helper) where
    initLabel = nfaBuilderHelperInit
    helperState h l = NfaState
      { stateLabel = l
      , stateEpsTransitions = closedEpsDelta h l
      , stateDirectTransition = delta h l
      , stateAccepting = nfaBuilderHelperAccept h l }
    closedEpsDelta h l = Set.map (helperState h) (nfaBuilderEpsClosure h l)
    delta h l s = Set.map (helperState h) (nfaBuilderHelperDelta h s l)

-- Arbitrary instances

newtype ArbNoEpsHelper s l = ArbNoEpsHelper (NfaBuilderHelper s l)

unwrapArbNoEpsHelper :: ArbNoEpsHelper s l -> NfaBuilderHelper s l
unwrapArbNoEpsHelper (ArbNoEpsHelper x) = x

instance (Ord l, CoArbitrary s, Arbitrary l, CoArbitrary l) => Arbitrary (ArbNoEpsHelper s l) where
  arbitrary = do
    ini <- arbitrary
    let epsDelta = const empty
    delta <- arbitrary
    accept <- arbitrary
    (return . ArbNoEpsHelper) (NfaBuilderHelper
      { nfaBuilderHelperInit = ini
      , nfaBuilderHelperEpsDelta = epsDelta
      , nfaBuilderHelperDelta = delta
      , nfaBuilderHelperAccept = accept } )

-- | Wrapper for NFA that doesn't use epsilon transitions
newtype NoEpsNfa s l = NoEpsNfa (Nfa s l)
  deriving ( Show )

instance (Ord l, CoArbitrary s, Arbitrary l, CoArbitrary l) => Arbitrary (NoEpsNfa s l) where
  arbitrary = NoEpsNfa . buildWithHelper . unwrapArbNoEpsHelper <$> arbitrary

-- Functions

consume :: (Ord l, Foldable t) => NfaConfig s l -> t s -> NfaConfig s l
consume = foldr (flip configTransition)
