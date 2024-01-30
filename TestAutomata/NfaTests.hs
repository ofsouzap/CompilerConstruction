{-# LANGUAGE ScopedTypeVariables #-}
module NfaTests
  ( spec ) where

import Data.Set
  ( singleton
  , union
  , empty )
import Test.Hspec
  ( Spec
  , describe
  , it )
import Test.QuickCheck
  ( property )
import Automata.Nfa
import Automata.Internal

compareConfigLabel :: Eq l => NfaConfig s l -> NfaConfig s l -> Bool
compareConfigLabel (NfaConfig s1) (NfaConfig s2) = s1 == s2

nfaEx0 :: Natural -> Nfa Natural Natural
nfaEx0 ini = buildWithHelper $ NfaBuilderHelper
  { nfaBuilderHelperInit = ini
  , nfaBuilderHelperEpsDelta = singleton . naturalPredOrZero
  , nfaBuilderHelperDelta = \ x -> singleton . (+) x
  , nfaBuilderHelperAccept = undefined }

nfaEx0CheckInitialStateClosure :: Natural -> Bool
nfaEx0CheckInitialStateClosure ini = resExp == res where
  resExp = naturalToInteger ini + 1
  res = (toInteger . length . stateEpsClosure . initState) (nfaEx0 ini)
  initState (Nfa x) = x

nfaEx0StepConfigClosureCheck :: Natural -> Natural -> Bool
nfaEx0StepConfigClosureCheck ini sym = resExp == res where
  resExp = naturalToInteger $ ini + sym + 1
  res = (toInteger . length . foldr (union . stateEpsClosure) empty . configStates) stepped
  stepped = consume (nfaInitialConfig nfa) [sym]
  configStates (NfaConfig xs) = xs
  nfa = nfaEx0 ini

spec :: Spec
spec =
  describe "NFA" $ do
    describe "no epsilon transitions" $ do
      it "should consume a single symbol sequence correctly" $ property $
        \ ((NoEpsNfa (Nfa ini)) :: NoEpsNfa Char String, c :: Char) ->
          let initConfig = NfaConfig (singleton ini) in
            configTransition initConfig c `compareConfigLabel` consume initConfig [c]
    describe "with epsilon transitions" $ do
      it "should find the correct initial state epsilon closure for example NFA 0" $ property
        nfaEx0CheckInitialStateClosure
      it "should find the correct configuration epsilon closure for example NFA 0" $ property $
        \ (x :: Natural) (y :: Natural) -> nfaEx0StepConfigClosureCheck x y
