{-# LANGUAGE ScopedTypeVariables #-}
module NfaTests
  ( spec ) where

import Data.Set
  ( singleton )
import Test.Hspec
  ( Spec
  , Expectation
  , describe
  , it
  , shouldBe )
import Test.QuickCheck
  ( property )
import Automata.Nfa

compareConfigLabel :: Eq l => NfaConfig s l -> NfaConfig s l -> Bool
compareConfigLabel (NfaConfig s1) (NfaConfig s2) = s1 == s2

epsTest0 :: Expectation
epsTest0 = resExp `shouldBe` res where -- TODO
  resExp = ""
  res = ""

spec :: Spec
spec =
  describe "NFA" $ do
    describe "no epsilon transitions" $ do
      it "should consume a single symbol sequence correctly" $ property $
        \ ((NoEpsNfa (Nfa ini)) :: NoEpsNfa Char String, c :: Char) ->
          let initConfig = NfaConfig (singleton ini) in
            configTransition initConfig c `compareConfigLabel` consume initConfig [c]
    describe "with epsilon transitions" $ do
      it "should pass test case 0" epsTest0
