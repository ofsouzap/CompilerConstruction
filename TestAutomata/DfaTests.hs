{-# LANGUAGE ScopedTypeVariables #-}
module DfaTests where

import Test.Hspec
  ( Spec
  , hspec
  , describe
  , it
  , shouldBe )
import Test.QuickCheck
  ( property )
import Automata.Dfa

compareConfigLabel :: Eq l => DfaConfig s l -> DfaConfig s l -> Bool
compareConfigLabel (DfaConfig s1) (DfaConfig s2) = stateLabel s1 == stateLabel s2

spec =
  describe "DFA" $ do
    it "should consume a single symbol sequence correctly" $ property $
      \ ((Dfa init) :: Dfa Char String, c :: Char) ->
        let initConfig = DfaConfig init in
          configTransition initConfig c `compareConfigLabel` consume initConfig [c]
