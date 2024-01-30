{-# LANGUAGE ScopedTypeVariables #-}
module DfaTests
  ( spec ) where

import Test.Hspec
  ( Spec
  , describe
  , it )
import Test.QuickCheck
  ( property )
import Automata.Dfa

compareConfigLabel :: Eq l => DfaConfig s l -> DfaConfig s l -> Bool
compareConfigLabel (DfaConfig s1) (DfaConfig s2) = stateLabel s1 == stateLabel s2

spec :: Spec
spec =
  describe "DFA" $ do
    it "should consume a single symbol sequence correctly" $ property $
      \ ((Dfa ini) :: Dfa Char String, c :: Char) ->
        let initConfig = DfaConfig ini in
          configTransition initConfig c `compareConfigLabel` consume initConfig [c]
