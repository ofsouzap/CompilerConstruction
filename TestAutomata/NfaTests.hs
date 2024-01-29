{-# LANGUAGE ScopedTypeVariables #-}
module NfaTests where

import Data.Set
  ( singleton )
import Test.Hspec
  ( Spec
  , hspec
  , describe
  , it
  , shouldBe )
import Test.QuickCheck
  ( property )
import Automata.Nfa

compareConfigLabel :: Eq l => NfaConfig s l -> NfaConfig s l -> Bool
compareConfigLabel (NfaConfig s1) (NfaConfig s2) = s1 == s2

spec =
  describe "NFA" $ do
    it "should consume a single symbol sequence correctly" $ property $
      \ ((Nfa init) :: Nfa Char String, c :: Char) ->
        let initConfig = NfaConfig (singleton init) in
          configTransition initConfig c `compareConfigLabel` consume initConfig [c]
