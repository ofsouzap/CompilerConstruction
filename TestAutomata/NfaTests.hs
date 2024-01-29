{-# LANGUAGE ScopedTypeVariables #-}
module NfaTests where

import Test.Hspec
  ( Spec
  , hspec
  , describe
  , it
  , shouldBe )
import Test.QuickCheck
  ( property )
import Automata.Nfa

spec =
  describe "NFA" $ do
    it "should consume a single symbol sequence correctly" $ property $
      \ (nfa :: Nfa Int Char, init :: NfaConfig Int, c :: Char) ->
        nfaTransition nfa c init == consume nfa init [c]
