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

spec =
  describe "DFA" $ do
    it "should consume a single symbol sequence correctly" $ property $
      \ (dfa :: Dfa Int Char, init :: DfaConfig Int, c :: Char) ->
        dfaTransition dfa c init == consume dfa init [c]
