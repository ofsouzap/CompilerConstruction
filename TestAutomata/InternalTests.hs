{-# LANGUAGE ScopedTypeVariables #-}
module InternalTests where

import Data.Set
  ( Set
  , fromList
  , empty
  , singleton
  , insert )
import Test.Hspec
  ( Spec
  , hspec
  , describe
  , it
  , shouldBe )
import Test.QuickCheck
  ( property
  , Arbitrary
  , arbitrary
  , elements )
import Automata.Internal

data LimitedThing =
    One
  | Two
  | Three
  | Four
  deriving (Eq, Show, Ord)

instance Arbitrary LimitedThing where
  arbitrary = elements [One, Two, Three, Four]

ltTotalClosureF :: LimitedThing -> Set LimitedThing
ltTotalClosureF One = singleton Two
ltTotalClosureF Two = singleton Three
ltTotalClosureF Three = singleton Four
ltTotalClosureF Four = singleton One

ltTotalClosureSet :: Set LimitedThing
ltTotalClosureSet = fromList [One, Two, Three, Four]

spec =
  describe "Internal" $ do
    describe "compute closure" $ do
      it "should have singleton closure for no-relations starting with singleton" $ property $
        \ (x :: Int) -> computeClosure (const empty) (singleton x) == singleton x
      it "should have idenity closure for no-relations" $ property $
        \ (xs :: Set Int) -> computeClosure (const empty) xs == xs
      it "should give total closure for the \"LimitedThing\" closed set" $ property $
        \ (xs' :: Set LimitedThing, x :: LimitedThing) -> computeClosure ltTotalClosureF (insert x xs') == ltTotalClosureSet
