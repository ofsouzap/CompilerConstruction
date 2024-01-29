module Main where

import Test.Hspec ( hspec )
import qualified DfaTests ( spec )

main :: IO ()
main = hspec $ do
  DfaTests.spec
