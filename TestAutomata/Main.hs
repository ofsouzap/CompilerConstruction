module Main where

import Test.Hspec ( hspec )
import qualified DfaTests ( spec )
import qualified NfaTests ( spec )

main :: IO ()
main = hspec $ do
  DfaTests.spec
  NfaTests.spec
