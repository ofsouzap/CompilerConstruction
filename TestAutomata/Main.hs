module Main where

import Test.Hspec ( hspec )
import qualified InternalTests ( spec )
import qualified DfaTests ( spec )
import qualified NfaTests ( spec )

main :: IO ()
main = hspec $ do
  InternalTests.spec
  DfaTests.spec
  NfaTests.spec
