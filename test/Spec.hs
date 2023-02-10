module Main (main) where

import Test.HUnit
import qualified TestLift
import qualified TestParser
import qualified TestStore
import qualified TestWeave

tests :: Test
tests =
  TestList
    [ TestLabel "Parser Tests" TestParser.suite,
      TestLabel "Store Tests" TestStore.suite,
      TestLabel "Lifting Tests" TestLift.suite,
      TestLabel "Weaving Tests" TestWeave.suite
    ]

main :: IO ()
main = runTestTTAndExit tests
