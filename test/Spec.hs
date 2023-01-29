module Main (main) where

import Test.HUnit
import qualified TestLift
import qualified TestParser
import qualified TestStore

tests :: Test
tests =
  TestList
    [ TestLabel "Parser Tests" TestParser.suite,
      TestLabel "Store Tests" TestStore.suite,
      TestLabel "Lifting Tests" TestLift.suite
    ]

main :: IO ()
main = runTestTTAndExit tests
