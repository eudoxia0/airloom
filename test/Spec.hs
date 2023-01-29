module Main (main) where

import Test.HUnit
import qualified TestParser

tests :: Test
tests =
  TestList
    [ TestLabel "Parser Tests" TestParser.suite
    ]

main :: IO ()
main = runTestTTAndExit tests
