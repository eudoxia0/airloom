module Main (main) where
import Test.HUnit

trivialTest :: Test
trivialTest = TestCase (assertEqual "1 + 1 = 2" (1 + 1) (2 :: Int))

tests :: Test
tests = TestList [
    TestLabel "trivial" trivialTest
  ]


main :: IO ()
main = runTestTTAndExit tests