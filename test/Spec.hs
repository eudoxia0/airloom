module Main (main) where
import Test.HUnit
import Text.Parsec (parse)

trivialTest :: Test
trivialTest = TestCase (assertEqual "1 + 1 = 2" (1 + 1) (2 :: Int))

tests :: Test
tests = TestList [
    TestLabel "trivial" trivialTest
  ]


main :: IO ()
main = runTestTTAndExit tests