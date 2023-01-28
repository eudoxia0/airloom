module Main (main) where
import Test.HUnit
import Text.Parsec (parse)
import AirLoom.Parser (nameChar)

nameCharTest :: Test
nameCharTest = TestCase (do
  assertEqual "nameChar" (parse nameChar "" "a") (Right 'a')
  assertEqual "nameChar" (parse nameChar "" " ") (Right ' ')
  assertEqual "nameChar" (parse nameChar "" "1") (Right '1'))

trivialTest :: Test
trivialTest = TestCase (assertEqual "1 + 1 = 2" (1 + 1) (2 :: Int))

tests :: Test
tests = TestList [
    TestLabel "trivial" trivialTest,
    TestLabel "nameChar" nameCharTest
  ]


main :: IO ()
main = runTestTTAndExit tests