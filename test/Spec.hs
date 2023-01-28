module Main (main) where
import Test.HUnit
import AirLoom.Parser (parseSourceLine, SourceLine (SourceTextLine))

-- Parser tests.

parseSourceLineTest :: Test
parseSourceLineTest = TestCase (do
  assertEqual "" (parseSourceLine "") (SourceTextLine "")
  assertEqual "" (parseSourceLine "abc") (SourceTextLine "abc")
  assertEqual "" (parseSourceLine "Hello, world!") (SourceTextLine "Hello, world!"))

trivialTest :: Test
trivialTest = TestCase (assertEqual "1 + 1 = 2" (1 + 1) (2 :: Int))

tests :: Test
tests = TestList [
    TestLabel "trivial" trivialTest,
    TestLabel "parseSourceLine" parseSourceLineTest
  ]


main :: IO ()
main = runTestTTAndExit tests