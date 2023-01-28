module Main (main) where
import Test.HUnit
import AirLoom.Parser (parseSourceLine, SourceLine (SourceTextLine, SourceTagLine), SourceTag (FragmentStartTag, FragmentEndTag))

-- Parser tests.

parseSourceLineTest :: Test
parseSourceLineTest = TestCase (do
  -- Source line cases.
  assertEqual "" (SourceTextLine "") (parseSourceLine "")
  assertEqual "" (SourceTextLine "abc") (parseSourceLine "abc")
  assertEqual "" (SourceTextLine "Hello, world!") (parseSourceLine "Hello, world!")
  -- loom:start cases
  assertEqual "" (SourceTagLine (FragmentStartTag ("test"))) (parseSourceLine "loom:start(test)")
  -- loom:end cases
  assertEqual "" (SourceTagLine (FragmentEndTag "test")) (parseSourceLine "loom:end(test)"))
  

trivialTest :: Test
trivialTest = TestCase (assertEqual "1 + 1 = 2" (1 + 1) (2 :: Int))

tests :: Test
tests = TestList [
    TestLabel "trivial" trivialTest,
    TestLabel "parseSourceLine" parseSourceLineTest
  ]


main :: IO ()
main = runTestTTAndExit tests