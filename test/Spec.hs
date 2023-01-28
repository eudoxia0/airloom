module Main (main) where
import Test.HUnit
import AirLoom.Parser (parseSourceLine, SourceLine (SourceTextLine, SourceTagLine), SourceTag (FragmentStartTag, FragmentEndTag), parseLoomStart)

-- Parser tests.

parseSourceLineTest :: Test
parseSourceLineTest = TestCase (do
  -- Source line cases.
  assertEqual "" (SourceTextLine "") (parseSourceLine "")
  assertEqual "" (SourceTextLine "abc") (parseSourceLine "abc")
  assertEqual "" (SourceTextLine "Hello, world!") (parseSourceLine "Hello, world!")
  assertEqual "" (SourceTextLine " loom:start(broken") (parseSourceLine " loom:start(broken")
  -- loom:start cases
  assertEqual "" (SourceTagLine (FragmentStartTag "b")) (parseSourceLine "   loom:start(b)   ")
  assertEqual "" (SourceTagLine (FragmentStartTag "a")) (parseSourceLine "loom:start(a)")
  -- loom:end cases
  assertEqual "" (SourceTagLine (FragmentEndTag "c")) (parseSourceLine "loom:end(c)")
  assertEqual "" (SourceTagLine (FragmentEndTag "d")) (parseSourceLine "    loom:end(d)   "))
  

trivialTest :: Test
trivialTest = TestCase (assertEqual "1 + 1 = 2" (1 + 1) (2 :: Int))

tests :: Test
tests = TestList [
    TestLabel "trivial" trivialTest,
    TestLabel "parseSourceLine" parseSourceLineTest
  ]


main :: IO ()
main = runTestTTAndExit tests