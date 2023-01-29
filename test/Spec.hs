module Main (main) where
import Test.HUnit
import AirLoom.Parser (parseSourceLine, SourceLine (SourceTextLine, SourceTagLine), SourceTag (FragmentStartTag, FragmentEndTag), parseSourceFile)

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

parseSourceFileTest :: Test
parseSourceFileTest = TestCase (do
    assertEqual "empty file" [] (parseSourceFile "")
    assertEqual "one text line" [SourceTextLine "abc"] (parseSourceFile "abc")
    assertEqual "two text lines" [SourceTextLine "abc", SourceTextLine "def"] (parseSourceFile "abc\ndef")
    assertEqual "text line and start tag" [SourceTextLine "abc", SourceTagLine (FragmentStartTag "foo")] (parseSourceFile "abc\nloom:start(foo)")
    assertEqual "start tag and end tag" [SourceTagLine (FragmentStartTag "bar"), SourceTagLine (FragmentEndTag "bar")] (parseSourceFile "loom:start(bar)\nloom:end(bar)")
    assertEqual "text lines, start and end tags" [SourceTextLine "abc", SourceTagLine (FragmentStartTag "foo"), SourceTextLine "def", SourceTagLine (FragmentEndTag "foo"), SourceTextLine "ghi"] (parseSourceFile "abc\nloom:start(foo)\ndef\nloom:end(foo)\nghi"))

trivialTest :: Test
trivialTest = TestCase (assertEqual "1 + 1 = 2" (1 + 1) (2 :: Int))

tests :: Test
tests = TestList [
    TestLabel "trivial" trivialTest,
    TestLabel "parseSourceLine" parseSourceLineTest,
    TestLabel "parseSourceFile" parseSourceFileTest
  ]


main :: IO ()
main = runTestTTAndExit tests