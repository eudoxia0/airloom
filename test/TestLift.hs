module TestLift (suite) where

import AirLoom.Lift
  ( TransformError (UnclosedTags, UnexpectedEndTag, UnmatchedEndTag),
    groupFragments,
    liftFragments,
    transformSource,
  )
import AirLoom.Parser
  ( SourceLine (SourceTagLine, SourceTextLine),
    SourceTag (FragmentEndTag, FragmentStartTag),
  )
import AirLoom.Store (Store (Store))
import qualified Data.HashMap.Strict as Map
import Test.HUnit

transformSourceTest :: Test
transformSourceTest =
  TestCase
    ( do
        let input =
              [ SourceTagLine (FragmentStartTag "file"),
                SourceTextLine "#include <stdio.h>",
                SourceTextLine "#include <stdlib.h>",
                SourceTextLine "",
                SourceTextLine "int main(void)",
                SourceTextLine "{",
                SourceTagLine (FragmentStartTag "printf"),
                SourceTextLine "    printf(\"Hello, World!\\n\");",
                SourceTagLine (FragmentEndTag "printf"),
                SourceTextLine "    return EXIT_SUCCESS;",
                SourceTextLine "}",
                SourceTagLine (FragmentEndTag "file")
              ]
        let expected =
              Right
                [ (["file"], "#include <stdio.h>"),
                  (["file"], "#include <stdlib.h>"),
                  (["file"], ""),
                  (["file"], "int main(void)"),
                  (["file"], "{"),
                  (["printf", "file"], "    printf(\"Hello, World!\\n\");"),
                  (["file"], "    return EXIT_SUCCESS;"),
                  (["file"], "}")
                ]
        assertEqual "transformSource" expected (transformSource input)
    )

transformSourceUnmatchedEndTagTest :: Test
transformSourceUnmatchedEndTagTest =
  TestCase
    ( do
        let input =
              [ SourceTagLine (FragmentStartTag "foo"),
                SourceTextLine "Test",
                SourceTagLine (FragmentEndTag "bar")
              ]
        let expected = Left (UnmatchedEndTag "foo" "bar")
        assertEqual "transformSource" expected (transformSource input)
    )

transformSourceUnclosedTagTest :: Test
transformSourceUnclosedTagTest =
  TestCase
    ( do
        let input =
              [ SourceTagLine (FragmentStartTag "foo"),
                SourceTextLine "Test"
              ]
        let expected = Left (UnclosedTags ["foo"])
        assertEqual "transformSource" expected (transformSource input)
    )

transformSourceUnexpectedEndTagTest :: Test
transformSourceUnexpectedEndTagTest =
  TestCase
    ( do
        let input =
              [ SourceTextLine "Test",
                SourceTagLine (FragmentEndTag "foo")
              ]
        let expected = Left (UnexpectedEndTag "foo")
        assertEqual "unexpectedEndTag" expected (transformSource input)
    )

groupFragmentsTest :: Test
groupFragmentsTest =
  TestCase
    ( do
        let input =
              [ (["file"], "#include <stdio.h>"),
                (["file"], "#include <stdlib.h>"),
                (["file"], ""),
                (["file"], "int main(void)"),
                (["file"], "{"),
                (["printf", "file"], "    printf(\"Hello, World!\\n\");"),
                (["file"], "    return EXIT_SUCCESS;"),
                (["file"], "}")
              ]
            expected =
              Store $
                Map.fromList
                  [ ("file", "#include <stdio.h>\n#include <stdlib.h>\n\nint main(void)\n{\n    printf(\"Hello, World!\\n\");\n    return EXIT_SUCCESS;\n}"),
                    ("printf", "    printf(\"Hello, World!\\n\");")
                  ]
            result = groupFragments input
        assertEqual "Stores match" expected result
    )

liftFragmentsTest :: Test
liftFragmentsTest =
  TestCase
    ( do
        let input =
              [ SourceTagLine (FragmentStartTag "file"),
                SourceTextLine "#include <stdio.h>",
                SourceTextLine "#include <stdlib.h>",
                SourceTextLine "",
                SourceTextLine "int main(void)",
                SourceTextLine "{",
                SourceTagLine (FragmentStartTag "printf"),
                SourceTextLine "    printf(\"Hello, World!\\n\");",
                SourceTagLine (FragmentEndTag "printf"),
                SourceTextLine "    return EXIT_SUCCESS;",
                SourceTextLine "}",
                SourceTagLine (FragmentEndTag "file")
              ]
            expected =
              Right $
                Store $
                  Map.fromList
                    [ ("file", "#include <stdio.h>\n#include <stdlib.h>\n\nint main(void)\n{\n    printf(\"Hello, World!\\n\");\n    return EXIT_SUCCESS;\n}"),
                      ("printf", "    printf(\"Hello, World!\\n\");")
                    ]
            result = liftFragments input
        assertEqual "Stores match" expected result
    )

suite :: Test
suite =
  TestList
    [ TestLabel "Successful transformSource" transformSourceTest,
      TestLabel "transformSource with unmatched end tag" transformSourceUnmatchedEndTagTest,
      TestLabel "transformSource with unclosed tag" transformSourceUnclosedTagTest,
      TestLabel "transformSource with unexpected end tag" transformSourceUnexpectedEndTagTest,
      TestLabel "groupFragments" groupFragmentsTest,
      TestLabel "liftFragments" liftFragmentsTest
    ]
