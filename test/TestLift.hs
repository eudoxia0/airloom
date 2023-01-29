module TestLift (suite) where

import AirLoom.Lift (transformSource)
import AirLoom.Parser
  ( SourceLine (SourceTagLine, SourceTextLine),
    SourceTag (FragmentEndTag, FragmentStartTag),
  )
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

suite :: Test
suite =
  TestList
    [ TestLabel "transformSource" transformSourceTest
    ]
