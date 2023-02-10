module TestWeave (suite) where

successfulWeaveTest :: Test
successfulWeaveTest =
  TestCase
    ( do
        let store = Store $ Map.fromList [("W", "world!")]
        -- Test successful weaving
        let input = [DocTextLine "Hello", DocTagLine $ TranscludeTag "W"]
        let expected = Right ["Hello", "world!"]
        let result = weave input store
        assertEqual "Weave should succeed" expected result
    )

unknownFragmentTest :: Test
unknownFragmentTest =
  TestCase
    ( do
        let store = Store $ Map.fromList [("W", "world!")]
        -- Test unknown fragment error
        let input = [DocTextLine "Hello", DocTagLine $ TranscludeTag "unknownKey", DocTextLine "world!"]
        let expected = Left (UnknownFragmentError "unknownKey")
        let result = weave input store
        assertEqual "Weave should fail with unknown fragment error" expected result
    )

suite :: Test
suite =
  TestList
    [ TestLabel "Successful weaving" successfulWeaveTest,
      TestLabel "Unknown fragment error" unknownFragmentTest
    ]
