{-# LANGUAGE OverloadedStrings #-}
module TestStore (suite) where

import AirLoom.Store (Store (Store))
import Data.Aeson (encode, eitherDecode)
import qualified Data.HashMap.Strict as Map
import Test.HUnit

toJsonTest :: Test
toJsonTest =
  TestCase
    ( do
        let store = Store $ Map.fromList [("key1", "value1"), ("key2", "value2")]
        let expected = "{\"key1\":\"value1\",\"key2\":\"value2\"}"
        let encoded = encode store
        assertEqual "Serializing Store to JSON" expected encoded
    )

fromJsonTest :: Test
fromJsonTest =
  TestCase
    ( do
        let json = "{\"key1\":\"value1\",\"key2\":\"value2\"}"
        let expected = Store (Map.fromList [("key1", "value1"), ("key2", "value2")])
        let result = eitherDecode json :: Either String Store
        assertEqual "Parsing store from JSON" (Right expected) result
    )

suite :: Test
suite =
  TestList
    [ TestLabel "Serialize to JSON" toJsonTest,
      TestLabel "Deserialize from JSON" fromJsonTest
    ]
