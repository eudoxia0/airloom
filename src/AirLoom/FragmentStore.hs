module AirLoom.FragmentStore
  ( Store,
    empty,
    add,
    get,
  )
where

import qualified Data.HashMap.Strict as Map

type Store = Map.HashMap String String

empty :: Store
empty = Map.empty

add :: Store -> String -> String -> Either String Store
add store key value =
  case Map.lookup key store of
    Just _ -> Left "Key already exists in store"
    Nothing -> Right $ Map.insert key value store

get :: Store -> String -> Maybe String
get store key = Map.lookup key store
