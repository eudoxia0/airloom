module AirLoom.Store
  ( Store,
    FragmentName,
    FragmentContents,
    empty,
    add,
    get,
  )
where

import qualified Data.HashMap.Strict as Map

type FragmentName = String

type FragmentContents = String

type Store = Map.HashMap FragmentName FragmentContents

empty :: Store
empty = Map.empty

add :: Store -> FragmentName -> FragmentContents -> Maybe Store
add store key value =
  case Map.lookup key store of
    Just _ -> Nothing
    Nothing -> Just $ Map.insert key value store

get :: Store -> FragmentName -> Maybe FragmentContents
get store key = Map.lookup key store
