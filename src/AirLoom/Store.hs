module AirLoom.Store
  ( Store,
    FragmentName,
    FragmentContents,
    InsertionError (DuplicateFragment)
    empty,
    add,
    get,
  )
where

import qualified Data.HashMap.Strict as Map

type FragmentName = String

type FragmentContents = String

type Store = Map.HashMap FragmentName FragmentContents

type InsertionError = DuplicateFragment FragmentName

empty :: Store
empty = Map.empty

add :: Store -> FragmentName -> FragmentContents -> Either InsertionError Store
add store key value =
  case Map.lookup key store of
    Just _ -> Left $ DuplicateFragment key
    Nothing -> Right $ Map.insert key value store

get :: Store -> FragmentName -> Maybe FragmentContents
get store key = Map.lookup key store
