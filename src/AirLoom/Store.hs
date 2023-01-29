module AirLoom.Store
  ( Store,
    FragmentName,
    FragmentContents,
    InsertionError (DuplicateFragment),
    empty,
    add,
    get,
    append,
    merge
  )
where

import qualified Data.HashMap.Strict as Map

type FragmentName = String

type FragmentContents = String

type Store = Map.HashMap FragmentName FragmentContents

data InsertionError = DuplicateFragment FragmentName

empty :: Store
empty = Map.empty

add :: Store -> FragmentName -> FragmentContents -> Either InsertionError Store
add store key value =
  case Map.lookup key store of
    Just _ -> Left $ DuplicateFragment key
    Nothing -> Right $ Map.insert key value store

get :: Store -> FragmentName -> Maybe FragmentContents
get store key = Map.lookup key store

-- If `name` is in `store`, appends a newline and `text` to its value. If
-- `name` is not in `store`, adds an entry mapping `name` to `text`.
append :: Store -> FragmentName -> FragmentContents -> Store
append store name text =
  case Map.lookup name store of
    Just existing -> Map.insert name (existing ++ "\n" ++ text) store
    Nothing -> Map.insert name text store

-- Merge two stores. If they have any keys in common, returns an
-- `InsertionError`.
merge :: Store -> Store -> Either InsertionError Store
merge a b =
  if Map.intersection a b == Map.empty
    then Right $ Map.union a b
    else Left $ DuplicateFragment $ head $ Map.keys $ Map.intersection a b
