module AirLoom.Store
  ( Store (Store),
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

import Data.Aeson
import qualified Data.HashMap.Strict as Map

type FragmentName = String

type FragmentContents = String

data Store = Store (Map.HashMap FragmentName FragmentContents)
  deriving (Eq, Show)

data InsertionError = DuplicateFragment FragmentName

empty :: Store
empty = Store Map.empty

add :: Store -> FragmentName -> FragmentContents -> Either InsertionError Store
add (Store store) key value =
  case Map.lookup key store of
    Just _ -> Left $ DuplicateFragment key
    Nothing -> Right $ Store $ Map.insert key value store

get :: Store -> FragmentName -> Maybe FragmentContents
get (Store store) key = Map.lookup key store

-- If `name` is in `store`, appends a newline and `text` to its value. If
-- `name` is not in `store`, adds an entry mapping `name` to `text`.
append :: Store -> FragmentName -> FragmentContents -> Store
append (Store store) name text =
  case Map.lookup name store of
    Just existing -> Store $ Map.insert name (existing ++ "\n" ++ text) store
    Nothing -> Store $ Map.insert name text store

-- Merge two stores. If they have any keys in common, returns an
-- `InsertionError`.
merge :: Store -> Store -> Either InsertionError Store
merge (Store a) (Store b) =
  if Map.intersection a b == Map.empty
    then Right $ Store $ Map.union a b
    else Left $ DuplicateFragment $ head $ Map.keys $ Map.intersection a b
