module AirLoom.FragmentStore where

import Data.Either (Either (..))
import qualified Data.HashMap.Strict as Map

type Store = Map.HashMap String String

emptyStore :: Store
emptyStore = Map.empty

addFragment :: Store -> String -> String -> Either String Store
addFragment store key value =
  case Map.lookup key store of
    Just _ -> Left "Key already exists in store"
    Nothing -> Right $ Map.insert key value store

lookupFragment :: Store -> String -> Maybe String
lookupFragment store key = Map.lookup key store
