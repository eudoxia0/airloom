module AirLoom.Weave (WeaveError (..), weave) where

import AirLoom.Parser
  ( DocLine (DocTagLine, DocTextLine),
    DocTag (TranscludeTag),
  )
import AirLoom.Store (FragmentName, Store, get)
import Data.Char (isSpace)
import Data.List (intercalate)

data WeaveError = UnknownFragmentError FragmentName
  deriving (Eq, Show)

weave :: [DocLine] -> Store -> Either WeaveError [String]
weave ls store = traverse (\line -> weaveLine line store) ls

weaveLine :: DocLine -> Store -> Either WeaveError String
weaveLine (DocTextLine s) _ = Right s
weaveLine (DocTagLine (TranscludeTag name)) store =
  case get store name of
    Just contents -> Right $ trimPrefixes contents
    Nothing -> Left (UnknownFragmentError name)

trimPrefixes :: String -> String
trimPrefixes s = (intercalate "\n") . map (drop commonIndent) . lines $ s
  where
    indents = map (length . takeWhile isSpace) . lines $ s
    commonIndent = minimum indents
