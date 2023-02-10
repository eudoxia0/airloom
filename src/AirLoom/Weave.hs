module AirLoom.Weave where
import AirLoom.Parser
  ( DocLine (DocTextLine, DocTagLine),
    DocTag (TranscludeTag),
  )
import AirLoom.Store (FragmentName, Store, get)

data WeaveError = UnknownFragmentError FragmentName
  deriving (Eq, Show)

weave :: [DocLine] -> Store -> Either WeaveError [String]
weave ls store = traverse (\line -> weaveLine line store) ls

weaveLine :: DocLine -> Store -> Either WeaveError String
weaveLine (DocTextLine s) _ = Right s
weaveLine (DocTagLine (TranscludeTag name)) store =
  case get store name of
    Just contents -> Right contents
    Nothing -> Left (UnknownFragmentError name)
