module AirLoom.Lift (liftFragments, transformSource, TransformError (..), groupFragments) where

import AirLoom.Parser
  ( SourceLine (SourceTagLine, SourceTextLine),
    SourceTag (FragmentEndTag, FragmentStartTag),
  )
import AirLoom.Store (FragmentName, Store, append, empty)
import qualified Data.HashSet as Set
import Data.Maybe (mapMaybe)

data TransformError
  = UnexpectedEndTag FragmentName
  | UnmatchedEndTag FragmentName FragmentName
  | UnclosedTags [FragmentName]
  | DuplicateFragment FragmentName
  deriving (Eq, Show)

-- Entrypoint: from source lines to store.

liftFragments :: [SourceLine] -> Either TransformError Store
liftFragments lst = do
  _ <- checkStartTagUniqueness lst
  ann <- transformSource lst
  return $ groupFragments ann

-- Check we don't have multiple fragments with the same name within a file.

checkStartTagUniqueness :: [SourceLine] -> Either TransformError [SourceLine]
checkStartTagUniqueness lines =
  case findFirstDuplicate (mapMaybe startTag lines) of
    (Just dup) -> Left $ DuplicateFragment dup
    Nothing -> Right lines

startTag :: SourceLine -> Maybe String
startTag line =
  case line of
    SourceTagLine (FragmentStartTag name) -> Just name
    _ ->
      Nothing

findFirstDuplicate :: [String] -> Maybe String
findFirstDuplicate list =
  findFirstDuplicate' list Set.empty

findFirstDuplicate' :: [String] -> Set.HashSet String -> Maybe String
findFirstDuplicate' list seen =
  case list of
    (elem : rest) ->
      if Set.member elem seen
        then Just elem
        else findFirstDuplicate' rest (Set.insert elem seen)
    [] -> Nothing

-- Annotate each source line with the fragments it belongs to.

type TagStack = [FragmentName]

transformSource :: [SourceLine] -> Either TransformError [(TagStack, String)]
transformSource ls = do
  (stack, ls') <- transformLines [] ls
  if null stack
    then Right (mapMaybe discardEmpty ls')
    else Left (UnclosedTags stack)

transformLines :: TagStack -> [SourceLine] -> Either TransformError (TagStack, [(TagStack, Maybe String)])
transformLines stack [] = Right (stack, [])
transformLines stack (line : ls) = do
  (newStack, result) <- transformLine stack line
  (finalStack, ls') <- transformLines newStack ls
  Right (finalStack, (newStack, result) : ls')

transformLine :: TagStack -> SourceLine -> Either TransformError (TagStack, Maybe String)
transformLine stack line =
  case line of
    -- Start of a fragment: put the name in the stack.
    SourceTagLine (FragmentStartTag tag) -> Right (tag : stack, Nothing)
    -- End of a fragment: pop the stack.
    SourceTagLine (FragmentEndTag tag) -> case stack of
      -- If the stack is empty, this is an unexpected end tag.
      [] -> Left (UnexpectedEndTag tag)
      -- If the top of the stack doesn't match the fragment name here, this
      -- is an unmatched end tag.
      (x : xs) ->
        if x == tag
          then Right (xs, Nothing)
          else Left (UnmatchedEndTag x tag)
    -- Ordinary line of text.
    SourceTextLine s -> Right (stack, Just s)

discardEmpty :: (TagStack, Maybe String) -> Maybe (TagStack, String)
discardEmpty (stack, maybeString) = case maybeString of
  Just s -> Just (stack, s)
  Nothing -> Nothing

-- Extract tagged lines into a store.

groupFragments :: [(TagStack, String)] -> Store
groupFragments l =
  foldl processLine empty l
  where
    processLine h (stack, line) = foldl processTag h stack
      where
        processTag h' name = append h' name line
