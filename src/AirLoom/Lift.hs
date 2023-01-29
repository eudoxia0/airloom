module AirLoom.Lift (transformSource, TransformError (..)) where

import AirLoom.Parser
  ( SourceLine (SourceTagLine, SourceTextLine),
    SourceTag (FragmentEndTag, FragmentStartTag),
  )
import Data.Maybe (mapMaybe)
import AirLoom.Store (FragmentName)

data TransformError
  = UnexpectedEndTag FragmentName
  | UnmatchedEndTag FragmentName FragmentName
  | UnclosedTags [FragmentName]
  deriving (Eq, Show)

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
