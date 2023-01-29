module AirLoom.Lift where

import AirLoom.Parser
  ( SourceLine (SourceTagLine, SourceTextLine),
    SourceTag (FragmentEndTag, FragmentStartTag),
  )
import Control.Monad (foldM)
import Data.Maybe (mapMaybe)

data TransformError
  = UnexpectedEndTag String
  | UnmatchedEndTag String String
  | UnclosedTags [String]
  deriving (Eq, Show)

type TagStack = [String]

transformSource :: [SourceLine] -> Either TransformError [(TagStack, String)]
transformSource lines = do
  (stack, lines) <- transformLines [] lines
  if null stack
    then Right (mapMaybe discardEmpty lines)
    else Left (UnclosedTags stack)

transformLines :: TagStack -> [SourceLine] -> Either TransformError (TagStack, [(TagStack, Maybe String)])
transformLines stack [] = Right (stack, [])
transformLines stack (line : lines) = do
  (newStack, result) <- transformLine stack line
  (finalStack, rest) <- transformLines newStack lines
  Right (finalStack, (newStack, result) : rest)

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
