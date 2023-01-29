module AirLoom.Parser (
  SourceTag (..),
  DocTag (..),
  SourceLine (..),
  DocLine (..),
  parseSourceFile,
  parseSourceLine,
  parseDocLine
) where 
import Text.Regex.TDFA

-- The type of lines in source files: either a source line or a tag like
-- `loom:start` or `loom:end`.
data SourceLine = SourceTextLine String
                | SourceTagLine SourceTag
  deriving (Eq, Show)

-- The type of lines in documentation files: either a text line or a tag like
-- `loom:include`.
data DocLine = DocTextLine String
             | DocTagLine DocTag
  deriving (Eq, Show)

-- The types of tags we can encounter in source files.
data SourceTag = FragmentStartTag String
               | FragmentEndTag String
  deriving (Eq, Show)

-- The types of tags we can encounter in documentation files.
data DocTag = TranscludeTag String
  deriving (Eq, Show)

-- Parse all of a source file.
parseSourceFile :: String -> [SourceLine]
parseSourceFile fileContent = map parseSourceLine (lines fileContent)

-- Given a line of text, returns a `SourceTagLine` if there's a `loom:*` tag,
-- or a `SourceTextLine` otherwise.
parseSourceLine :: String -> SourceLine
parseSourceLine line =
  case parseLoomStart line of
    Just l -> l
    Nothing -> (case parseLoomEnd line of
                Just l -> l
                Nothing -> SourceTextLine line)

-- Given a line of text, returns a `DocTagLine` if there's a `loom:*` tag, or a
-- `DocTextLine` otherwise.
parseDocLine :: String -> DocLine
parseDocLine line =
  case parseLoomInclude line of
    Just l -> l
    Nothing -> DocTextLine line

-- Try parsing a `loom:start` tag.
parseLoomStart :: String -> Maybe SourceLine
parseLoomStart line =
  case line =~ loomStartRegex :: (String, String, String, [String]) of
    (_, _, _, text : []) -> Just $ SourceTagLine $ FragmentStartTag text
    _                    -> Nothing

-- Try parsing a `loom:end` tag.
parseLoomEnd :: String -> Maybe SourceLine
parseLoomEnd line =
  case line =~ loomEndRegex :: (String, String, String, [String]) of
    (_, _, _, text : []) -> Just $ SourceTagLine $ FragmentEndTag text
    _                    -> Nothing

-- Try parsing a `loom:include` tag.
parseLoomInclude :: String -> Maybe DocLine
parseLoomInclude line =
  case line =~ loomIncludeRegex :: (String, String, String, [String]) of
    (_, _, _, text : []) -> Just $ DocTagLine $ TranscludeTag text
    _                    -> Nothing

-- Matches `loom:start` tags.
loomStartRegex :: String
loomStartRegex = tagRegex "loom:start"

-- Matches `loom:end` tags.
loomEndRegex :: String
loomEndRegex = tagRegex "loom:end"

-- Matches `loom:include` tags.
loomIncludeRegex :: String
loomIncludeRegex = tagRegex "loom:include"

-- Makes a regular expression for matching a tag with the given name.
tagRegex :: String -> String
tagRegex s =
  "^.*" ++ s ++ "\\(([a-zA-Z0-9,':]+)\\).*$"