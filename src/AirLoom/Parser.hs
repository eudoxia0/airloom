module AirLoom.Parser where 

-- The types of tags we can encounter in source files.
data SourceTag = FragmentStartTag String
               | FragmentEndTag String

-- The types of tags we can encounter in documentation files.
data DocTag = TranscludeTag String

-- The type of lines in source files: either a source line or a tag like
-- `loom:start` or `loom:end`.
data SourceLine = SourceTextLine String
                | SourceTagLine SourceTag

-- The type of lines in documentation files: either a text line or a tag like
-- `loom:include`.
data DocLine = DocTextLine String
             | DocTagLine DocTag

-- Given a line of text, returns a `SourceTagLine` if there's a `loom:*` tag,
-- or a `SourceTextLine` otherwise.
parseSourceLine :: String -> SourceLine
parseSourceLine line =
  ""
  
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
  "[^\\\\]" ++ s ++ "\\(([a-zA-Z0-9,':]+)\\)"