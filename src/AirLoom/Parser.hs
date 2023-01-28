module AirLoom.Parser where 

-- The types of tags we can encounter in source files.
data SourceTag = FragmentStartTag String
               | FragmentEndTag String

-- The types of tags we can encounter in documentation files.
data DocTag = TranscludeTag String

-- The type of lines in source files.
data SourceLine = SourceTextLine String
                | SourceTagLine SourceTag

-- The type of lines in documentation files.
data DocLine = DocTextLine String
             | DocTagLine DocTag

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