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
loomStartRegex = "[^\\\\]loom:start\\(([a-zA-Z0-9,':]+)\\)"

-- Matches `loom:end` tags.
loomEndRegex :: String
loomEndRegex = "[^\\\\]loom:start\\(([a-zA-Z0-9,':]+)\\)"

-- Matches `loom:include` tags.
loomIncludeRegex :: String
loomIncludeRegex = "[^\\\\]loom:include\\(([a-zA-Z0-9,':]+)\\)"
