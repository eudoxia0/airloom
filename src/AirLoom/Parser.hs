module AirLoom.Parser where 

-- The types of tags we can encounter in source files.
data SourceTag = FragmentStartTag String
               | FragmentEndTag String

-- The types of tags we can encounter in documentation files.
data DocTag = TranscludeTag String

loomStartRegex :: String
loomStartRegex = "[^\\\\]loom:start\\(([a-zA-Z0-9,':]+)\\)"

loomEndRegex :: String
loomEndRegex = "[^\\\\]loom:start\\(([a-zA-Z0-9,':]+)\\)"

