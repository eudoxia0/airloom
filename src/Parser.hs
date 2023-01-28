module Parser where
import Text.Parsec (many1, manyTill, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, char, anyChar, alphaNum, oneOf, noneOf, newline)

-- The types of tags we can encounter in source files.
data SourceTag = FragmentStartTag String
               | FragmentEndTag String

-- Parses 'loom:start', unless prefixed by a backslash.
loomStart :: Parser String
loomStart = do
  _ <- noneOf "\\"
  string "loom:start"

-- Parses a line containing a `loom:start` tag, and returns the name.
fragmentStartLine :: Parser SourceTag
fragmentStartLine = do
  _ <- manyTill anyChar loomStart
  tag <- fragmentStartTag
  _ <- manyTill anyChar newline
  return tag

-- Parses the contents of a `loom:start` tag.
fragmentStartTag :: Parser SourceTag
fragmentStartTag = do
  _ <- char '('
  name <- fragmentName
  _ <- char ')'
  return (FragmentStartTag name)

-- Parses the name of a fragment.
fragmentName :: Parser String
fragmentName =
  many1 nameChar

-- Valid characters for fragment names.
nameChar :: Parser Char
nameChar =
  alphaNum <|> oneOf " ,':"