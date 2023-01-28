module Parser where
import Text.Parsec (many1, manyTill, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, char, anyChar, alphaNum, oneOf, newline)

-- The types of tags we can encounter in source files.
data SourceTag = FragmentStartTag String
               | FragmentEndTag String

fragmentStartLine :: Parser SourceTag
fragmentStartLine = do
  _ <- manyTill anyChar (string "loom:start")
  tag <- fragmentStartTag
  _ <- manyTill anyChar newline
  return tag


fragmentStartTag :: Parser SourceTag
fragmentStartTag = do
  _ <- string "loom:start"
  _ <- char '('
  name <- fragmentName
  _ <- char ')'
  return (FragmentStartTag name)

fragmentName :: Parser String
fragmentName =
  many1 nameChar

nameChar :: Parser Char
nameChar =
  alphaNum <|> oneOf " ,':"