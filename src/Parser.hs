module Parser where
import Text.Parsec (many1, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, char, alphaNum, oneOf)

-- The types of tags we can encounter in source files.
data SourceTag = FragmentStartTag String
               | FragmentEndTag String

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