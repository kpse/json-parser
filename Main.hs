module Main where

import Control.Applicative ( (<|>), many)
import Data.Char (isSpace, isDigit)
import Parser ( Parser(..), notEmpty, spanP, charP )
-- import ParserE ( Parser(..), notEmpty, spanP, charP )

data JsonValue = JsonNull
                | JsonBool Bool
                | JsonNumber Double
                | JsonString String
                | JsonArray [JsonValue]
                | JsonObject [(String, JsonValue)]
                deriving (Show, Eq)

jsonNull :: Parser JsonValue
jsonNull = const JsonNull <$> stringP "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
    where f "true" = JsonBool True
          f "false" = JsonBool False
          f _ = undefined

jsonString :: Parser JsonValue
jsonString = charP '"' *> f <* charP '"'
    where f = JsonString <$> spanP (/= '"')

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notEmpty (spanP included)
    where
        f = JsonNumber . read
        included a = isDigit a || (== '.') a




jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *> ele <* ws <* charP ']')
    where ele = sepBy (ws *> charP ',' <* ws) jsonValue

sepBy :: Parser Char -> Parser JsonValue -> Parser [JsonValue]
sepBy s cur = (:) <$> cur <*> many (s *> cur ) <|> pure []

pairBy :: Parser [(String, JsonValue)]
pairBy = (:) <$> pair <*> many (commaSep *> pair)
    where pair = (,) <$> pairKey <*> jsonValue

pairKey :: Parser String
pairKey = quoteP *> spanP (/= '"') <* quoteP <* colonP

colonP :: Parser Char
colonP = ws *> charP ':' <* ws

commaSep :: Parser Char
commaSep = ws *> charP ',' <* ws

quoteP :: Parser Char
quoteP  = charP '"'

startBrace :: Parser Char
startBrace = ws *> charP '{' <* ws

endBrace :: Parser Char
endBrace = ws *> charP '}' <* ws


ws :: Parser String
ws = spanP isSpace

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (startBrace *> pairBy <* endBrace)



stringP :: String -> Parser String
stringP = traverse charP



jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject


main :: IO ()
main = do
  print $ runParser (charP 'n') "hello"
  print $ runParser (charP 'n') "nice"
  print $ runParser (stringP "null") "nice"
  print $ runParser jsonValue "nullabc"
  print $ runParser (stringP "true") "trueabc"
  print $ runParser jsonValue "trueabc"
  print $ runParser jsonValue "\"trueab\"c"
  print $ runParser jsonValue "123p"
  print $ runParser jsonNumber "p"
  print $ runParser jsonNumber "3.14"
  print $ runParser jsonArray "[]"
  print $ runParser jsonValue "[2, true, false, [], \"123\"]"
  print $ runParser jsonValue "[{\"key\": 42}, true, false, [], \"123\"]"
  print $ runParser jsonValue "{\"key\":\"a\",\"k\":42}"
  print $ runParser pairKey "\"key\":"
  print $ runParser (charP '{' *> ws *> commaSep <* ws <* charP '}') "{,}"
  print $ runParser (charP '{' *> ws *> pairKey <* ws <* charP '}') "{\"somekey\":}"

