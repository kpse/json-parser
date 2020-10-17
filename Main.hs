module Main where

import Control.Applicative
import Data.Char (isSpace, isDigit)
data JsonValue = JsonNull
                | JsonBool Bool
                | JsonNumber Double
                | JsonString String
                | JsonArray [JsonValue]
                | JsonObject [(String, JsonValue)]
                deriving (Show, Eq)

newtype Parser a = Parser {
  runParser :: String -> Maybe (String, a)
}

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

notEmpty :: Parser String -> Parser String
notEmpty (Parser p) = Parser $ \i -> do
    (i2, rest) <- p i
    case rest of
        [] -> Nothing
        _ -> Just (i2, rest)


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


spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> let (m, rest) = span f input in Just (rest, m)

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y:ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP


instance Functor Parser where
  fmap f (Parser p) = Parser g
    where
      g input = do
        (input', x) <- p input
        Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
                                                    (i2, f) <- p1 input
                                                    (i3, a) <- p2 i2
                                                    Just (i3, f a)


instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser a1) <|> (Parser a2) = Parser $ \i -> a1 i <|> a2 i

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
  print $ runParser jsonObject "{\"key\":\"a\",\"k\":42}"
  print $ runParser pairKey "\"key\""
  print $ runParser (charP '{' *> ws *> commaSep <* ws <* charP '}') "{,}"
  print $ runParser (charP '{' *> ws *> pairKey <* ws <* charP '}') "{\"somekey\"}"

