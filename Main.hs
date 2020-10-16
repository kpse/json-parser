module Main where

import Control.Applicative
import Data.Char (isDigit)
data JsonValue = JsonNull
                | JsonBool Bool
                | JsonNumber Integer
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
jsonNumber = f <$> spanP isDigit
    where f = JsonNumber . read

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
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString


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
