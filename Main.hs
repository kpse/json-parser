module Main where


data JsonValue = JsonNull
                | JsonBool Bool
                | JsonNumber Integer
                | JSonString String
                | JsonArray [JsonValue]
                | JsonObject [(String, JsonValue)]
                deriving (Show, Eq)

newtype Parser a = Parser {
  runParser :: String -> Maybe (String, a)
}

jsonNull :: Parser JsonValue
jsonNull = const JsonNull <$> stringP "null"

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

jsonValue :: Parser JsonValue
jsonValue = jsonNull
                                                    

main :: IO ()
main = do
  print $ runParser (charP 'n') "hello"
  print $ runParser (charP 'n') "nice"
  print $ runParser (stringP "null") "nice"
  print $ runParser (stringP "null") "nullabc"
