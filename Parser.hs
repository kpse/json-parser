module Parser (Parser(..), notEmpty, spanP, charP) where

import Control.Applicative ( Alternative((<|>), empty) )

newtype Parser a = Parser {
  runParser :: String -> Maybe (String, a)
}


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


notEmpty :: Parser String -> Parser String
notEmpty (Parser p) = Parser $ \i -> do
    (i2, rest) <- p i
    case rest of
        [] -> Nothing
        _ -> Just (i2, rest)

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> let (m, rest) = span f input in Just (rest, m)
        
charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y:ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing