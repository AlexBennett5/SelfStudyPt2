module MonadParser where

newtype Parser a = Parser (parse :: String -> [(a, String)])

item :: Parser Char
item = Parser (\cs -> case cs of
                          ""      -> []
                          (c:cs)  -> [(c,cs)])

instance Monad Parser where
  return a  = Parser (\cs -> [(a, cs)])
  p >>= f   = Parser (\cs -> concat [parse (f a) cx | (a, cx) <- parse p cs])

instance MonadZero Parser where
  zero = Parser (\cs -> [])

instance MonadPlus Parser where
  p ++ q = Parser (\cs -> parse p cs ++ parse q cs)

(+++) :: Parser a -> Parser a -> Parser a
  p +++ q = Parser (\cs -> case parse (p ++ q) cs of
                              []      -> []
                              (x:xs)  -> [x])

sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item
  if p c then return c else zero

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser String
string "" = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- many p; return (a:as)}

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do a <- p
                    as <- many (do {sep; p})
                    return (a:as)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

-- Lexical combinators --

space :: Parser String
space = many (sat isSpace)

token :: Parser a -> Parser a
token p = do {a <- p; space; return a}
