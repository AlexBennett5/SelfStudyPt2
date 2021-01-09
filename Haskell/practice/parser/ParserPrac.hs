import Control.Monad

newtype Parser a = Parser { parse :: String -> [(a, String)] }

item :: Parser Char
item = Parser (\s -> case s of 
                ""     -> []
                (c:cs) -> [(c,cs)] )

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser (\cs -> [(a,cs)])
  p >>= f = Parser (\cs -> concat [parse (f a) s | (a, s) <- parse p cs])

oneThree :: Parser (Char, Char)
oneThree = do { c <- item; item; d <- item; return (c, d) }

main :: IO ()
main = do
  sequence_ [putStrLn ([a] ++ " " ++ [b] ++ " " ++ c) | ((a,b), c) <- parse oneThree "hey"]
