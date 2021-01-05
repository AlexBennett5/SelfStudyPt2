module Tokenizer where

data Token = LPAREN
           | RPAREN
           | DEFINE
           | QUOTE
           | LAMBDA
           | IF
           | PRIMITIVE String
           | INT String
           | IDEN String
           | INVALID_TOKEN
     deriving (Show, Eq)

isNum :: Char -> Bool
isNum x = x `elem` ['0'..'9']

isAlpha :: Char -> Bool
isAlpha x = x `elem` ['a'..'z']++['A'..'Z']

primitives :: [[Char]]
primitives = ["+", "-", "*", "/", "<", "<=", "and", "or", "car", "cdr"]

isPrimitive :: [Char] -> Bool
isPrimitive x = x `elem` primitives

isInt :: [Char] -> Bool
isInt [] = True
isInt (x:xs)
  | isNum x = isInt xs
  | otherwise = False

isIden :: [Char] -> Bool
isIden [] = True
isIden (x:xs)
  | isAlpha x = isRestIden xs
  | otherwise = False

isRestIden :: [Char] -> Bool
isRestIden [] = True
isRestIden (x:xs)
  | isAlpha x = isRestIden xs
  | isNum x = isRestIden xs
  | otherwise = False

processWord :: [Char] -> Token
processWord word 
  | word == "(" = LPAREN
  | word == ")" = RPAREN
  | word == "define" = DEFINE
  | word == "'" = QUOTE
  | word == "lambda" = LAMBDA
  | word == "if" = IF
  | isPrimitive(word) = PRIMITIVE word
  | isInt(word) = INT word
  | isIden(word) = IDEN word
  | otherwise = INVALID_TOKEN

untilNotNum :: [Char] -> [Char] -> ([Char], [Char])
untilNotNum (x:xs) acc
  | isNum x = untilNotNum xs (acc ++ [x])
  | otherwise = (acc, x:xs)

untilNotAlphanum :: [Char] -> [Char] -> ([Char], [Char])
untilNotAlphanum (x:xs) acc
  | isAlpha x = untilNotAlphanum xs (acc ++ [x])
  | isNum x = untilNotAlphanum xs (acc ++ [x])
  | otherwise = (acc, x:xs)

untilWhitespace :: [Char] -> [Char] -> ([Char], [Char])
untilWhitespace (x:xs) acc
  | x == ' ' = (acc, xs)
  | otherwise = untilWhitespace xs (acc ++ [x])

splitChars :: [Char] -> ([Char], [Char])
splitChars [] = ([], [])
splitChars (' ':xs) = splitChars xs
splitChars (x:[]) = ([x], [])
splitChars (x:xs)
  | isNum x = untilNotNum xs [x]
  | isAlpha x = untilNotAlphanum xs [x]
  | x == '(' = ([x], xs)
  | x == ')' = ([x], xs)
  | x == '\'' = ([x], xs)
  | otherwise = untilWhitespace xs [x] 

tokenize :: [Char] -> [Token]
tokenize [] = []
tokenize str = [processWord word] ++ (tokenize rest)
  where splitStr = splitChars str
        word = fst splitStr
        rest = snd splitStr
           
