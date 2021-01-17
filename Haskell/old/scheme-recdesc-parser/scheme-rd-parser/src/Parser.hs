module Parser where

import qualified Tokenizer as Tok

type Identifier = String

data Atomic = BoolVal Bool
            | NumVal Int
            | IdenVal Identifier
            deriving (Show)

data Expr = AtomicExpr (Atomic)
          | QuoteExpr (Expr)
          | DefineVarExpr (String, Expr)
          | DefineFuncExpr (String, [IdenVal], Expr)
          | AppFuncExpr (String, [Atomic])
          | LambdaExpr ([Iden], Expr)
          | IfElseExpr (Expr, Expr, Expr)
          deriving (Show)

parseExpr :: [Token] -> Maybe ([Token], Expr)
parseExpr (LPAREN:rest) = case parseExpr rest of
                            Just (rest, finExpr) -> case rest of
                                                      (RPAREN:rest) -> Just (rest, finExpr)
                                                      _             -> Nothing
                            _                    -> Nothing 
parseExpr (QUOTE:rest) = case parseExpr rest of
                            Just (rest, finExpr) -> Just (rest, QuoteExpr (finExpr))
                            _                    -> Nothing
parseExpr (DEFINE:rest) = parseDefine rest



parseExprs :: [Token] -> [Expr] -> Maybe [Expr]
parseExprs tok [] = case parseExpr tok of
                       Just (rest, ex) -> parseExprs rest [ex]
                       _               -> Nothing
parseExprs tok exs = case parseExpr tok of
                       Just (rest, ex) -> parseExprs rest (exs ++ [ex])
                       _               -> exs

parseToplevel :: [Char] -> Maybe [Expr]
parseToplevel str = parseExprs (Tok.tokenize str) []

