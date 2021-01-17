import Tokenizer
import Test.HUnit

tokTest :: String -> String -> [Token] -> Test
tokTest name str tok = TestCase $ assertEqual name tok (tokenize str) 

tests :: Test
tests = TestList [ tokTest "()" "()" [LPAREN, RPAREN]
                 , tokTest "(3)" "(3)" [LPAREN, INT "3", RPAREN]
                 , tokTest "(+ 1 2)" "(+ 1 2)" [LPAREN, PRIMITIVE "+", INT "1", INT "2", RPAREN]
                 , tokTest "(< 2 3)" "(< 2 3)" [LPAREN, PRIMITIVE "<", INT "2", INT "3", RPAREN]
                 , tokTest "(car (1 2 3 4))" "(car (1 2 3 4))" [LPAREN, PRIMITIVE "car", LPAREN, INT "1", INT "2", INT "3", INT "4", RPAREN, RPAREN]
                 , tokTest "(define variable 34)" "(define variable 34)" [LPAREN, DEFINE, IDEN "variable", INT "34", RPAREN]
                 , tokTest "(define (function x y) (subfunc x y))" "(define (function x y) (subfunc x y))" [LPAREN, DEFINE, LPAREN, IDEN "function", IDEN "x", IDEN "y", RPAREN, LPAREN, IDEN "subfunc", IDEN "x", IDEN "y", RPAREN, RPAREN]
                 , tokTest "'(function x y)" "'(function x y)" [QUOTE, LPAREN, IDEN "function", IDEN "x", IDEN "y", RPAREN]
                 , tokTest "(lambda (x) (+ x 2))" "(lambda (x) (+ x 2))" [LPAREN, LAMBDA, LPAREN, IDEN "x", RPAREN, LPAREN, PRIMITIVE "+", IDEN "x", INT "2", RPAREN, RPAREN]
                 , tokTest "(lambda (x y) (+ x y))" "(lambda (x y) (+ x y))" [LPAREN, LAMBDA, LPAREN, IDEN "x", IDEN "y", RPAREN, LPAREN, PRIMITIVE "+", IDEN "x", IDEN "y", RPAREN, RPAREN]
                 , tokTest "(if (func1 a) (func2 a) (- a b))" "(if (func1 a) (func2 a) (- a b))" [LPAREN, IF, LPAREN, IDEN "func1", IDEN "a", RPAREN, LPAREN, IDEN "func2", IDEN "a", RPAREN, LPAREN, PRIMITIVE "-", IDEN "a", IDEN "b", RPAREN, RPAREN]]

main :: IO Counts
main = do runTestTT tests
