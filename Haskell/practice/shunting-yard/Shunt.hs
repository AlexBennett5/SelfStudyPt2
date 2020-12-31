module Shunt where

-- QUEUE --

data Queue a = Queue [a] [a]

newQueue :: Queue a
newQueue = Queue [] []

emptyQueue :: Queue a -> Bool
emptyQueue (Queue [] []) = True
emptyQueue _             = False

enq :: Queue a -> a -> Queue a
enq (Queue xs ys) y = Queue xs (y:ys)

deq :: Queue a -> (a, Queue a)
deq (Queue [] []) = error "Can't deq from an empty queue"
deq (Queue (x:xs) ys) = (x, Queue xs ys)
deq (Queue [] ys) = deq (Queue (reverse ys) [])

assembleQueue :: Queue a -> String -> Queue a
assembleQueue q str = case str of
                      []   -> q
                      x:xs -> assembleQueue (enq q x) xs 

-- STACK --

type Stack a = [a] deriving (Show)

emptyStack :: Stack a
emptyStack [] = True
emptyStack _  = False

pop :: Stack a -> (a, Stack a)
pop [] = error "Can't pop an empty stack"
pop (x:xs) = (x, Stack xs)

push :: Stack a -> Stack a
push x (Stack xs) = Stack x:xs

peek :: Stack a -> a
peek x::xs = x

-- SHUNTING YARD --

data Operator = Operator { symb :: Char
                         , prec :: Int 
                         , left :: Bool}
                         deriving (Show)

defaultOperators :: [Operator]
defaultOperators = [ Operator {symb = "+", prec = 2, left = True}
                   , Operator {symb = "-", prec = 2, left = True}
                   , Operator {symb = "/", prec = 3, left = True}
                   , Operator {symb = "*", prec = 3, left = True}
                   , Operator {symb = "^", prec = 4, left = False} ]

operatorSymbs :: [Char]
operatorSymbs = fmap symb defaultOperators

getPrecFromOp :: Char -> Int
getPrecFromOp op = prec head [e | e <- defaultOperators, symb e == op]

operatorStackHandler :: Queue Char -> Stack Char -> (Queue Char, Stack Char)
operatorStackHandler o s = if testOp
                           then 

loop :: Queue Char -> Queue Char -> Stack Char -> IO ()
loop q o s = do {
  (tok, newQueue) <- deq q
  let nextstep
    | isDigit tok              -> enq o tok
    | tok `elem` operatorSymbs ->  
}

dowhile :: Bool -> Queue Char -> Queue Char -> Stack Char -> IO ()
dowhile cond q o s = when cond (loop q s)

main = do {
  putStrLn "Enter the formula: "
  inputStr <- getLine
  q <- assembleQueue newQueue inputStr
  s <- Stack []
  o <- newQueue
  dowhile (emptyQueue q) q o s
}
