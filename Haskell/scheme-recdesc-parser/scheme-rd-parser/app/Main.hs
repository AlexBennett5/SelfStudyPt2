module Main where

import Tokenizer
import Control.Monad
import System.Console.Haskeline

main :: IO ()
main = forever $ do
  putStr "Scheme> "
  line <- getLine
  print $ tokenize line
