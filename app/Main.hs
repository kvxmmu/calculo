module Main where

import Parser.Tokenizer

main :: IO ()
main =
    putStrLn (show (parseInput "2 + 2"))
