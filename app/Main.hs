module Main where

import System.IO
import System.Exit

import Parser.Tokenizer

calculoLoop = do
    putStr "expression> "
    hFlush stdout
    expressionOrCommand <- getLine    
    if startsWith expressionOrCommand '.' then
        case tail expressionOrCommand of
            "help" -> putStrLn (concat [
                "Enter expression and it will be calculated\n",
                "Commands:\n",
                "\t.help - show this message\n",
                "\t.quit - quit from app"
                ])

            "quit" -> exitWith ExitSuccess
            otherwise -> putStrLn "Unknown command, pass .help to get list"
    else
        putStrLn (show (parseInput expressionOrCommand))

    calculoLoop

main :: IO ()
main = calculoLoop
