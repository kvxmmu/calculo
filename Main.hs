module Main where
    import Calculator.Tokenizer

    main :: IO ()
    main = do
        print $ tokenize "if 1 = 2 then 4 else 0"