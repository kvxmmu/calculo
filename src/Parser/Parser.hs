module Parser.Parser where
    import Parser.Token
    import Parser.Tree
    import Parser.Tokenizer

    parseInner :: [Token] -> ([Node], [Token])
    parseInner tokens
        | null tokens = ([], [])
        | isBracket token =
            if getBracket token then
                let (parsed, remaining) = parseInner tailing
                in let (outParsed, outRemaining) = parseInner remaining
                in ([TreeNode (parsed ++ outParsed)], outRemaining)
            else
                ([], tailing)
        | otherwise =
            let (parsed, remaining) = parseInner tailing
            in (TokenNode token : parsed, remaining)
        where
            token = head tokens
            tailing = tail tokens

    parseText :: String -> [Node]
    parseText text = fst (parseInner (tokenize text))

