module Parser.Tokenizer where
    import Parser.Token

    startsWith :: (Eq a) => [a] -> a -> Bool
    startsWith (x:_) val = x == val
    startsWith [] val = False

    isDigit :: Char -> Bool
    isDigit chr = chr `elem` ['0'..'9']

    isId :: Char -> Bool
    isId chr = chr `elem` ['A'..'Z'] ++ ['a'..'z']

    isWhitespace :: Char -> Bool
    isWhitespace chr = chr `elem` " \t\n"

    isBrackets :: Char -> Bool
    isBrackets chr = chr `elem` "()"

    isOperator :: Char -> Bool
    isOperator chr = chr `elem` "+-*/"

    parseNumber :: String -> (String, String)
    parseNumber seq
        | null seq = ("", [])
        | isDigit heading =
            let (numberTailing, seqTail) = parseNumber tailing
            in (heading : numberTailing, seqTail)
        | otherwise   = ("", seq)
        where
            heading = head seq
            tailing = tail seq

    parseInput :: String -> [Token]
    parseInput "" = []
    parseInput seq
        | null seq = []
        | isDigit heading =
            let (numberSeq, seqTail) = parseNumber seq
            in number numberSeq : parseInput seqTail
        | isWhitespace heading = parseInput tailing
        | isOperator heading = operator [heading] : parseInput tailing
        | isBrackets heading = bracket [heading] : parseInput tailing
        | otherwise = []

        where
            heading = head seq
            tailing = tail seq
