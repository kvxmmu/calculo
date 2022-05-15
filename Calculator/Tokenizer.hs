{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Calculator.Tokenizer (tokenize) where
    import Calculator.Token
    import Calculator.Helpers

    import Data.Maybe (fromJust)

    isSkippable :: Char -> Bool
    isSkippable c = c `elem` " \n\t"

    matchOperator seq
        | null seq = Nothing
        | matchSequence seq "**" = Just (Pow, doubleTail)
        | matchSequence seq ">=" = Just (GtOrEq, doubleTail)
        | matchSequence seq "<=" = Just (LtOrEq, doubleTail)
        | heading == '+' = Just (Add, tailing)
        | heading == '-' = Just (Sub, tailing)
        | heading == '*' = Just (Mul, tailing)
        | heading == '/' = Just (Div, tailing)
        | heading == '>' = Just (Gt, tailing)
        | heading == '<' = Just (Lt, tailing)
        | heading == '=' = Just (Assign, tailing)
        | otherwise = Nothing
        where heading = head seq
              doubleTail = tail $ tail seq
              tripleTail = tail $ tail $ tail seq
              tailing = tail seq
    
    matchNumber number state pointed
        | null number = ending
        | isFloatingPointSign heading = matchNumber tailing extended True
        | isDigit heading =
            matchNumber tailing extended pointed
        | otherwise = ending
        where heading = head number
              tailing = tail number
              extended = state ++ [heading]
              ending = if null state then
                           Nothing
                       else
                           Just (Number $ read state, number)

    isName chr =
        let allowed = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_"
        in chr `elem` allowed

    matchBrackets seq =
        case head seq of
            '(' -> OpenBracket
            ')' -> CloseBracket
            '{' -> OpenBrace
            '}' -> CloseBrace
            _ -> unreachable
    isBrackets seq =
        head seq `elem` "(){}"

    isDigit item = item `elem` ['0'..'9']
    isFloatingPointSign item = item == '.'

    matchName seq state
        | null seq = ending
        | isName heading =
            matchName tailing (state ++ [heading])
        | otherwise = ending
        where
            ending =
                (Name state, seq)
            heading = head seq
            tailing = tail seq

    tokenize :: (Num a, Read a) => [Char] -> Maybe [Token a]
    tokenize seq =
        let maybeTokens = tokenizeInternal seq []
        in
        case maybeTokens of
            Just tokens -> Just $ postProcessTokens tokens
            Nothing -> Nothing

    postProcessTokens :: (Num a, Read a) => [Token a] -> [Token a]
    postProcessTokens seq
        | null seq = []
        | otherwise =
            case heading of
                Name "if" -> Operator IfClause : postProcessTokens tailing
                Name "else" -> Operator ElseClause : postProcessTokens tailing
                Name "let" -> Operator Let : postProcessTokens tailing
                _ -> seq
        where
            heading = head seq
            tailing = tail seq

    tokenizeInternal :: (Num a, Read a) => [Char] -> [Token a] -> Maybe [Token a]
    tokenizeInternal string tokens
        | null string = Just tokens
        | isSkippable heading =
            tokenizeInternal tailing tokens
        | isDigit heading =
            let Just (token, nextTail) = matchNumber string "" False
            in
            tokenizeInternal nextTail (tokens ++ [token])
        | isBrackets string =
            tokenizeInternal tailing (tokens ++ [matchBrackets string])
        | isName heading =
            let (token, nextTail) = matchName string ""
            in
            tokenizeInternal nextTail (tokens ++ [token])
        | otherwise =
            let operator = matchOperator string in
            case operator of
                Just (op, nextTail) ->
                    tokenizeInternal nextTail (tokens ++ [Operator { op = op }])
                Nothing ->
                    Nothing
                    
        where
            heading = head string
            tailing = tail string