module Calculator.Parser where
    import Calculator.Token
    import Calculator.Tree
    import Calculator.Helpers

    parseInternal :: (Read a, Num a) => [Token a] -> [Node a]
    parseInternal seq = []
