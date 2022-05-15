module Calculator.Tree where
    import Calculator.Token (Token)

    data Node a = Plain (Token a) | Tree [Node a]
