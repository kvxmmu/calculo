module Calculator.Tree where
    import Calculator.Token (Token)

    data Node a = Plain (Token a) | Brackets [Node a]
                                  | Braces [Node a]
                                  | If { cond  :: [Node a]
                                       , true  :: [Node a]
                                       , false :: [Node a] }
                                  | Let { name :: Token a
                                        , expr :: [Node a] }
