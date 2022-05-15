module Calculator.Token where
    data Priority = DoItNow | High
                            | Medium
                            | Low
                            | VeryLow

    data Operator = Add | Sub
                        | Mul
                        | Div
                        | IfClause
                        | ElseClause
                        | Assign
                        | Gt
                        | Lt
                        | GtOrEq
                        | LtOrEq
                        | Let
                        | Pow deriving(Show, Eq)

    data Token a = Name [Char] | Operator { op :: Operator }
                               | Number a
                               | OpenBracket
                               | CloseBracket
                               | OpenBrace
                               | CloseBrace deriving(Show, Eq)

    priorityOfInt :: Operator -> Int
    priorityOfInt op =  priorityToInt $ priorityOf op

    priorityOf :: Operator -> Priority
    priorityOf Add = Low
    priorityOf Sub = Low
    priorityOf Mul = Medium
    priorityOf Div = Medium
    priorityOf IfClause = High
    priorityOf ElseClause = High
    priorityOf Lt = VeryLow
    priorityOf Gt = VeryLow
    priorityOf LtOrEq = VeryLow
    priorityOf GtOrEq = VeryLow
    priorityOf Let = DoItNow
    priorityOf Assign = VeryLow
    priorityOf Pow = High

    priorityToInt :: Priority -> Int
    priorityToInt DoItNow = 999
    priorityToInt High = 3
    priorityToInt Medium = 2
    priorityToInt Low = 1
    priorityToInt VeryLow = 0
