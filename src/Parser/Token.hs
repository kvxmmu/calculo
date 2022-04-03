module Parser.Token where
    type Priority = Int;

    data TokenType    = Id | Bracket Bool
                           | Number
                           | Operator deriving(Show)

    data OperatorType = Add | Sub
                            | Mul
                            | Div deriving(Show)

    data Token = Token{ tokenType :: TokenType
                      , tokenData :: String
                      , operatorType :: Maybe OperatorType } deriving(Show)
    
    number :: String -> Token
    number n = Token{ tokenType = Number
                    , tokenData = n
                    , operatorType = Nothing  }
    
    id :: String -> Token
    id n = Token{ tokenType = Id
                , tokenData = n
                , operatorType = Nothing  }
    
    bracket :: String -> Token
    bracket n = Token{ tokenType = Bracket (n == "(")
                     , tokenData = n
                     , operatorType = Nothing }
    
    operator :: String -> Token
    operator n =
        let operatorEnum = case n of "+" -> Add
                                     "-" -> Sub
                                     "*" -> Mul
                                     "/" -> Div
                                     _ -> error "Unknown operator"
        in
            Token{ tokenData = ""
                 , tokenType = Operator
                 , operatorType = Just operatorEnum }

    getBracket :: Token -> Bool
    getBracket (Token (Bracket side) _ _ ) = side
    getBracket _ = error "Token is not bracket"

    isTokenOperator :: TokenType -> Bool
    isTokenOperator Operator = True
    isTokenOperator _ = False

    isBracket :: Token -> Bool
    isBracket (Token (Bracket _) _ _ ) = True
    isBracket _ = False

    isNotOperator :: TokenType -> Bool
    isNotOperator tk = not (isTokenOperator tk)

    priorityOf :: Token -> Priority
    priorityOf Token{tokenType = Operator, tokenData = tokenData} =
        case tokenData of
            "*" -> 1
            "/" -> 1
            _ -> 0

    priorityOf _ = error "Cannot determine priority on a non-operator token"
        
