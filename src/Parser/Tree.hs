module Parser.Tree where
    import Parser.Token

    data Node = TokenNode Token | TreeNode [Node] deriving(Show)
