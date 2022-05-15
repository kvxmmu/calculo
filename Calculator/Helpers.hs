module Calculator.Helpers where
    unreachable :: a
    unreachable = error "This branch is unreachable"

    matchSequence :: (Eq a) => [a] -> [a] -> Bool
    matchSequence seq1 seq2
        | null seq1 = True
        | null seq2 = False
        | head seq1 == head seq2 = matchSequence (tail seq1) (tail seq2)
        | otherwise = False

    headEqual :: (Eq a) => a -> [a] -> Bool
    headEqual item seq = head seq == item
