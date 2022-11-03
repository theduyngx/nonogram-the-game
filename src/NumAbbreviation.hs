module NumAbbreviation (showNum) where

showNum :: Int -> String
showNum n
    | n < 10    = show n
    | otherwise =
        let  alphabet = ['A'..'Z'] in
        if   n-10 < length alphabet then [alphabet !! (n-10)]
        else show n