{-
   Module  : NumAbbreviation
   Purpose : Only for game displaying purposes where making more-than-one
             digit numbers a single-character alphabetical letter makes
             the game look more aesthetically pleasing. The abbreviations
             start from 10 and end at 35. Any number larger than 35 will
             appear as it is.
-}

module NumAbbreviation (showNum, printDict) where

-- | showing the number as alphabetical letter
showNum :: Int -> String
showNum n
    | n < 10    = show n
    | otherwise =
        let  alphabet = ['A'..'Z'] in
        if   n-10 < length alphabet then [alphabet !! (n-10)]
        else show n

-- | printing the abbreviation dictionary in-game
printDict :: IO ()
printDict = do
    putStrLn "Abbreviation dictionary:"
    printDictAux 0 ['A'..'Z']

printDictAux :: Int -> [Char] -> IO ()
printDictAux n alphabet
    | n < length alphabet =
      let str =  if (n+1) `mod` 4 == 0 || n == length alphabet - 1
                 then "\n" else "\t"
      in  do
          putStr $ "\t" ++ show (n+10) ++ " - " ++ [alphabet !! n] ++ str
          printDictAux (n+1) alphabet
    | otherwise = return ()