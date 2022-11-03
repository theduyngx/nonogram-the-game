{-
   Module  : UserSolve
   Author  : The Duy Nguyen <theduyn@student.unimelb.edu.au>
   Purpose : Deals with user's interaction with the program, from starting the game
             to playing, finishing and quitting the game.
-}

module UserSolve (userSolve, printP, help) where

import Data.List (transpose)
import AlgoSolve

-- * User input functions
-- | recursive function for user solving the nonogram until it is correct
userSolve :: Puzzle -> Hints -> Hints -> IO String
userSolve puzzle rowHint colHint =
    if   solved puzzle rowHint colHint
    then do
         printP puzzle rowHint colHint
         putStrLn "Congratulations, you've completed the puzzle! Start again?"
         return "cmd"
    else do
         input <- getLine
         let elseCase = putStrLn "Unknown cmd, help?"
                        >> userSolve puzzle rowHint colHint
         case input of
              "quit"   -> putStrLn "quitting game..." >> return ""
              "help"   -> help >> userSolve puzzle rowHint colHint
              "home"   -> return "m"
              "print"  -> printP puzzle rowHint colHint
                          >> userSolve puzzle rowHint colHint
              "reveal" -> return "sol"
              str      ->
                  case length $ words str of
                       3 -> let command
                                 | h == 'a' = do
                                       puzzle' <- addX r c puzzle
                                       userSolve puzzle' rowHint colHint
                                 | h == 'd' = do
                                       puzzle' <- delX r c puzzle
                                       userSolve puzzle' rowHint colHint
                                 | otherwise = elseCase
                                 where [h:hs,rs,cs] = words str
                                       r = read rs :: Int
                                       c = read cs :: Int
                            in command
                       _ -> elseCase

-- * mark or unmarked functions
-- | function marking (or adding) X to position
addX :: Int -> Int -> Puzzle -> IO Puzzle
addX  r c pz@(x:xs)
    | r < 1 || c < 1 || r > length pz || c > length x = do
          putStrLn "\t** Position not within range of the board!"
          return pz
    | otherwise = if (pz !! (r-1)) !! (c-1) == 'X'
                  then do
                       putStrLn "\t** Position is already marked X!"
                       return pz
                  else do
                       putStrLn $ "Marked (" ++ show r ++ ", " ++ show c ++ ")." 
                       return $ findCol r c pz 1 addRow

-- | function to mark X to a specific row
--   called as an argument of findCol for the function addX
addRow :: Int -> Row -> Int -> Row
addRow _ [] _ = []
addRow c (row:rows) ci
    | c == ci   = 'X' : rows
    | c >  ci   = row : addRow c rows (ci+1)
    | otherwise = error "row index somehow exceeded number of rows"

-- | function to delete a marked X on the board
delX :: Int -> Int -> Puzzle -> IO Puzzle
delX  r c pz@(x:xs)
    | r < 1 || c < 1 || r > length pz || c > length x = do
          putStrLn "\t** Position not within range of the board!"
          return pz
    | otherwise = if (pz !! (r-1)) !! (c-1) == '_'
                  then do
                       putStrLn "\t** Position is already empty!"
                       return pz
                  else do
                       putStrLn $ "Unmarked (" ++ show r ++ ", " ++ show c ++ ")." 
                       return $ findCol r c pz 1 delRow

-- | function to unmark X in a specific row;
--   similar function to addRow
delRow :: Int -> Row -> Int -> Row
delRow _ [] _ = []
delRow c (row:rows) ci
    | c == ci   = '_' : rows
    | c >  ci   = row : delRow c rows (ci+1)
    | otherwise = error "row index somehow exceeded number of rows"

-- | function to find the column that needs modification;
--   used in both addX and delX functions
findCol :: Int -> Int -> Puzzle -> Int -> (Int -> Row -> Int -> Row) -> Puzzle
findCol r c (x:xs) ri f
    | r == ri   = f c x 1 : xs
    | r >  ri   = x : findCol r c xs (ri+1) f
    | otherwise = error "column index somehow exceeded number of columns"

-- * printing functions
-- | printing the entire board (with row and column hints)
printP :: Puzzle -> Hints -> Hints -> IO ()
printP [] rhs chs =
    case rhs of
         [] -> printColHint chs
         _  -> error "row hint not empty after complete traversal in printP"
printP (r:rs) (rh:rhs) chs = do
    let rowP = concatMap (('|' :) . tail . init . show) r
    putStr $ rowP ++ "|\t"
    printRowHint rh
    printP rs rhs chs

-- | helper function printing the row hints
printRowHint :: Hint -> IO ()
printRowHint rhs = do
    let line = concatMap (flip (++) "," . show) rhs
    putStrLn $ init line

-- | helper function printing the column hints
printColHint :: Hints -> IO ()
printColHint [] = return ()
printColHint chs = do
    let line = concatMap ((flip (++) "," . show) . head) chs
    putStrLn $ init (' ' : line)
    printColHint (filter (/= []) . map tail $ chs)

-- | function initiated when user inputs the 'help' command in-game
help :: IO ()
help = putStr $ unlines [
       "In-game help:",
       "    print           prints the board",
       "    add row col     adds X to position (row, col)",
       "    del row col     delete X at (row, col)",
       "    reveal          reveal the solution",
       "    home            go to home screen",
       "    quit            quit game",
       "    help            display this help message"]

-- * check solution functions
-- | checking whether the nonogram has been solved or not
solved :: Puzzle -> Hints -> Hints -> Bool
solved pz rowHint colHint = rowSolved pz rowHint && rowSolved (transpose pz) colHint

rowSolved :: Puzzle -> Hints -> Bool
rowSolved [] _ = True
rowSolved (r:rs) (h:hs) =
    length (filter (== 'X') r) == sum h && (validRow r' h && rowSolved rs hs)
    where r' = reverse . dropWhile (== '_') . reverse . dropWhile (== '_') $ r
          validRow x (y:ys) =
            let (fr, en) = splitAt y x in
            case fr of
              [] -> False
              _  ->
                let allX = all (== 'X') fr in
                case en of
                  [] -> allX
                  _  -> allX && head en == '_' && validRow (dropWhile (== '_') en) ys