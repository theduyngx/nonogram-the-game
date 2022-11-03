{-
   Module  : AlgoSolve
             Algorithm is based on Ted Yin's deductive solver algorithm for solving
             nonograms.
   Purpose : Solving the nonogram efficiently. This is used to either verify that
             the puzzle is indeed solvable, or to show the solution itself user
             asks for it, or both.

The algorithm first generates all possible patterns given the row and column 'hints',
which is to be seen as constraints. Then given the constraints, it will eliminate
based on column constraint, to finally check which one of the rest is the solution.
The algorithm concerns a great deal with deduction, and hence process of elimination
and constraint binding are optimized.
-}

module AlgoSolve (nonogram, Hint, Hints, Row, Puzzle) where

import Data.List (intersperse)

-- * types
type Hint   = [Int]
type Hints  = [Hint]
type Row    = [Char]
type Puzzle = [Row]

-- generate all possible patterns given row/col constraints
gen :: Int -> Hint -> Hints
gen n []
    | n >= -1 = return []
    | otherwise = []
gen n (x:xs) = do p <- [0..n-x]
                  s <- gen (n - (p + x + 1)) xs
                  return $ [p..p+x-1] ++ map (p + x + 1 +) s

output :: [Hints] -> Puzzle
output sols = [do b <- sol
                  '|' : intersperse '|' (map (\x -> if x `elem` b then 'X' else '_') [0..7])
                   ++ "|\n" | sol <- sols]

-- eliminate the possible patterns, given the information in a column
elim :: (Eq a) => [a] -> [a] -> [[a]] -> [[a]]
elim x w ps = filter (\p -> all (`elem` p) x && all (`notElem` p) w) ps

nonogram :: Hints -> Hints -> Puzzle
nonogram row col = output $ find 0 (map (gen 8) row) (map (gen 9) col)
                                   (replicate 8 []) (replicate 8 [])
  where find _ [] colps _ _
          | [] `elem` colps = []
          | otherwise = return []
        find i (rs:rowps) colps cb cw
          | [] `elem` colps = []
          | otherwise =
              do r <- rs
                 let cb' = [b' | (c, b) <- zip [0..7] cb,
                                 let b' = if c `elem` r then i:b else b]
                 let cw' = [w' | (c, w) <- zip [0..7] cw,
                                 let w' = if c `notElem` r then i:w else w]
                 sol <- find (i + 1) rowps
                             (map (uncurry . uncurry $ elim) (zip (zip cb' cw') colps))
                             cb' cw'
                 return $ r : sol