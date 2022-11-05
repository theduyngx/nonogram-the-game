{-
   Module  : AlgoSolve
             Based on Deducing solver algorithm.
   Purpose : Solving the nonogram efficiently. This is used to either verify that
             the puzzle is indeed solvable, or to show the solution itself user
             asks for it, or both.
-}

module AlgoSolve (nonogram, Hint, Hints, Row, Grid, Puzzle) where

import Control.Monad (zipWithM)
import Data.List     (transpose, group)
import Data.Maybe    (maybeToList)
import NumAbbreviation

-- * types
-- ** Hints
type Hint   = [Int]
type Hints  = [Hint]

-- ** Grid
type Row  s = [s]
type Grid s = [Row s]
type Puzzle = Grid Char
-- ** partial information about a square
type Square = Maybe Bool

-- * functions
-- | All solutions to the nonogram
solve :: Hints -> Hints -> [Grid Bool]
solve rs cs =
    [grid' | grid  <- maybeToList (deduction rs cs),
             grid' <- zipWithM (rowsMatch $ length cs) rs grid,
             map (map length . filter head . group) (transpose grid') == cs]

-- | A nonogram with all the values we can deduce
deduction :: Hints -> Hints -> Maybe (Grid Square)
deduction rs cs = converge step initial
    where nr = length rs
          nc = length cs
          improve = zipWithM . common
          initial = replicate nr (replicate nc Nothing)
          step    = (improve nc rs . transpose) <.> (improve nr cs . transpose)
          (g <.> f) x = f x >>= g

-- | repeatedly apply f until a fixed point is reached
converge :: (Monad m, Eq a) => (a -> m a) -> a -> m a
converge f s = do
    s' <- f s
    if s' == s then return s else converge f s'

-- | common n ks partial = commonality between all possible ways of
--   placing blocks of length ks in a row of length n that match partial.
common :: Int -> Hint -> Row Square -> Maybe (Row Square)
common n ks partial =
    case rowsMatch n ks partial of
         [] -> Nothing
         rs -> Just . foldr1 (zipWith unify) . map (map Just) $ rs

unify :: Square -> Square -> Square
unify x y = if x == y then x else Nothing

-- | rowsMatch n ks partial = all possible ways of placing blocks of
--   length ks in a row of length n that match partial.
rowsMatch :: Int -> Hint -> Row Square -> [Row Bool]
rowsMatch _ [] [] = [[]]
rowsMatch _ _  [] = []
rowsMatch n ks (Just s  : partial) = rowsMatchAux n ks s partial
rowsMatch n ks (Nothing : partial) =
    rowsMatchAux n ks True partial ++ rowsMatchAux n ks False partial

-- | helper function for rowsMatch
rowsMatchAux :: Int -> Hint -> Bool -> Row Square -> [Row Bool]
rowsMatchAux n ks False partial =
    [False : row | row <- rowsMatch (n-1) ks partial]
rowsMatchAux n [k] True partial =
    [replicate k True ++ replicate (n-k) False |
     n >= k && notElem (Just False) front && notElem (Just True) back]
         where (front, back) = splitAt (k-1) partial
rowsMatchAux n (k:ks) True partial =
    [replicate k True ++ False : row |
     n > k+1 && notElem (Just False) front && blank /= Just True,
     row <- rowsMatch (n-k-1) ks p']
         where (front, blank:p') = splitAt (k-1) partial
rowsMatchAux _ _ _ _ = error "rowsMatchAux: incorrect pattern matched"

-- | function printing puzzle's grid
showGrid :: Hints -> Hints -> Grid Bool -> String
showGrid  rhs  chs  shs = unlines (zipWith showRow rhs shs ++ showCols chs)
    where showRow rs ss = concat
              [['|', (\x -> if x then 'X' else '_') s] | s <- ss] ++ "| " ++
               unwords (map showNum rs)
          showCols cs
              | all null cs = []
              | otherwise   = let tailEmp l = if null l then [] else tail l
                              in  concatMap showCol cs : showCols (map tailEmp cs)
                              where showCol (k:_) = ' ' : showNum k
                                    showCol  _    = "  "

-- | Print the first solution (if any) to the nonogram
nonogram :: Hints -> Hints -> String
nonogram rs cs =
    case solve rs cs of
         []       -> "nil"
         (grid:_) -> showGrid rs cs grid