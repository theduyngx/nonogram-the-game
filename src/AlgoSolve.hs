{-
   Module  : AlgoSolve
             Based on Deducing solver algorithm.
   Purpose : Solving the nonogram efficiently. This is used to either verify that
             the puzzle is indeed solvable, or to show the solution itself user
             asks for it, or both.
-}

module AlgoSolve (nonogram, Hint, Hints, Row, Puzzle) where

import Control.Monad (zipWithM)
import Data.List     (transpose, group)
import Data.Maybe    (maybeToList)
import NumAbbreviation

-- * types
type Hint   = [Int]
type Hints  = [Hint]
type Row    = [Char]
type Puzzle = [Row]

type RowG s = [s]
type Grid s = [RowG s]

-- partial information about a square
type Square = Maybe Bool

-- All solutions to the nonogram
solve :: Hints -> Hints -> [Grid Bool]
solve rs cs = [grid' | grid <- maybeToList (deduction rs cs),
                       grid' <- zipWithM (rowsMatch nc) rs grid,
                       map contract (transpose grid') == cs]
    where nc       = length cs
          contract = map length . filter head . group

-- A nonogram with all the values we can deduce
deduction :: Hints -> Hints -> Maybe (Grid Square)
deduction rs cs = converge step initial
    where nr = length rs
          nc = length cs
          initial = replicate nr (replicate nc Nothing)
          step    = (improve nc rs . transpose) <.> (improve nr cs . transpose)
          improve n   = zipWithM (common n)
          (g <.> f) x = f x >>= g

-- repeatedly apply f until a fixed point is reached
converge :: (Monad m, Eq a) => (a -> m a) -> a -> m a
converge f s = do
        s' <- f s
        if s' == s then return s else converge f s'

-- common n ks partial = commonality between all possible ways of
-- placing blocks of length ks in a row of length n that match partial.
common :: Int -> Hint -> RowG Square -> Maybe (RowG Square)
common n ks partial = case rowsMatch n ks partial of
        [] -> Nothing
        rs -> Just (foldr1 (zipWith unify) (map (map Just) rs))
  where unify :: Square -> Square -> Square
        unify x y
            | x == y = x
            | otherwise = Nothing

-- rowsMatch n ks partial = all possible ways of placing blocks of
-- length ks in a row of length n that match partial.
rowsMatch :: Int -> Hint -> RowG Square -> [RowG Bool]
rowsMatch _ [] [] = [[]]
rowsMatch _ _  [] = []
rowsMatch n ks (Nothing:partial) =
        rowsMatchAux n ks True partial ++
        rowsMatchAux n ks False partial
rowsMatch n ks (Just s:partial) = 
        rowsMatchAux n ks s partial

rowsMatchAux :: Int -> Hint -> Bool -> RowG Square -> [RowG Bool]
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

-- | displaying the grids, used when printing out the solution to the puzzle
showGrid :: Hints -> Hints -> Grid Bool -> String
showGrid rhs chs shs = unlines (zipWith showRow rhs shs ++ showCols chs)
    where showRow rs ss = concat [['|', cellChar s] | s <- ss] ++ "| " ++
                          unwords (map showNum rs)
          showCols cs
              | all null cs = []
              | otherwise =
                    concatMap showCol cs :
                    showCols (map (\l -> if null l then [] else tail l) cs)
                    where showCol (k:_) = ' ' : showNum k
                          showCol  _    = "  "
          cellChar True  = 'X'
          cellChar False = '_'

-- Print the first solution (if any) to the nonogram
nonogram :: Hints -> Hints -> String
nonogram rs cs =
    case solve rs cs of
         []       -> "nil"
         (grid:_) -> showGrid rs cs grid