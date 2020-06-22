module Solver where

import           Board
import qualified Data.Set                      as Set
import qualified Data.Sequence                 as Seq
import           Data.Ord                       ( comparing )
import           Data.Maybe
import           Data.List                      ( minimumBy )
import           Debug.Trace                    ( traceShowId )

solve :: Board -> Maybe Board
solve board =
    let boards = nextStepBoards board
        sols   = backtrackSolve boards
    in  case sols of
            []        -> Nothing
            (sol : _) -> Just sol

backtrackSolve :: [Board] -> [Board]
backtrackSolve []     = []
backtrackSolve boards = do
    board <- boards
    if isSolved board
        then return board
        else do
            board' <- backtrackSolve (nextStepBoards board)
            return board'

boardRange :: [Int]
boardRange = [0 .. boardDimension - 1]

nextStepBoards :: Board -> [Board]
nextStepBoards board =
    let (loc, vals) = findUnsolved board in [ set board loc v | v <- vals ]

-- optimized by fewest current possibilities
findUnsolved :: Board -> (Loc, [Int])
findUnsolved board =
    let
        locs         = [ (r, c) | r <- boardRange, c <- boardRange ]
        unsolvedLocs = filter (\loc -> isNothing (get board loc)) locs
        bestLoc      = minimumBy (comparing (Set.size . (getValidValues board)))
                                 unsolvedLocs
    in
        (bestLoc, Set.toList $ getValidValues board bestLoc)


isSolved :: Board -> Bool
isSolved = all (all (isJust))

getValidValues :: Board -> Loc -> Set.Set Int
getValidValues board loc
    | isJust $ get board loc = Set.singleton $ fromJust $ get board loc
    | otherwise = foldl
        (Set.difference) -- start with full set and chip away as values are ruled out
        (Set.fromList [1 .. boardDimension]) -- all numbers are possible at first
        [ getRowValues board loc
        , getColValues board loc
        , getSquareValues board loc
        ]
