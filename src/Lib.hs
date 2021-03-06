module Lib
    ( someFunc
    )
where

import           Board
import           Solver
import           System.IO
import           Data.Maybe                     ( fromMaybe )


someFunc :: IO ()
someFunc = do
    putStr "Enter the number of the puzzle you want to load: "
    hFlush stdout -- ensures prompt is printed, even without \n character
    puzzleNum <- getLine
    boardStr  <- readFile $ "src/puzzles/" ++ puzzleNum ++ ""
    putStrLn "Original: "
    putStrLn $ prettyPrint $ fromStr boardStr
    putStrLn "\nSolved: "
    putStrLn $ prettyPrint $ fromMaybe (fromStr boardStr)
                                       (solve (fromStr boardStr))
