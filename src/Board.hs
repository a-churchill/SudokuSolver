module Board
  ( Loc
  , Row
  , Board
  , boardDimension
  , get
  , getRowValues
  , getColValues
  , getSquareValues
  , set
  , fromStr
  , prettyPrint
  )
where

import qualified Data.Sequence                 as Seq
import qualified Data.Set                      as Set
import           Data.Char
import qualified Data.Foldable                 as Foldable
import           Data.Maybe                     ( catMaybes )

boardDimension :: Int
boardDimension = 9

boardSquareDimension :: Int
boardSquareDimension = 3

type Loc = (Int, Int)  -- row, column
type Row = Seq.Seq (Maybe Int)
type Board = Seq.Seq (Row)  -- stored in row-major form

{- ACCESSING -}

inBounds :: Int -> Bool
inBounds x = x >= 0 && x < boardDimension

get :: Board -> Loc -> Maybe Int
get board (r, c) | not $ inBounds r = Nothing
                 | not $ inBounds c = Nothing
                 | otherwise        = Seq.index (Seq.index board r) c



getRowValues :: Board -> Loc -> Set.Set Int
getRowValues board (r, _) =
  Set.fromList $ catMaybes $ Foldable.toList $ Seq.index board r

getColValues :: Board -> Loc -> Set.Set Int
getColValues board (_, c) = Set.fromList $ catMaybes $ Foldable.toList $ fmap
  (\row -> Seq.index row c)
  board

floorSquareLoc :: Int -> Int
floorSquareLoc x = x - (x `mod` boardSquareDimension)

getSquareValues :: Board -> Loc -> Set.Set Int
getSquareValues board (r, c) =
  let lowR   = floorSquareLoc r
      lowC   = floorSquareLoc c
      rRange = [lowR .. (lowR + boardSquareDimension - 1)]
      cRange = [lowC .. (lowC + boardSquareDimension - 1)]
      locs   = [ (r, c) | r <- rRange, c <- cRange ]
  in  Set.fromList $ catMaybes $ map (get board) locs

{- CREATING/PARSING -}

parseChar :: Char -> Maybe Int
parseChar c | c == '.'  = Nothing
            | isDigit c = Just (digitToInt c)
            | otherwise = Nothing

makeSeq :: String -> Seq.Seq (Seq.Seq (Maybe Int))
makeSeq = Seq.singleton . Seq.fromList . map parseChar

fromStr :: String -> Board
fromStr [] = Seq.empty
fromStr boardStr
  | length boardStr <= boardDimension
  = makeSeq boardStr
  | otherwise
  = let (first, rest) = splitAt boardDimension boardStr
    in  (makeSeq first) Seq.>< (fromStr rest)

{- MODIFYING -}

set :: Board -> Loc -> Int -> Board
set board (r, c) val =
  let newRow = Seq.update c (Just val) (Seq.index board r)
  in  Seq.update r newRow board

{- PRINTING -}

hSeparator :: [Char]
hSeparator = " " ++ replicate (1 + (boardDimension * 4)) '-' ++ "\n"

vSeparator :: [Char]
vSeparator = " | "

prettyPrintChar :: Maybe Int -> String
prettyPrintChar Nothing  = " "
prettyPrintChar (Just c) = show c

prettyPrintLine :: Row -> String
prettyPrintLine row =
  vSeparator ++ foldr (addSeparator vSeparator prettyPrintChar) "" row ++ "\n"

addSeparator :: String -> (a -> String) -> a -> String -> String
addSeparator sep toStr x rest = toStr x ++ sep ++ rest

prettyPrint :: Board -> String
prettyPrint b =
  hSeparator ++ foldr (addSeparator hSeparator prettyPrintLine) "" b
