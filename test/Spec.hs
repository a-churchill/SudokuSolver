import           Test.Hspec
import           Test.QuickCheck
import           Control.Exception              ( evaluate )
import           Board
import           Solver
import qualified Data.Set                      as Set
import qualified Data.Sequence                 as Seq

board :: Board
board = Seq.fromList
    [ Seq.fromList
        [ Just 7
        , Nothing
        , Nothing
        , Nothing
        , Nothing
        , Just 1
        , Nothing
        , Nothing
        , Nothing
        ]
    , Seq.fromList
        [ Nothing
        , Just 6
        , Nothing
        , Nothing
        , Nothing
        , Nothing
        , Nothing
        , Just 1
        , Nothing
        ]
    , Seq.fromList
        [ Just 2
        , Nothing
        , Nothing
        , Nothing
        , Just 7
        , Just 5
        , Just 9
        , Nothing
        , Nothing
        ]
    , Seq.fromList
        [ Just 5
        , Nothing
        , Just 3
        , Nothing
        , Nothing
        , Nothing
        , Nothing
        , Just 7
        , Nothing
        ]
    , Seq.fromList
        [ Nothing
        , Just 4
        , Nothing
        , Nothing
        , Nothing
        , Just 7
        , Nothing
        , Nothing
        , Nothing
        ]
    , Seq.fromList
        [ Nothing
        , Just 1
        , Nothing
        , Just 6
        , Just 4
        , Nothing
        , Nothing
        , Nothing
        , Nothing
        ]
    , Seq.fromList
        [ Just 6
        , Nothing
        , Nothing
        , Nothing
        , Nothing
        , Nothing
        , Nothing
        , Nothing
        , Nothing
        ]
    , Seq.fromList
        [ Just 4
        , Just 9
        , Just 5
        , Nothing
        , Nothing
        , Nothing
        , Just 7
        , Nothing
        , Nothing
        ]
    , Seq.fromList
        [ Nothing
        , Nothing
        , Nothing
        , Nothing
        , Nothing
        , Nothing
        , Just 3
        , Just 5
        , Just 9
        ]
    ]

boardSol :: Board
boardSol = Seq.fromList
    [ Seq.fromList
        [Just 7, Just 5, Just 4, Just 9, Just 6, Just 1, Just 2, Just 8, Just 3]
    , Seq.fromList
        [Just 3, Just 6, Just 9, Just 8, Just 2, Just 4, Just 5, Just 1, Just 7]
    , Seq.fromList
        [Just 2, Just 8, Just 1, Just 3, Just 7, Just 5, Just 9, Just 4, Just 6]
    , Seq.fromList
        [Just 5, Just 2, Just 3, Just 1, Just 9, Just 8, Just 6, Just 7, Just 4]
    , Seq.fromList
        [Just 8, Just 4, Just 6, Just 5, Just 3, Just 7, Just 1, Just 9, Just 2]
    , Seq.fromList
        [Just 9, Just 1, Just 7, Just 6, Just 4, Just 2, Just 8, Just 3, Just 5]
    , Seq.fromList
        [Just 6, Just 3, Just 8, Just 7, Just 5, Just 9, Just 4, Just 2, Just 1]
    , Seq.fromList
        [Just 4, Just 9, Just 5, Just 2, Just 1, Just 3, Just 7, Just 6, Just 8]
    , Seq.fromList
        [Just 1, Just 7, Just 2, Just 4, Just 8, Just 6, Just 3, Just 5, Just 9]
    ]

main :: IO ()
main = hspec $ do
    describe "Board.get" $ do
        it "returns correct value at (8, 8)" $ do
            get board (8, 8) `shouldBe` Just 9
        it "returns correct value at (4, 7)" $ do
            get board (4, 7) `shouldBe` Nothing
        it "returns nothing out of bounds" $ do
            get board (-1, 9) `shouldBe` Nothing
    describe "Board.getSquareValues" $ do
        it "returns correct values at (1, 3)" $ do
            getSquareValues board (1, 3) `shouldBe` Set.fromList [1, 5, 7]
        it "returns correct values at (0, 0)" $ do
            getSquareValues board (0, 0) `shouldBe` Set.fromList [2, 6, 7]
        it "returns correct values at (7, 1)" $ do
            getSquareValues board (7, 1) `shouldBe` Set.fromList [4, 5, 6, 9]
        it "returns correct values at (8, 5)" $ do
            getSquareValues board (8, 5) `shouldBe` Set.fromList []
        it "returns correct values at (6, 8)" $ do
            getSquareValues board (6, 8) `shouldBe` Set.fromList [3, 5, 7, 9]
    describe "Board.getRowValues" $ do
        it "returns correct values at (8, 2)" $ do
            getRowValues board (8, 2) `shouldBe` Set.fromList [3, 5, 9]
        it "returns correct values at (2, 5)" $ do
            getRowValues board (2, 5) `shouldBe` Set.fromList [2, 5, 7, 9]
        it "returns correct values at (8, 2)" $ do
            getRowValues board (8, 8) `shouldBe` Set.fromList [3, 5, 9]
    describe "Board.getColValues" $ do
        it "returns correct values at (3, 5)" $ do
            getColValues board (3, 5) `shouldBe` Set.fromList [1, 5, 7]
        it "returns correct values at (3, 5)" $ do
            getColValues board (5, 1) `shouldBe` Set.fromList [1, 4, 6, 9]
        it "returns correct values at (3, 5)" $ do
            getColValues board (0, 0) `shouldBe` Set.fromList [2, 4, 5, 6, 7]
    describe "Solver.getValidValues" $ do
        it "returns correct values at (7, 8)" $ do
            getValidValues board (7, 8) `shouldBe` Set.fromList [1, 2, 6, 8]
        it "returns correct values at (4, 2)" $ do
            getValidValues board (4, 2) `shouldBe` Set.fromList [2, 6, 8, 9]
        it "returns correct values at (0, 5)" $ do
            getValidValues board (0, 5) `shouldBe` Set.fromList [1]
        it "returns correct values at (5, 2)" $ do
            getValidValues board (5, 2) `shouldBe` Set.fromList [2, 7, 8, 9]
    describe "Solver.solve" $ do
        it "returns solution for board 3" $ do
            solve board `shouldBe` Just (boardSol)

