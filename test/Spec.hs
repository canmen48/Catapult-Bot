-- #############################################################################
-- ###### GRADING TESTS                                               ##########
-- #############################################################################

import Test.Hspec

import Board
     (validateFEN,
      buildBoard,
      Player(White, Black),
      Cell(Empty, Flag, Soldier, General),
      Pos(Pos))

import Catapult (Move(Move), playerWon, flagMoves, generalMoves, soldierMoves, catapultMoves, listMoves)

main :: IO ()
main = hspec $ do
    describe "validateFen Function tests" $ do
        it "validates an empty FEN String" $ do
            let validFEN = ""
            validateFEN validFEN `shouldBe` False
        it "invalidates an incorrect FEN string" $ do
            let invalidFEN = "/1w1w1w1w1w/bbbbbbbbbb/GGGGGGGGGd//////"
            validateFEN invalidFEN `shouldBe` False
        it "invalidates a short FEN (not enough rows) " $ do
            let invalidFEN = "1w1w1w1w1w/1w1w1w1w1w/w1w1w1w1w1/5g4/4G5"
            validateFEN invalidFEN `shouldBe` False
        it "invalidates a short FEN (not enough columns)" $ do
            let invalidFEN = "1w1w1w1w1w/1w1w1w1w1w/w1w1w1w1w1/5g4/4G5/b1b1b1b1b1/"
            validateFEN invalidFEN `shouldBe` False
        
    describe "flagMoves Function" $ do
        it "white flag already placed" $ do
            let board = buildBoard "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/1W8"
            let moves = flagMoves board White
            not (null moves) `shouldBe` False
        it "calculates valid flag moves for a player White (no moves) full" $ do
            let board = buildBoard "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/bbbbbbbbbb"
            let moves = flagMoves board White
            not (null moves) `shouldBe` False
        it "calculates valid flag moves for a player White (no moves)" $ do
            let board = buildBoard "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/1bbbbbbbb1"
            let moves = flagMoves board White
            not (null moves) `shouldBe` False
        it "black flag already placed" $ do
            let board = buildBoard "1B8/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
            let moves = flagMoves board Black
            not (null moves) `shouldBe` False
        it "calculates valid flag moves for a player Black (no moves) full" $ do
            let board = buildBoard "wwwwwwwwww/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
            let moves = flagMoves board Black
            not (null moves) `shouldBe` False
        it "calculates valid flag moves for a player Black (no moves)" $ do
            let board = buildBoard "1wwwwwwww1/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
            let moves = flagMoves board Black
            not (null moves) `shouldBe` False
        it "flagMoves with all positions filled with obstacles for Black" $ do
            let board = buildBoard "wwwwwwwwww/wwwwwwwwww/wwwwwwwwww/wwwwwwwwww/wwwwwwwwww/wwwwwwwwww/wwwwwwwwww/wwwwwwwwww/wwwwwwwwww/wwwwwwwwww"
            let moves = flagMoves board White
            moves `shouldBe` []

    describe "generalMoves Function tests" $ do

        it "calculates valid white general moves" $ do
            let board = buildBoard "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
            let moves = generalMoves board White (Pos 'f' 4)
            not (null moves) `shouldBe` True
        it "calculates invalid white general moves corner" $ do
            let board = buildBoard "gwwwwwwwww/ww1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
            let moves = generalMoves board White (Pos 'a' 0)
            not (null moves) `shouldBe` False
        it "calculates invalid white general moves middle" $ do
            let board = buildBoard "gwwwwwwwww/ww1w1w1w1w/1w1w1w1w1w/wwwwwwwwww/wwwwwgwwww/bbbbGbbbbb/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
            let moves = generalMoves board White (Pos 'f' 4)
            not (null moves) `shouldBe` False
        it "wrong general white" $ do
            let board = buildBoard "G9/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
            let moves = generalMoves board White (Pos 'a' 0)
            not (null moves) `shouldBe` False
        it "soldier instead of general white" $ do
            let board = buildBoard "w9/111w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
            let moves = generalMoves board White (Pos 'a' 0)
            not (null moves) `shouldBe` False
        it "calculates valid black general moves" $ do
            let board = buildBoard "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
            let moves = generalMoves board Black (Pos 'e' 5)
            not (null moves) `shouldBe` True
        it "calculates invalid black general moves corner" $ do
            let board = buildBoard "/ww1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1bbbb/bbbbbbbbbG"
            let moves = generalMoves board Black (Pos 'j' 9)
            not (null moves) `shouldBe` False
        it "calculates invalid black general moves middle" $ do
            let board = buildBoard "gwwwwwwwww/ww1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/wwwwwgwwww/bbbbGbbbbb/bbbbbbbbbb/b1b1b1b1b1/b1b1b1b1b1/"
            let moves = generalMoves board Black (Pos 'e' 5)
            not (null moves) `shouldBe` False
        it "wrong general black" $ do
            let board = buildBoard "Gwwwwwwwww/ww1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/9g"
            let moves = generalMoves board Black (Pos 'j' 9)
            not (null moves) `shouldBe` False
        it "soldier instead of general black" $ do
            let board = buildBoard "w9/111w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/9b"
            let moves = generalMoves board Black (Pos 'j' 9)
            not (null moves) `shouldBe` False
        
    
    