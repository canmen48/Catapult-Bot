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

import Catapult (Move(Move), playerWon, flagMoves, generalMoves, soldierMoves, catapultMoves, listMoves, isValidMove, retreatMovesBackwards)

main :: IO ()
main = hspec $ do
    describe "validateFen Function tests" $ do
        it "validates a correct FEN string" $ do
            let validFEN = "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/541/4Gg4/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
            validateFEN validFEN `shouldBe` True
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
    describe "buildBoard Function tests" $ do
        it "builds a valid board from a FEN string" $ do
            let fen = "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
            let board = buildBoard fen
            length board `shouldBe` 10
            all (\row -> length row == 10) board `shouldBe` True
        it "builds an empty board" $ do
            let fen = "/////////"
            let board = buildBoard fen
            length board `shouldBe` 10
            all (\row -> length row == 10) board `shouldBe` True

    describe "flagMoves Function" $ do

        it "calculates valid flag moves for a player White" $ do
            let board = buildBoard "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
            let moves = flagMoves board White
            not (null moves) `shouldBe` True
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
        it "calculates valid flag moves for a player Black" $ do
            let board = buildBoard "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
            let moves = flagMoves board Black
            not (null moves) `shouldBe` True
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
            let moves = flagMoves board Black
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
        it "empty instead of general white" $ do
            let board = buildBoard "/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
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
        it "empty instead of general black" $ do
            let board = buildBoard "/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
            let moves = generalMoves board Black (Pos 'j' 9)
            not (null moves) `shouldBe` False

    describe "soldierMoves Function tests" $ do
        it "calculates valid white soldier moves" $ do
            let board = buildBoard "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1//3w6"
            let moves = soldierMoves board White (Pos 'd' 9)
            not (null moves) `shouldBe` True
        it "calculates white soldier moves lower right corner" $ do
            let board = buildBoard "9w/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1//w9"
            let moves = soldierMoves board White (Pos 'j' 0)
            not (null moves) `shouldBe` True
        it "calculates white soldier moves lower left corner" $ do
            let board = buildBoard "w9/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1//w9"
            let moves = soldierMoves board White (Pos 'a' 0)
            not (null moves) `shouldBe` True
        it "calculates white soldier moves upper right corner" $ do
            let board = buildBoard "9b/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1//9w"
            let moves = soldierMoves board White (Pos 'j' 9)
            not (null moves) `shouldBe` True
        it "calculates white soldier moves upper left corner" $ do
            let board = buildBoard "b9/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1//w9"
            let moves = soldierMoves board White (Pos 'a' 9)
            not (null moves) `shouldBe` True
        it "calculates invalid white soldier moves corner" $ do
            let board = buildBoard "gwwwwwwwww/ww1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/wwwwwwwwww/wwwwwwwwww"
            let moves = soldierMoves board White (Pos 'j' 9)
            not (null moves) `shouldBe` False
        it "calculates invalid white soldier moves middle" $ do
            let board = buildBoard "gwwwwwwwww/ww1w1w1w1w/1w1w1w1w1w/wwwwwwwwww/wwwwwwwwww/wwwwwwwwww/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
            let moves = soldierMoves board White (Pos 'f' 4)
            not (null moves) `shouldBe` False
        it "wrong soldier white" $ do
            let board = buildBoard "bwwwwwwwww/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
            let moves = soldierMoves board White (Pos 'a' 0)
            not (null moves) `shouldBe` False
        it "general instead of soldier white" $ do
            let board = buildBoard "g9/111w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
            let moves = soldierMoves board White (Pos 'a' 0)
            not (null moves) `shouldBe` False
        it "empty instead of soldier white" $ do
            let board = buildBoard "/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
            let moves = soldierMoves board White (Pos 'a' 0)
            not (null moves) `shouldBe` False
        it "white soldier can takeout and can escape from every way" $ do 
            let board = buildBoard "/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/4w5///"
            let moves = soldierMoves board White (Pos 'e' 6)
            not (null moves) `shouldBe` True
        it "white invalid position (negative)" $ do --New tests begin here
            let board = buildBoard "/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/4w5///"
            let moves = soldierMoves board White (Pos 'e' 2)
            not (null moves) `shouldBe` False
        it "white invalid position (more than 9)" $ do 
            let board = buildBoard "/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/4w5///"
            let moves = soldierMoves board White (Pos 'e' 10)
            not (null moves) `shouldBe` False
        it "white invalid column letter" $ do 
            let board = buildBoard "/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/4w5///"
            let moves = soldierMoves board White (Pos 't' 5)
            not (null moves) `shouldBe` False
        it "white both column row invalid" $ do 
            let board = buildBoard "/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/4w5///"
            let moves = soldierMoves board White (Pos 't' 19)
            not (null moves) `shouldBe` False
        it "white soldier can takeout and can escape from diagonals way" $ do 
            let board = buildBoard "/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/3bGb4/4w5/4w5/4w5/4w5"
            let moves = soldierMoves board White (Pos 'e' 6)
            not (null moves) `shouldBe` True
        it "white soldier can takeout and can escape from just straight way" $ do 
            let board = buildBoard "/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/bbbb1bbbbb/4w5/bbbb1bbbbb/bbbb1bbbbb/bbbb1bbbbb"
            let moves = soldierMoves board White (Pos 'e' 6)
            not (null moves) `shouldBe` True
        it "white soldier can takeout cant escape (not enough spaces)" $ do
            let board = buildBoard "/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/bbbbbbbbbb/1w8"
            let moves = soldierMoves board White (Pos 'b' 9)
            not (null moves) `shouldBe` True
        it "white soldier can takeout cant escape (first tile obstructed second free)" $ do  
            let board = buildBoard "/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/4w5/wwwwwwwwww//"
            let moves = soldierMoves board White (Pos 'e' 6)
            not (null moves) `shouldBe` True
        it "white soldier can takeout cant escape (first tile free second obstructed)" $ do 
            let board = buildBoard "/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/4w5//wwwwwwwwww/"
            let moves = soldierMoves board White (Pos 'e' 6)
            not (null moves) `shouldBe` True
        it "white soldier can takeout cant escape (both tiles obstructed)" $ do 
            let board = buildBoard "/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/4w5/wwwwwwwwww/wwwwwwwwww/"
            let moves = soldierMoves board White (Pos 'e' 6)
            not (null moves) `shouldBe` True
        it "calculates valid black soldier moves" $ do
            let board = buildBoard "1b8//1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1//3b6"
            let moves = soldierMoves board Black (Pos 'd' 9)
            not (null moves) `shouldBe` True
        it "calculates black soldier moves lower right corner" $ do
            let board = buildBoard "9b/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1//w9"
            let moves = soldierMoves board Black (Pos 'j' 0)
            not (null moves) `shouldBe` True
        it "calculates black soldier moves lower left corner" $ do
            let board = buildBoard "b9/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1//w9"
            let moves = soldierMoves board Black (Pos 'a' 0)
            not (null moves) `shouldBe` True
        it "calculates black soldier moves upper right corner" $ do
            let board = buildBoard "9b/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1//9b"
            let moves = soldierMoves board Black (Pos 'j' 9)
            not (null moves) `shouldBe` True
        it "calculates black soldier moves upper left corner" $ do
            let board = buildBoard "b9/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1//b9"
            let moves = soldierMoves board Black (Pos 'a' 9)
            not (null moves) `shouldBe` True
        it "calculates invalid black soldier moves corner" $ do
            let board = buildBoard "bbbbbbbbbb/bbbbbbbbbb/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/wwwwwwwwww/wwwwwwwwww"
            let moves = soldierMoves board Black (Pos 'a' 0)
            not (null moves) `shouldBe` False
        it "calculates invalid black soldier moves middle" $ do
            let board = buildBoard "gwwwwwwwww/ww1w1w1w1w/1w1w1w1w1w/bbbbbbbbbb/bbbbbbbbbb/bbbbbbbbbb/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
            let moves = soldierMoves board Black (Pos 'f' 4)
            not (null moves) `shouldBe` False
        it "wrong soldier black" $ do
            let board = buildBoard "1w8/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
            let moves = soldierMoves board Black (Pos 'b' 0)
            not (null moves) `shouldBe` False
        it "general instead of soldier black" $ do
            let board = buildBoard "g9/111w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/1G8"
            let moves = soldierMoves board Black (Pos 'b' 9)
            not (null moves) `shouldBe` False
        it "empty instead of soldier black" $ do
            let board = buildBoard "/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
            let moves = soldierMoves board Black (Pos 'b' 9)
            not (null moves) `shouldBe` False
        it "black soldier can takeout and can escape from every way" $ do --New tests begin here
            let board = buildBoard "///4b5/wwwwwwwwww/4G5/4w5///"
            let moves = soldierMoves board Black (Pos 'e' 3)
            not (null moves) `shouldBe` True
        it "black soldier can takeout and can escape from diagonals way" $ do 
            let board = buildBoard "4b5/4b5/4b5/4b5/wwwwwwwwww/4G5/4w5///"
            let moves = soldierMoves board Black (Pos 'e' 3)
            not (null moves) `shouldBe` True
        it "black soldier can takeout and can escape from just straight way" $ do 
            let board = buildBoard "bbbb1bbbbb/bbbb1bbbbb/bbbb1bbbbb/4b5/wwwwwwwwww/4G5/4w5///"
            let moves = soldierMoves board Black (Pos 'e' 3)
            not (null moves) `shouldBe` True
        it "black soldier can takeout cant escape (not enough spaces)" $ do
            let board = buildBoard "/1b8/wwwwwwwww/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/bbbbbbbbbb/1w8"
            let moves = soldierMoves board Black (Pos 'b' 1)
            not (null moves) `shouldBe` True
        it "black soldier can takeout cant escape (first tile obstructed second free)" $ do  
            let board = buildBoard "//bbbbbbbbbb/4b5/wwwwwwwwww/4G5/4w5///"
            let moves = soldierMoves board Black (Pos 'e' 3)
            not (null moves) `shouldBe` True
        it "black soldier can takeout cant escape (first tile free second obstructed)" $ do 
            let board = buildBoard "/bbbbbbbbbb//4b5/wwwwwwwwww/4G5/4w5///"
            let moves = soldierMoves board Black (Pos 'e' 3)
            not (null moves) `shouldBe` True
        it "black soldier can takeout cant escape (both tiles obstructed)" $ do 
            let board = buildBoard "/bbbbbbbbbb/bbbbbbbbbb/4b5/wwwwwwwwww/4G5/4w5///"
            let moves = soldierMoves board Black (Pos 'e' 3)
            not (null moves) `shouldBe` True
        it "black invalid position (negative)" $ do --New tests begin here
            let board = buildBoard "/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/4w5///"
            let moves = soldierMoves board Black (Pos 'e' 2)
            not (null moves) `shouldBe` False
        it "black invalid position (more than 9)" $ do 
            let board = buildBoard "/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/4w5///"
            let moves = soldierMoves board Black (Pos 'e' 10)
            not (null moves) `shouldBe` False
        it "black invalid column letter" $ do 
            let board = buildBoard "/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/4w5///"
            let moves = soldierMoves board Black (Pos 't' 5)
            not (null moves) `shouldBe` False
        it "black both column row invalid" $ do 
            let board = buildBoard "/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/4w5///"
            let moves = soldierMoves board Black (Pos 't' 19)
            not (null moves) `shouldBe` False
    describe "catapultMoves Function tests" $ do
        it "calculates invalid catapult shift moves (no space)"  $ do
            let board = buildBoard "2b7/2b7/2b7///////1W8"
            let moves = catapultMoves board Black (Pos 'c' 2)
            not (null moves) `shouldBe` False
        it "calculates valid catapult shift moves" $ do
            let board = buildBoard "/2b7/3bG5/4b5//////1W8"
            let moves = catapultMoves board Black (Pos 'c' 1)
            not (null moves) `shouldBe` True
        it "calculates valid catapult shoot and shift moves" $ do
            let board = buildBoard "/2b7/3bG5/4b5//6w3/2w7///1W8"
            let moves = catapultMoves board Black (Pos 'c' 1)
            not (null moves) `shouldBe` True
    describe "playerWon Function tests" $ do
        it "returns False when both players still have their general and flag, and moves are available" $ do
            let board = buildBoard "1B8/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/4w5///1W8"
            playerWon board White `shouldBe` False
            playerWon board Black `shouldBe` False

        it "returns True when the opponent has no flag" $ do
            let board = buildBoard "/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/4w5///1W8"
            playerWon board White `shouldBe` True
            playerWon board Black `shouldBe` False

        it "returns True when the opponent has no general" $ do
            let board = buildBoard "1B8/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4//4w5///1W8"
            playerWon board White `shouldBe` True
            playerWon board Black `shouldBe` False

        it "returns True when the opponent has no valid moves" $ do
            let board = buildBoard "1B8/1w8/wwwwwwwwww/wwwwgwwwww/wwwwGwwwww/wwwwwwwwww////1W8"
            playerWon board White `shouldBe` True
            playerWon board Black `shouldBe` False

        it "returns True when the opponent has no flag" $ do
            let board = buildBoard "1B8/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/4w5///"
            playerWon board White `shouldBe` False
            playerWon board Black `shouldBe` True

        it "returns True when the opponent has no general" $ do
            let board = buildBoard "1B8/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5G4//4w5///1W8"
            playerWon board White `shouldBe` False
            playerWon board Black `shouldBe` True

        it "returns True when the opponent has no valid moves" $ do
            let board = buildBoard "1B8/bbbbbbbbbb/bbbbgbbbbb/bbbbbbbbbb//4G5////1W8"
            playerWon board White `shouldBe` False
            playerWon board Black `shouldBe` True

    describe "listMoves Function tests" $ do
        --Some tests here in listMoves functions are created by AI and some of them are modified by it
        it "returns all valid moves for White when both players have their generals, flags, and valid moves" $ do
            let board = buildBoard "1B8/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/4w5///1W8"
            let moves = listMoves board White
            not (null moves) `shouldBe` True -- Ensure moves exist
            

        it "returns all valid moves for Black when both players have their generals, flags, and valid moves" $ do
            let board = buildBoard "1B8/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/4w5///1W8"
            let moves = listMoves board Black
            not (null moves) `shouldBe` True -- Ensure moves exist
            

        it "returns all valid moves for White when Black has no flag" $ do
            let board = buildBoard "/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/4w5///1W8"
            let moves = listMoves board White
            not (null moves) `shouldBe` True -- Ensure moves exist
            

        it "returns all valid moves for Black when Black has no flag" $ do
            let board = buildBoard "/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/4w5///1W8"
            let moves = listMoves board Black
            not (null moves) `shouldBe` True -- Ensure moves exist
            

        it "returns all valid moves for Black when White has no general" $ do
            let board = buildBoard "1B8/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5G4//4w5///1W8"
            let moves = listMoves board Black
            not (null moves) `shouldBe` True -- Ensure moves exist
            

        it "returns all valid moves for White when White has no general" $ do
            let board = buildBoard "1B8/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5G4//4w5///1W8"
            let moves = listMoves board White
            not (null moves) `shouldBe` True -- Ensure moves exist
            

        it "returns only flag and general moves for both players when there are no soldiers" $ do
            let board = buildBoard "/3g6/3b6////3G6/3W6///"
            let whiteMoves = listMoves board White
            let blackMoves = listMoves board Black
            not (null whiteMoves) `shouldBe` True -- Ensure White has moves
            not (null blackMoves) `shouldBe` True -- Ensure Black has moves
    describe "Helper functions test" $ do
        it "invalid input on isValidMove function" $ do
            let board = buildBoard "1B8/3w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5G4//4w5///1W8"
            let moves = listMoves board White
            isValidMove board (Pos 't' 5) `shouldBe` False 

        it "retreatMovesBackwards test" $ do 
            let board = buildBoard "4b5/4b5/4b5/4b5/wwwwwwwwww/4G5/4w5///"
            let moves = soldierMoves board Black (Pos 'e' 3)
            not (null moves) `shouldBe` True