-- #############################################################################
-- ###### GRADING TESTS                                               ##########
-- #############################################################################

import Test.Hspec
import Board
import Board
     (validateFEN,
      buildBoard,
      Player(White, Black),
      Cell(Empty, Flag, Soldier, General),
      Pos(Pos))

import Catapult (Move(Move), playerWon, flagMoves, generalMoves, soldierMoves, catapultMoves, listMoves)


sampleBoard = [
  [Empty          , Empty          , Empty          , Empty          , (Flag White)   , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
  [Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
  [Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
  [Empty          , Empty          , Empty          , Empty          , Empty          , (General White), Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , (General Black), Empty          , Empty          , Empty          , Empty          , Empty          ],
  [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
  [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
  [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , (Flag Black)   , Empty          , Empty          ]
  ]

blackGeneralOnEdge = [
  [Empty          , Empty          , Empty          , Empty          , (Flag White)   , Empty          , Empty          , Empty          , Empty          , (General Black)],
  [Empty          , (Soldier White), Empty          , (Soldier White), (General White), (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
  [Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
  [Empty          , (Soldier White), Empty          , (Soldier White), (Soldier Black), (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
  [Empty          , Empty          , Empty          , Empty          , (Soldier Black), Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , (Soldier Black), Empty          , Empty          , (Soldier Black), Empty          , Empty          ],
  [(Soldier Black), Empty          , Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , Empty          , (Soldier Black), Empty          ],
  [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , (Flag Black)   , Empty          , Empty          ]
  ]

initialBoard = [
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
  [Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
  [Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
  [Empty          , Empty          , Empty          , Empty          , Empty          , (General White), Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , (General Black), Empty          , Empty          , Empty          , Empty          , Empty          ],
  [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
  [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
  [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ]
  ]

initialBoardBlack = [
  [Empty          , Empty          , Empty          , Empty          , (Flag White)   , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
  [Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
  [Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
  [Empty          , Empty          , Empty          , Empty          , Empty          , (General White), Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , (General Black), Empty          , Empty          , Empty          , Empty          , Empty          ],
  [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
  [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
  [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ]
  ]

cannonShots = [
  [Empty          , Empty          , Empty          , Empty          , (Flag White)   , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , (Soldier White), (General White), (Soldier White), Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , (Soldier White), Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , (Soldier White), Empty          , (Soldier White), Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , (Soldier White)],
  [Empty          , Empty          , Empty          , (Soldier Black), Empty          , Empty          , Empty          , Empty          , (Soldier White), Empty          ],
  [Empty          , Empty          , (Soldier Black), Empty          , Empty          , Empty          , Empty          , (Soldier White), Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , (General Black), Empty          , Empty          , Empty          , (Soldier White), Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , (Soldier Black), Empty          , Empty          , (Flag Black)   , Empty          , Empty          ]
  ]

cannonShots2 = [
  [Empty          , Empty          , Empty          , Empty          , (Flag White)   , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , (Soldier White), Empty          , (Soldier White), Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , (Soldier White), Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , (Soldier White), Empty          , (Soldier White), Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , (General White), (Soldier White)],
  [Empty          , Empty          , Empty          , (Soldier Black), Empty          , Empty          , Empty          , Empty          , (Soldier White), Empty          ],
  [Empty          , Empty          , (Soldier Black), Empty          , Empty          , Empty          , Empty          , (Soldier White), Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , (General Black), Empty          , Empty          , Empty          , (Soldier White), Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , (Soldier Black), Empty          , Empty          , (Flag Black)   , Empty          , Empty          ]
  ]

whiteNoFlag = [
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , (General Black)],
  [Empty          , (Soldier White), Empty          , (Soldier White), (General White), (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
  [Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
  [Empty          , (Soldier White), Empty          , (Soldier White), (Soldier Black), (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
  [Empty          , Empty          , Empty          , Empty          , (Soldier Black), Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , (Soldier Black), Empty          , Empty          , (Soldier Black), Empty          , Empty          ],
  [(Soldier Black), Empty          , Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , Empty          , (Soldier Black), Empty          ],
  [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , (Flag Black)   , Empty          , Empty          ]
  ]

blackNoFlag = [
  [Empty          , Empty          , Empty          , Empty          , (Flag White)   , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , (General White), (Soldier White), Empty          , (Soldier White), Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , (Soldier White), Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , (Soldier White), Empty          , (Soldier White), Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , (Soldier White)],
  [Empty          , Empty          , Empty          , (Soldier Black), Empty          , Empty          , Empty          , Empty          , (Soldier White), Empty          ],
  [Empty          , Empty          , (Soldier Black), Empty          , Empty          , Empty          , Empty          , (Soldier White), Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , (General Black), Empty          , Empty          , Empty          , (Soldier White), Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , (Soldier Black), Empty          , Empty          , Empty          , Empty          , Empty          ]
  ]

blackNoMoves = [
  [Empty          , (Soldier White), Empty          , Empty          , Empty          , Empty          , Empty          , (Flag White)   , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , (General White), Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , (Soldier White), Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [(Soldier White), (Soldier White), Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [(General Black), (Flag Black)   , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ]
  ]

whiteNoGeneral = [
  [Empty          , Empty          , Empty          , Empty          , (Flag White)   , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
  [Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
  [Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , (General Black), Empty          , Empty          , Empty          , Empty          , Empty          ],
  [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
  [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
  [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , (Flag Black)   , Empty          , Empty          ]
  ]

emptyBoard = [
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ]
  ]

blackGeneralOnEdge2 = [
  [Empty          , Empty          , Empty          , Empty          , (Flag White)   , Empty          , Empty          , Empty          , Empty          , (General Black)],
  [Empty          , (Soldier White), Empty          , (Soldier White), (General White), (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
  [Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
  [Empty          , (Soldier White), Empty          , (Soldier White), (Soldier Black), (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
  [Empty          , Empty          , Empty          , Empty          , (Soldier Black), Empty          , Empty          , Empty          , (Soldier Black), (Soldier Black)],
  [Empty          , Empty          , Empty          , Empty          , (Soldier Black), Empty          , Empty          , Empty          , Empty          , Empty          ],
  [(Soldier Black), Empty          , Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , Empty          , Empty          , Empty          ],
  [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
  [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , (Flag Black)   , Empty          , Empty          ]
  ]


allElementsIn :: Eq a => [a] -> [a] -> Bool
allElementsIn xs ys = all (`elem` ys) xs

containsElements :: Eq a => [a] -> [a] -> Bool
containsElements xs ys = allElementsIn ys xs

sameElements :: Eq a => [a] -> [a] -> Bool
sameElements xs ys = (allElementsIn xs ys) && (allElementsIn ys xs)

main :: IO ()
main = hspec $ do
    testValidateFEN
    testBuildBoard
    testFlagMoves
    testGeneralMoves
    testSoldierMoves
    testCatapultMoves
    testPlayerWon
    testListMoves


testValidateFEN :: Spec
testValidateFEN = describe "IF Grade validateFEN 2FP" $ do
        it "empty string is not valid - TEST: validateFEN " $ do
            validateFEN "" `shouldBe` (False :: Bool)
        it "row missing - TEST: validateFEN  " $ do
            validateFEN "1W8/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1" `shouldBe` (False :: Bool)
        it "column missing - TEST: validateFEN " $ do
            validateFEN "W8/w1w1w1w1w/w1w1w1w1w/w1w1w1w1w/4g4/3G5/1b1b1b1b1/1b1b1b1b1/1b1b1b1b1/B8" `shouldBe` (False :: Bool)
        it "wrong character - TEST: validateFEN " $ do
            validateFEN "1W8/1w1w1w1f1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/1B8" `shouldBe` (False :: Bool)
        it "correct board - TEST: validateFEN " $ do
            validateFEN "1W8/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/1B8" `shouldBe` (True :: Bool)
        it "correctboard alternative representation - TEST: validateFEN " $ do
            validateFEN "1W8/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g22/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/1B8" `shouldBe` (True :: Bool)
        it "empty board is also valid - TEST: validateFEN " $ do
            validateFEN "1W8/1wg7///////b1G7/1B8" `shouldBe` (True :: Bool)
        it "no towns is valid - TEST: validateFEN " $ do
            validateFEN "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/" `shouldBe` (True :: Bool)


testBuildBoard :: Spec
testBuildBoard = describe "IF Grade buildBoard 2FP" $ do
        it "sampleBoard - TEST: buildBoard " $ do
            buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2" `shouldBe` sampleBoard
        it "blackGeneralOnEdge - TEST: buildBoard " $ do
            buildBoard "4W4G/1w1wgw1w1w/1w1w1w1w1w/1w1wbw1w1w/4b5/4b2b2/b2b1b2b1/b1b1b1b1b1/6b1b1/7B2" `shouldBe` blackGeneralOnEdge
        it "cannonShots - TEST: buildBoard " $ do
            buildBoard "4W5/5wgw2/6w3/5w1w2/9w/3b4w1/2b4w2//1G3w4/4b2B2" `shouldBe` cannonShots
        it "whiteNoFlag - TEST: buildBoard " $ do
            buildBoard "9G/1w1wgw1w1w/1w1w1w1w1w/1w1wbw1w1w/4b5/4b2b2/b2b1b2b1/b1b1b1b1b1/6b1b1/7B2" `shouldBe` whiteNoFlag
        it "blackNoFlag - TEST: buildBoard " $ do
            buildBoard "4W5/4gw1w2/6w3/5w1w2/9w/3b4w1/2b4w2//1G3w4/4b5" `shouldBe` blackNoFlag
        it "blackNoMoves - TEST: buildBoard " $ do
            buildBoard "1w5W2/4g5/1w8//////ww8/GB8" `shouldBe` blackNoMoves
        it "initialBoard - TEST: buildBoard " $ do
            buildBoard "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/" `shouldBe` initialBoard
        it "emptyBoard - TEST: buildBoard " $ do
            buildBoard "/////////" `shouldBe` emptyBoard


testFlagMoves :: Spec
testFlagMoves = describe "IF Grade flagMoves 2FP" $ do
        it "white flag already placed - TEST: flagMoves " $ do
            flagMoves sampleBoard White `sameElements` []
        it "black flag already placed - TEST: flagMoves " $ do
            flagMoves sampleBoard White `sameElements` []
        it "no flag placed, white's turn - TEST: flagMoves " $ do
            flagMoves initialBoard White `sameElements` [(Move (Pos 'b' 9) (Pos 'b' 9)), 
                                                     (Move (Pos 'c' 9) (Pos 'c' 9)), 
                                                     (Move (Pos 'd' 9) (Pos 'd' 9)), 
                                                     (Move (Pos 'e' 9) (Pos 'e' 9)), 
                                                     (Move (Pos 'f' 9) (Pos 'f' 9)), 
                                                     (Move (Pos 'g' 9) (Pos 'g' 9)), 
                                                     (Move (Pos 'h' 9) (Pos 'h' 9)), 
                                                     (Move (Pos 'i' 9) (Pos 'i' 9))]
        it "no flag placed, black's turn - TEST: flagMoves " $ do
            flagMoves initialBoardBlack Black `sameElements` [(Move (Pos 'b' 0) (Pos 'b' 0)), 
                                                          (Move (Pos 'c' 0) (Pos 'c' 0)), 
                                                          (Move (Pos 'd' 0) (Pos 'd' 0)), 
                                                          (Move (Pos 'e' 0) (Pos 'e' 0)), 
                                                          (Move (Pos 'f' 0) (Pos 'f' 0)), 
                                                          (Move (Pos 'g' 0) (Pos 'g' 0)), 
                                                          (Move (Pos 'h' 0) (Pos 'h' 0)), 
                                                          (Move (Pos 'i' 0) (Pos 'i' 0))]

testGeneralMoves :: Spec
testGeneralMoves = describe "IF Grade generalMoves 2FP" $ do
        it "moves on sampleBoard - TEST: generalMoves " $ do
            generalMoves sampleBoard White (Pos 'f' 5) `sameElements` [(Move (Pos 'f' 5) (Pos 'e' 6)), 
                                                                   (Move (Pos 'f' 5) (Pos 'e' 5)), 
                                                                   (Move (Pos 'f' 5) (Pos 'f' 4)), 
                                                                   (Move (Pos 'f' 5) (Pos 'g' 4)), 
                                                                   (Move (Pos 'f' 5) (Pos 'g' 5)), 
                                                                   (Move (Pos 'f' 5) (Pos 'g' 6))]
        it "empty square - TEST: generalMoves " $ do
            generalMoves sampleBoard White (Pos 'g' 5) `sameElements` []
        it "soldier square - TEST: generalMoves " $ do
            generalMoves sampleBoard White (Pos 'f' 6) `sameElements` []
        it "wrong color - TEST: generalMoves " $ do
            generalMoves sampleBoard Black (Pos 'f' 5) `sameElements` []
        it "black general on edge - TEST: generalMoves " $ do
            generalMoves blackGeneralOnEdge Black (Pos 'j' 9) `sameElements` [(Move (Pos 'j' 9) (Pos 'i' 9)), 
                                                                          (Move (Pos 'j' 9) (Pos 'i' 8))]
        it "white general sampleBoard - TEST: generalMoves " $ do
            generalMoves sampleBoard White (Pos 'f' 5) `sameElements` [(Move (Pos 'f' 5) (Pos 'e' 6)), 
                                                                       (Move (Pos 'f' 5) (Pos 'e' 5)), 
                                                                       (Move (Pos 'f' 5) (Pos 'f' 4)), 
                                                                       (Move (Pos 'f' 5) (Pos 'g' 6)), 
                                                                       (Move (Pos 'f' 5) (Pos 'g' 5)), 
                                                                       (Move (Pos 'f' 5) (Pos 'g' 4))]
        it "white general cannonShots - TEST: generalMoves " $ do
            generalMoves cannonShots Black (Pos 'b' 1) `sameElements` [(Move (Pos 'b' 1) (Pos 'a' 2)), 
                                                                       (Move (Pos 'b' 1) (Pos 'a' 1)), 
                                                                       (Move (Pos 'b' 1) (Pos 'a' 0)), 
                                                                       (Move (Pos 'b' 1) (Pos 'b' 2)), 
                                                                       (Move (Pos 'b' 1) (Pos 'b' 0)), 
                                                                       (Move (Pos 'b' 1) (Pos 'c' 2)), 
                                                                       (Move (Pos 'b' 1) (Pos 'c' 1)), 
                                                                       (Move (Pos 'b' 1) (Pos 'c' 0))]
        it "black general no moves - TEST: generalMoves " $ do
            generalMoves blackNoMoves Black (Pos 'a' 0) `sameElements` []


testSoldierMoves :: Spec
testSoldierMoves = describe "IF Grade soldierMoves 2FP" $ do
        it "empty square - TEST: soldierMoves " $ do
            soldierMoves blackGeneralOnEdge Black (Pos 'i' 9) `sameElements` []
        it "general square - TEST: soldierMoves " $ do
            soldierMoves blackGeneralOnEdge Black (Pos 'j' 9) `sameElements` []
        it "wrong color - TEST: soldierMoves " $ do
            soldierMoves blackGeneralOnEdge White (Pos 'h' 4) `sameElements` []
        it "normal moves - TEST: soldierMoves " $ do
            soldierMoves blackGeneralOnEdge Black (Pos 'i' 3) `sameElements` [(Move (Pos 'i' 3) (Pos 'i' 4)),
                                                                          (Move (Pos 'i' 3) (Pos 'h' 3)),
                                                                          (Move (Pos 'i' 3) (Pos 'j' 3))]
        it "blocked moves - TEST: soldierMoves " $ do
            soldierMoves blackGeneralOnEdge Black (Pos 'i' 2) `sameElements` [(Move (Pos 'i' 2) (Pos 'h' 2)),
                                                                          (Move (Pos 'i' 2) (Pos 'j' 2))]
        it "retreat moves - TEST: soldierMoves " $ do
            soldierMoves blackGeneralOnEdge Black (Pos 'e' 5) `containsElements` [(Move (Pos 'e' 5) (Pos 'g' 3)),
                                                                               (Move (Pos 'e' 5) (Pos 'c' 3))]
        it "kill moves1 - TEST: soldierMoves " $ do
            soldierMoves blackGeneralOnEdge2 White (Pos 'j' 6) `containsElements` [(Move (Pos 'j' 6) (Pos 'i' 5)),
                                                                               (Move (Pos 'j' 6) (Pos 'j' 5))]
        it "kill moves2 - TEST: soldierMoves " $ do
            soldierMoves blackGeneralOnEdge2 Black (Pos 'i' 5) `containsElements` [(Move (Pos 'i' 5) (Pos 'h' 6)),
                                                                               (Move (Pos 'i' 5) (Pos 'j' 6))]

testCatapultMoves :: Spec
testCatapultMoves = describe "IF Grade catapultMoves 2FP" $ do
        it "empty square - TEST: catapultMoves " $ do
            catapultMoves blackGeneralOnEdge Black (Pos 'j' 5) `sameElements` []
        it "general square - TEST: catapultMoves " $ do
            catapultMoves blackGeneralOnEdge Black (Pos 'j' 9) `sameElements` []
        it "wrong color - TEST: catapultMoves " $ do
            catapultMoves blackGeneralOnEdge White (Pos 'e' 4) `sameElements` []
        it "not a cannon - TEST: catapultMoves " $ do
            catapultMoves blackGeneralOnEdge Black (Pos 'a' 2) `sameElements` []
        it "no targets - TEST: catapultMoves " $ do
            catapultMoves blackGeneralOnEdge Black (Pos 'i' 1) `sameElements` [(Move (Pos 'i' 1) (Pos 'i' 4))]
        it "blocked by board boundaries - TEST: catapultMoves " $ do
            catapultMoves cannonShots White (Pos 'f' 8) `sameElements` [(Move (Pos 'f' 8) (Pos 'i' 5))]
        it "normal moves1 - TEST: catapultMoves " $ do
            catapultMoves cannonShots White (Pos 'h' 8) `sameElements` [(Move (Pos 'h' 8) (Pos 'e' 5)),
                                                                        (Move (Pos 'h' 8) (Pos 'd' 4)),
                                                                        (Move (Pos 'h' 8) (Pos 'c' 3))]
        it "normal moves2 - TEST: catapultMoves " $ do
            catapultMoves cannonShots White (Pos 'j' 5) `sameElements` [(Move (Pos 'j' 5) (Pos 'g' 2))]


testPlayerWon :: Spec
testPlayerWon = describe "IF Grade playerWon 2FP" $ do
        it "black did not win yet - TEST: playerWon " $ do
            playerWon blackGeneralOnEdge Black `shouldBe` False
        it "white did not win yet - TEST: playerWon " $ do
            playerWon blackGeneralOnEdge White `shouldBe` False
        it "white no flag black - TEST: playerWon " $ do
            playerWon whiteNoFlag Black `shouldBe` True
        it "white no flag white - TEST: playerWon " $ do
            playerWon whiteNoFlag White `shouldBe` False
        it "black no flag white - TEST: playerWon " $ do
            playerWon blackNoFlag White `shouldBe` True
        it "black no flag black - TEST: playerWon " $ do
            playerWon blackNoFlag Black `shouldBe` False
        it "black no moves white - TEST: playerWon " $ do
            playerWon blackNoMoves White `shouldBe` True
        it "white no general - TEST: playerWon " $ do
            playerWon whiteNoGeneral Black `shouldBe` True


allSquares = [(Pos 'a' 9), (Pos 'b' 9), (Pos 'c' 9), (Pos 'd' 9), (Pos 'e' 9), (Pos 'f' 9), (Pos 'g' 9), (Pos 'h' 9), (Pos 'i' 9), (Pos 'j' 9), 
              (Pos 'a' 8), (Pos 'b' 8), (Pos 'c' 8), (Pos 'd' 8), (Pos 'e' 8), (Pos 'f' 8), (Pos 'g' 8), (Pos 'h' 8), (Pos 'i' 8), (Pos 'j' 8), 
              (Pos 'a' 7), (Pos 'b' 7), (Pos 'c' 7), (Pos 'd' 7), (Pos 'e' 7), (Pos 'f' 7), (Pos 'g' 7), (Pos 'h' 7), (Pos 'i' 7), (Pos 'j' 7), 
              (Pos 'a' 6), (Pos 'b' 6), (Pos 'c' 6), (Pos 'd' 6), (Pos 'e' 6), (Pos 'f' 6), (Pos 'g' 6), (Pos 'h' 6), (Pos 'i' 6), (Pos 'j' 6), 
              (Pos 'a' 5), (Pos 'b' 5), (Pos 'c' 5), (Pos 'd' 5), (Pos 'e' 5), (Pos 'f' 5), (Pos 'g' 5), (Pos 'h' 5), (Pos 'i' 5), (Pos 'j' 5), 
              (Pos 'a' 4), (Pos 'b' 4), (Pos 'c' 4), (Pos 'd' 4), (Pos 'e' 4), (Pos 'f' 4), (Pos 'g' 4), (Pos 'h' 4), (Pos 'i' 4), (Pos 'j' 4), 
              (Pos 'a' 3), (Pos 'b' 3), (Pos 'c' 3), (Pos 'd' 3), (Pos 'e' 3), (Pos 'f' 3), (Pos 'g' 3), (Pos 'h' 3), (Pos 'i' 3), (Pos 'j' 3), 
              (Pos 'a' 2), (Pos 'b' 2), (Pos 'c' 2), (Pos 'd' 2), (Pos 'e' 2), (Pos 'f' 2), (Pos 'g' 2), (Pos 'h' 2), (Pos 'i' 2), (Pos 'j' 2), 
              (Pos 'a' 1), (Pos 'b' 1), (Pos 'c' 1), (Pos 'd' 1), (Pos 'e' 1), (Pos 'f' 1), (Pos 'g' 1), (Pos 'h' 1), (Pos 'i' 1), (Pos 'j' 1), 
              (Pos 'a' 0), (Pos 'b' 0), (Pos 'c' 0), (Pos 'd' 0), (Pos 'e' 0), (Pos 'f' 0), (Pos 'g' 0), (Pos 'h' 0), (Pos 'i' 0), (Pos 'j' 0)]

getAllMovesOnPos :: Board -> Pos -> Player -> [Move]
getAllMovesOnPos board pos player = (generalMoves board player pos) ++ (soldierMoves board player pos) ++ (catapultMoves board player pos)

getAllMovesH :: Board -> [Pos] -> Player -> [Move]
getAllMovesH board [] player = []
getAllMovesH board (x:xs) player = (getAllMovesOnPos board x player) ++ (getAllMovesH board xs player)

getAllMoves :: Board -> Player -> [Move]
getAllMoves board player = getAllMovesH board allSquares player

testListMoves :: Spec
testListMoves = describe "IF Grade listMoves 2FP" $ do
        it "on sampleBoard - TEST: listMoves " $ do
            listMoves sampleBoard Black `sameElements` (getAllMoves sampleBoard Black)
        it "on blackGeneralOnEdge - TEST: listMoves " $ do
            listMoves blackGeneralOnEdge Black `sameElements` (getAllMoves blackGeneralOnEdge Black)
        it "on cannonShots - TEST: listMoves " $ do
            listMoves cannonShots Black `sameElements` (getAllMoves cannonShots Black)
        it "on cannonShots2 - TEST: listMoves " $ do
            listMoves cannonShots2 Black `sameElements` (getAllMoves cannonShots2 Black)