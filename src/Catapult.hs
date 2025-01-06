module Catapult where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char
import Board
import Data.Char
import Data.Maybe
import Debug.Trace
import Data.List (nub)
data Move = Move {start :: Pos, target :: Pos}

instance Show Move where
  show (Move (Pos startC startR) (Pos targetC targetR)) = [startC] ++ show startR ++ "-" ++ [targetC] ++ show targetR

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1)) (Move (Pos sc2 sr2) (Pos tc2 tr2)) =
    sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2

-- #################################################################################################
-- ################## IMPLEMENT flagMoves :: Board -> Player -> [Move]           ###################
-- ################## - 2 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

data Catapult = N | NE | E | SE | S | SW | W | NW deriving Show
data Direction = Straight | Leftway | Rightway

flagMoves :: Board -> Player -> [Move]
flagMoves board player
  | hasFlag board player = []
  | otherwise = possibleFlagMoves board player

--Controls if the flag of the respective player is placed

hasFlag :: Board -> Player -> Bool
hasFlag board player = any ( elem (Flag player)) board


--Generates possible flag moves for the player respectively

possibleFlagMoves :: Board -> Player -> [Move]
possibleFlagMoves board White = allMoves board White 9 ['b','c','d','e','f','g','h','i']
possibleFlagMoves board Black = allMoves board Black 0 ['b','c','d','e','f','g','h','i']

allMoves :: Board -> Player -> Int -> [Char] -> [Move]
allMoves board player row columns =
  [Move (Pos c row) (Pos c row) | c <- columns, isBlank board (Pos c row)]
  --Function to control if a cell is empty
isBlank :: Board -> Pos -> Bool
isBlank board (Pos column1 row1)=
  case getCell board (Pos column1 row1) of
    Empty -> True
    _  -> False
  --Function to get the wanted cell 
getCell :: Board -> Pos -> Cell
--AI was used to correctly enumerate the char section
getCell board (Pos col row)
  | row < 0 || row >= length board = Empty  -- Check if the row index is out of bounds
  | colIdx < 0 || colIdx >= length (head board) = Empty  -- Check if the column index is out of bounds
  | otherwise = (board !! row) !! colIdx
  where
    colIdx = numericalColumn col
    
numericalColumn :: Char -> Int
numericalColumn column = fromEnum column - fromEnum 'a'
-- Column number to original value
numberToChar :: Int -> Char
numberToChar number= toEnum (number + fromEnum 'a')



-- #################################################################################################
-- ################## IMPLEMENT generalMoves :: Board -> Player -> Pos -> [Move] ###################
-- ################## - 4 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

generalMoves :: Board -> Player -> Pos -> [Move]
generalMoves board player (Pos column row)
  -- AI was used to correctly keep the function in bounds
  | not (isGeneral board player (Pos column row)) = []
  | otherwise =
      [Move (Pos column row) (Pos colChar row1) |
         colNum <- [numericalColumn column - 1 .. numericalColumn column
                                                    + 1],
         colNum >= 0,
         colNum <= 9,
         row1 <- [row - 1 .. row + 1],
         row1 >= 0,
         row1 <= 9,
         let colChar = numberToChar colNum,
         isValidMove board (Pos colChar row1)]

-- Control the general
isGeneral :: Board -> Player -> Pos -> Bool
isGeneral board player (Pos col row) =
  case getCell board (Pos col row) of
    General p -> p == player
    _ -> False



-- AI Helper function
isValidMove :: Board -> Pos -> Bool
isValidMove board (Pos col row) = isBlank board (Pos col row)


-- #################################################################################################
-- ################## IMPLEMENT soldierMoves :: Board -> Player -> Pos -> [Move] ###################
-- ################## - 4 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

soldierMoves :: Board -> Player -> Pos -> [Move]
soldierMoves board player (Pos col row)
  | not (isSoldier board player (Pos col row))=[]
  | otherwise=nub (
    normalMoves board player (Pos col row) ++
    attackMoves board player (Pos col row) ++
    retreatMoves board player (Pos col row))
--Control the soldier
isSoldier :: Board -> Player -> Pos -> Bool
isSoldier board player (Pos col row)=
  case getCell board (Pos col row) of
    Soldier p -> p == player
    _ -> False
--Classic moves
normalMoves :: Board -> Player -> Pos -> [Move]
normalMoves board player (Pos col row)=
  let direction = if player == White then 1 else -1
      colLeft = numericalColumn col - 1
      colRight = numericalColumn col + 1
  in [Move (Pos col row) (Pos col (row - direction))
      | isValidMove board (Pos col (row - direction)), (row - direction) >= 0, (row - direction) <= 9] ++ -- Forward Move
     [Move (Pos col row) (Pos (numberToChar colLeft) row)
      | colLeft >= 0, colLeft <= 9, isValidMove board (Pos (numberToChar colLeft) row)] ++ -- Left Move
     [Move (Pos col row) (Pos (numberToChar colRight) row)
      | colRight >= 0, colRight <= 9, isValidMove board (Pos (numberToChar colRight) row)] -- Right Move
--Takeout moves
attackMoves :: Board -> Player -> Pos -> [Move]
attackMoves board player (Pos col row)=
  let direction= if player==White then 1 else -1
  in [Move (Pos col row) (Pos colChar row1)|colNum <- [numericalColumn col - 1 .. numericalColumn col
                                                    + 1],
         colNum >= 0,
         colNum <= 9,row1 <- [row - direction],
         row1 >= 0,
         row1 <= 9,
         let colChar = numberToChar colNum,
         isEnemy board player (Pos colChar row1)]
--Flee Moves
retreatMoves :: Board -> Player -> Pos -> [Move]
retreatMoves board player (Pos col row)
  | not (isUnderThreat board player (Pos col row))=[]
  | otherwise= retreatMovesBackwards board player (Pos col row)
--Is the soldier under any threat
isUnderThreat:: Board -> Player -> Pos -> Bool
isUnderThreat board player (Pos col row)=
  let direction = if player == White then 1 else -1
      forward = Pos col (row - direction)
      diagonalLeft = Pos (numberToChar (numericalColumn col-1)) (row - direction)
      diagonalRight = Pos (numberToChar (numericalColumn col+1)) (row - direction)
  in any (isEnemy board player) [forward, diagonalLeft, diagonalRight]

retreatMovesBackwards:: Board -> Player -> Pos -> [Move]
retreatMovesBackwards board player (Pos col row)=
  let direction= if player==White then 1 else -1
  in [Move (Pos col row) (Pos col (row + direction*2))
      | isBlank board (Pos col (row + direction*2)),
      (row+direction*2)>=0,
      (row+direction*2)<=9,
      isPathClear board player (Pos col row) Straight] ++ --Backwards 2 steps
      [Move (Pos col row) (Pos (numberToChar (numericalColumn col-2)) (row + direction*2))|
         numericalColumn col-2 >= 0,
         numericalColumn col-2 <= 9,
         isPathClear board player (Pos col row) Leftway]++ --Left diagonal retreat 2 steps
      [Move (Pos col row) (Pos (numberToChar (numericalColumn col+2)) (row + direction*2))|
         numericalColumn col+2 >= 0,
         numericalColumn col+2 <= 9,
         isPathClear board player (Pos col row) Rightway]   --Right diagonal retreat 2 steps
--AI was used to implement this function properly
isPathClear :: Board -> Player -> Pos -> Direction -> Bool
isPathClear board player (Pos col row) direction =
  case direction of
    Straight ->
      all (isValidMove board) [Pos col r | r <- [row + offset, row + offset * 2]] -- Controlling the 2 steps behind
    Leftway -> --Controlling 2 steps left diagonal
      let colLeft = numericalColumn col - 1
          colLeftTwo = numericalColumn col - 2
      in colLeft >= 0 && colLeftTwo >= 0 &&
         all (isValidMove board)
         [Pos (numberToChar colLeft) (row + offset),
          Pos (numberToChar colLeftTwo) (row + offset * 2)]
    Rightway -> --Controlling 2 steps right diagonal
      let colRight = numericalColumn col + 1
          colRightTwo = numericalColumn col + 2
      in colRight <= 9 && colRightTwo <= 9 &&
         all (isValidMove board)
         [Pos (numberToChar colRight) (row + offset),
          Pos (numberToChar colRightTwo) (row + offset * 2)]
  where
    offset = if player == White then 1 else -1
--Checks if a cell is enemy or not
isEnemy :: Board -> Player -> Pos -> Bool
isEnemy board player pos =
  case getCell board pos of
    Soldier p -> p /= player
    General p -> p /= player
    Flag p -> p /= player
    _ -> False
-- #################################################################################################
-- ################## IMPLEMENT catapultMoves :: Board -> Player -> Pos -> [Move]  ###################
-- ################## - 4 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

catapultMoves :: Board -> Player -> Pos -> [Move]
catapultMoves board player (Pos col row)=[]



-- #################################################################################################
-- ################## IMPLEMENT playerWon :: Board -> Maybe Player               ###################
-- ################## - 2 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

playerWon :: Board -> Player -> Bool
playerWon board player=False
  {-let opponent = if player == White then Black else White
      --Testing all of the possibilities
      hasOpponentFlag = any (elem (Flag opponent)) board
      hasOpponentGeneral = any (elem (General opponent)) board
      opponentHasMoves = not (null (listMoves board opponent))
  in not (hasOpponentFlag && hasOpponentGeneral) || not opponentHasMoves-}

-- #################################################################################################
-- ################## IMPLEMENT listMoves :: Board -> Player -> [Move]           ###################
-- ################## - 2 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

listMoves :: Board -> Player -> [Move]
listMoves board player =[]