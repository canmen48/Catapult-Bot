module Catapult where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char
import Board
import Data.Char
import Data.Maybe

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
getCell board (Pos col row)= (board !! row) !! (numericalColumn col)




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
      [Move (Pos column row) (Pos colChar row1) 
      | colNum <- [numericalColumn column - 1 .. numericalColumn column + 1],
        row1 <- [row - 1 .. row + 1],
        colNum >= 0, colNum <= 9, 
        row1 >= 0, row1 <= 9, 
        let colChar = numberToChar colNum,
        isValidMove board (Pos colChar row1)]

-- Control the general
isGeneral :: Board -> Player -> Pos -> Bool
isGeneral board player (Pos col row) =
  case getCell board (Pos col row) of
    General p -> p == player
    _ -> False

-- Original value to column number
numericalColumn :: Char -> Int
numericalColumn column = fromEnum column - fromEnum 'a'

-- Column number to original value
numberToChar :: Int -> Char
numberToChar number
  | number < 0 || number > 9 = error "Out of Bounds"
  | otherwise = toEnum (number + fromEnum 'a')

-- AI Helper function
isValidMove :: Board -> Pos -> Bool
isValidMove board (Pos col row)
  | col < 'a' || col > 'j' = False 
  | row < 0 || row >= length board = False 
  | otherwise = isBlank board (Pos col row)


-- #################################################################################################
-- ################## IMPLEMENT soldierMoves :: Board -> Player -> Pos -> [Move] ###################
-- ################## - 4 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

soldierMoves :: Board -> Player -> Pos -> [Move]
soldierMoves _ _ _ = [(Move (Pos 'a' 9) (Pos 'a' 9))]


-- #################################################################################################
-- ################## IMPLEMENT catapultMoves :: Board -> Player -> Pos -> [Move]  ###################
-- ################## - 4 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

catapultMoves :: Board -> Player -> Pos -> [Move]
catapultMoves _ _ _ = [(Move (Pos 'a' 9) (Pos 'a' 9))]


-- #################################################################################################
-- ################## IMPLEMENT playerWon :: Board -> Maybe Player               ###################
-- ################## - 2 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

playerWon :: Board -> Player -> Bool
--playerWon _ _ = True
playerWon board player=
  let opponent = if player == White then Black else White
      --Testing all of the possibilites
      hasOpponentFlag = any (elem (Flag opponent)) board
      hasOpponentGeneral = any (elem (General opponent)) board
      opponentHasMoves = not (null (listMoves board opponent))
  in not (hasOpponentFlag && hasOpponentGeneral) || not opponentHasMoves

-- #################################################################################################
-- ################## IMPLEMENT listMoves :: Board -> Player -> [Move]           ###################
-- ################## - 2 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

listMoves :: Board -> Player -> [Move]
listMoves _ _ = [(Move (Pos 'a' 9) (Pos 'a' 9))]

