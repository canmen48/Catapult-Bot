module Board where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO ANY CHANGES TO package.yaml, e.g.:
--       import Data.Chars
import Data.Char

-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################

data Player = White | Black deriving Show
data Cell = Empty | General Player | Soldier Player | Flag Player deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
data Dir = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) White White = True
  (==) Black Black = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Soldier p1) (Soldier p2) = p1 == p2
  (==) (General p1) (General p2) = p1 == p2
  (==) (Flag p1) (Flag p2) = p1 == p2
  (==) _ _ = False


-- ##############################################################################
-- ################## IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Functional Points                   ###################
-- ################## - 1 Coverage Point                      ###################
-- #############################################################################
--AI was used to implement the splitOn function properly 
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn delim (x:xs)
    | x == delim = [] : rest
    | otherwise  = (x : head rest) : tail rest
  where
    rest = splitOn delim xs
correctRow :: String -> Int -> Bool
correctRow row x=
    let validRow=map parseFENChar row
    in all (>0) validRow && sum validRow==x
--AI was used to create a function that will read the integers in a string as integers and to count letters in the parts
parseFENChar :: Char -> Int
parseFENChar c
  | isDigit c = read [c]
  | c `elem` "bBwWgG" = 1
  | otherwise = 0
handleEmpty :: String -> String
handleEmpty "" = "0"  -- 
handleEmpty row = row
validateFEN :: String -> Bool
validateFEN str
  | null str = False
  | otherwise =
      let parts = splitOn '/' str
      in length parts == 10 && all ((`correctRow` 10) . handleEmpty) parts





-- ##############################################################################
-- ################## IMPLEMENT buildBoard :: String -> Board ###################
-- ################## - 2 Functional Points                   ###################
-- ################## - 1 Coverage Point                      ###################
-- ##############################################################################

buildBoard :: String -> Board
buildBoard _ = []